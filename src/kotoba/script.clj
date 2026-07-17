(ns kotoba.script
  "Checked KIR -> restricted ES module backend. No source reader lives here."
  (:require [clojure.string :as str])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile]
           [com.google.javascript.jscomp CompilerOptions$LanguageMode]
           [com.google.javascript.rhino Node]))

(def artifact-schema "kotoba-js-artifact/v1")
(def supported-kir-format :kotoba.kir/v3)

(def ^:private forbidden-output
  [#"\beval\s*\(" #"\bFunction\s*\(" #"\bglobalThis\b" #"\bwindow\b"
   #"\bdocument\b" #"\bprocess\b" #"\brequire\s*\(" #"\bimport\s*\("
   #"__proto__" #"\.constructor\b"])

(def ^:private forbidden-global-names
  #{"eval" "Function" "globalThis" "window" "document" "self" "process" "require"})

(def ^:private forbidden-properties #{"__proto__" "prototype" "constructor"})

(defn- fail! [message data]
  (throw (ex-info message (assoc data :phase :kotoba-script))))

(defn- js-name [x]
  (let [s (str x)]
    (str "k$"
         (apply str
                (mapcat (fn [ch]
                          (if (or (Character/isLetterOrDigit ^char ch) (= ch \_))
                            [(str ch)]
                            [(str "$" (format "%04x" (int ch)))]))
                        s)))))

(defn- bigint-literal [n]
  (when-not (integer? n)
    (fail! "KIR literal is not an integer" {:node n}))
  (str n "n"))

(defn- js-string [value]
  (if (string? value) (pr-str value) "null"))

(declare emit-expr)

(defn- emit-call [op args env functions]
  (let [a #(emit-expr % env functions)]
    (cond
      (= op '+) (str "i64(" (str/join " + " (map a args)) ")")
      (= op '-) (if (= 1 (count args))
                  (str "i64(-" (a (first args)) ")")
                  (str "i64(" (str/join " - " (map a args)) ")"))
      (= op '*) (str "i64(" (str/join " * " (map a args)) ")")
      (= op 'quot) (str "quot(" (a (first args)) "," (a (second args)) ")")
      (= op 'bit-xor) (str "i64(" (str/join " ^ " (map a args)) ")")
      (= op 'bit-and) (str "i64(" (str/join " & " (map a args)) ")")
      (contains? '#{= < > <= >=} op)
      (let [js-op (case op = "===" < "<" > ">" <= "<=" >= ">=")]
        (str "((" (str/join (str " " js-op " ") (map a args)) ")?1n:0n)"))
      (= op 'pair) (str "Object.freeze([" (a (first args)) "," (a (second args)) "])")
      (= op 'pair-first) (str (a (first args)) "[0]")
      (= op 'pair-second) (str (a (first args)) "[1]")
      (= op 'cap-call) (str "callCapability(" (first args) "," (a (second args)) ")")
      (contains? functions op)
      (str (js-name op) "(" (str/join "," (map a args)) ")")
      :else (fail! "unsupported KIR operation" {:operation op}))))

(defn emit-expr [form env functions]
  (cond
    (integer? form) (bigint-literal form)
    (symbol? form) (or (get env form)
                       (fail! "unbound KIR symbol" {:symbol form}))
    (seq? form)
    (let [[op & args] form]
      (case op
        let (let [[bindings body] args
                  pairs (partition 2 bindings)]
              (loop [remaining pairs env env bindings-js []]
                (if-let [[name value] (first remaining)]
                  (let [n (js-name name)]
                    (recur (next remaining) (assoc env name n)
                           (conj bindings-js (str "const " n "="
                                                  (emit-expr value env functions) ";"))))
                  (str "(()=>{" (apply str bindings-js) "return "
                       (emit-expr body env functions) ";})()"))))
        if (let [[test then else] args]
             (str "((" (emit-expr test env functions) ")===0n?"
                  (emit-expr else env functions) ":"
                  (emit-expr then env functions) ")"))
        (emit-call op args env functions)))
    :else (fail! "unsupported KIR node" {:node form})))

(defn- ast-nodes [^Node root]
  (tree-seq #(some? (.getFirstChild ^Node %))
            #(iterator-seq (.iterator (.children ^Node %)))
            root))

(defn- ast-problem [^Node node]
  (let [token (str (.getToken node))
        qname (when (.isGetProp node) (.getQualifiedName node))
        property (cond
                   qname (last (str/split qname #"\\."))
                   (and (.isGetElem node)
                        (some-> node .getLastChild .isStringLit))
                   (some-> node .getLastChild .getString))]
    (cond
      (and (.isName node) (contains? forbidden-global-names (.getString node)))
      {:kind :ambient-global :name (.getString node)}

      (contains? forbidden-properties property)
      {:kind :forbidden-property :property property}

      (or (= token "DYNAMIC_IMPORT") (.isImport node) (.isImportMeta node))
      {:kind :dynamic-import}

      :else nil)))

(defn verify-output!
  "Parse generated JavaScript and fail closed on ambient-authority AST nodes.
  Regexes remain only as defense-in-depth for spellings rejected before parse."
  [source]
  (when-not (string? source)
    (fail! "generated output is not text" {}))
  (when-let [pattern (some #(when (re-find % source) %) forbidden-output)]
    (fail! "generated JavaScript violates restricted subset" {:pattern (str pattern)}))
  (let [compiler (com.google.javascript.jscomp.Compiler.)
        options (doto (CompilerOptions.)
                  (.setLanguageIn CompilerOptions$LanguageMode/ECMASCRIPT_NEXT)
                  (.setLanguageOut CompilerOptions$LanguageMode/ECMASCRIPT_NEXT))
        result (.compile compiler
                         (SourceFile/fromCode "externs.js" "")
                         (SourceFile/fromCode "kotoba-generated.mjs" source)
                         options)]
    (when-not (.-success result)
      (fail! "generated JavaScript failed AST parse"
             {:errors (mapv str (.-errors result))}))
    ;; Compiler root is ROOT(EXTERNS_ROOT, JS_ROOT); inspect only authored JS.
    (when-let [problem (some ast-problem
                             (ast-nodes (.getLastChild (.getRoot compiler))))]
      (fail! "generated JavaScript AST violates restricted subset" problem)))
  source)

(defn- capability-ids [kir]
  (->> (:effects kir)
       (keep (fn [effect]
               (when (and (vector? effect) (= :cap/call (first effect))
                          (integer? (second effect)))
                 (second effect))))
       sort vec))

(defn emit
  "Emit a restricted ESM string from checked `:kotoba.kir/v3` data."
  ([kir] (emit kir {}))
  ([kir {:keys [source-digest kir-digest compiler-version]}]
  (when-not (= supported-kir-format (:format kir))
    (fail! "unsupported or unchecked KIR format" {:format (:format kir)}))
  (let [function-names (mapv :name (:functions kir))
        functions (set function-names)
        entry (:entry kir)
        _ (when-not (contains? functions entry)
            (fail! "KIR entry is missing" {:entry entry}))
        caps (capability-ids kir)
        function-source
        (str/join "\n"
                  (map (fn [{:keys [name params body]}]
                         (let [env (into {} (map (juxt identity js-name) params))]
                           (str "function " (js-name name) "("
                                (str/join "," (map js-name params)) "){charge();return "
                                (emit-expr body env functions) ";}")))
                       (:functions kir)))
        source
        (str "export const kotobaArtifact=Object.freeze({schema:'" artifact-schema
             "',kirFormat:'" (name supported-kir-format) "',entry:'" entry
             "',sourceDigest:" (js-string source-digest)
             ",kirDigest:" (js-string kir-digest)
             ",compilerVersion:" (js-string compiler-version)
             ",requiredCapabilities:Object.freeze([" (str/join "," caps) "])});\n"
             "export function instantiateKotoba(grants=Object.freeze({})){\n"
             "const grantIds=Object.keys(grants).map(Number).sort((a,b)=>a-b);"
             "const required=[" (str/join "," caps) "];"
             "if(grantIds.length!==required.length||grantIds.some((v,i)=>v!==required[i]))"
             "throw new Error('capability-grant-mismatch');"
             "const i64=n=>BigInt.asIntN(64,n);let fuel=256;"
             "const charge=()=>{fuel--;if(fuel<0)throw new Error('fuel-exhausted');};"
             "const quot=(a,b)=>{if(b===0n)throw new Error('division-by-zero');"
             "if(a===-9223372036854775808n&&b===-1n)throw new Error('signed-division-overflow');return a/b;};"
             "const callCapability=(id,value)=>{const f=grants[id];"
             "if(typeof f!=='function')throw new Error('capability-denied:'+id);return i64(BigInt(f(value)));};\n"
             function-source "\n"
             "return Object.freeze({" (str/join "," (map (fn [f] (str "'" f "':" (js-name f))) function-names)) "});\n}\n")]
    (verify-output! source))))
