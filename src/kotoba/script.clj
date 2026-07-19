(ns kotoba.script
  "Checked KIR -> restricted ES module backend. No source reader lives here."
  (:require [clojure.string :as str])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile]
           [com.google.javascript.jscomp CompilerOptions$LanguageMode]
           [com.google.javascript.rhino Node]))

(def artifact-schema "kotoba-js-artifact/v1")
(def supported-kir-formats #{:kotoba.kir/v3 :kotoba.kir/v4})

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

(defn- f64-literal [n]
  (when-not (instance? Double n)
    (fail! "KIR literal is not f64" {:node n}))
  (cond
    (Double/isNaN n) "Number.NaN"
    (= Double/POSITIVE_INFINITY n) "Number.POSITIVE_INFINITY"
    (= Double/NEGATIVE_INFINITY n) "Number.NEGATIVE_INFINITY"
    (= Long/MIN_VALUE (Double/doubleToRawLongBits n)) "-0"
    :else (str (Double/toString n))))

(defn- js-string [value]
  (if (string? value) (pr-str value) "null"))

(defn- js-value-validator [type expression]
  (case type
    :i64 (str "validateI64(" expression ")")
    :string (str "validateString(" expression ")")
    :f64 (str "validateF64(" expression ")")
    (fail! "unsupported KIR value type" {:type type})))

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
      (= op 'string-byte-length) (str "utf8Length(" (a (first args)) ")")
      (= op 'string=?) (str "((" (a (first args)) "===" (a (second args)) ")?1n:0n)")
      (= op 'string-concat) (str "boundedString(" (a (first args)) "+" (a (second args)) ")")
      (= op 'f64-to-bits) (str "f64ToBits(" (a (first args)) ")")
      (= op 'f64-from-bits) (str "f64FromBits(" (a (first args)) ")")
      (contains? functions op)
      (str (js-name op) "(" (str/join "," (map a args)) ")")
      :else (fail! "unsupported KIR operation" {:operation op}))))

(defn emit-expr [form env functions]
  (cond
    (integer? form) (bigint-literal form)
    (instance? Double form) (f64-literal form)
    (string? form) (pr-str form)
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
  (when-not (contains? supported-kir-formats (:format kir))
    (fail! "unsupported or unchecked KIR format" {:format (:format kir)}))
  (let [function-names (mapv :name (:functions kir))
        functions (set function-names)
        entry (:entry kir)
        _ (when (and entry (not (contains? functions entry)))
            (fail! "KIR entry is missing" {:entry entry}))
        exports (or (:exports kir) function-names)
        _ (when-not (every? functions exports)
            (fail! "KIR export is missing" {:exports exports}))
        caps (capability-ids kir)
        typed-values? (= :kotoba.kir/v4 (:format kir))
        function-source
        (str/join "\n"
                  (map (fn [{:keys [name params param-types result body]}]
                         (let [env (into {} (map (juxt identity js-name) params))
                               parameter-types (or param-types (vec (repeat (count params) :i64)))
                               validation-source
                               (when typed-values?
                                 (apply str
                                        (map (fn [parameter type]
                                               (str (js-value-validator type (js-name parameter)) ";"))
                                             params parameter-types)))
                               expression (emit-expr body env functions)]
                           (str "function " (js-name name) "("
                                (str/join "," (map js-name params)) "){charge();"
                                validation-source
                                (if typed-values?
                                  (str "const k$result=" expression ";return "
                                       (js-value-validator (or result :i64) "k$result") ";}")
                                  (str "return " expression ";}")))))
                       (:functions kir)))
        source
        (str "export const kotobaArtifact=Object.freeze({schema:'" artifact-schema
             "',kirFormat:'" (name (:format kir)) "',entry:"
             (if entry (str "'" entry "'") "null")
             ",valueProfile:'" (if typed-values? "typed-v1" "i64-v1") "'"
             ",sourceDigest:" (js-string source-digest)
             ",kirDigest:" (js-string kir-digest)
             ",compilerVersion:" (js-string compiler-version)
             ",requiredCapabilities:Object.freeze([" (str/join "," caps) "])});\n"
             "export function instantiateKotoba(grants=Object.freeze({})){\n"
             "const grantIds=Object.keys(grants).map(Number).sort((a,b)=>a-b);"
             "const required=[" (str/join "," caps) "];"
             "if(grantIds.length!==required.length||grantIds.some((v,i)=>v!==required[i]))"
             "throw new Error('capability-grant-mismatch');"
             "const i64=n=>BigInt.asIntN(64,n);let fuel=256;"
             "const validateI64=n=>{if(typeof n!=='bigint'||BigInt.asIntN(64,n)!==n)throw new Error('invalid-i64-value');return n;};"
             "const validateF64=n=>{if(typeof n!=='number')throw new Error('invalid-f64-value');return n;};"
             "const f64Buffer=new ArrayBuffer(8);const f64View=new DataView(f64Buffer);"
             "const f64ToBits=n=>{validateF64(n);if(Number.isNaN(n))return 9221120237041090560n;"
             "f64View.setFloat64(0,n,true);return f64View.getBigInt64(0,true);};"
             "const f64FromBits=n=>{validateI64(n);f64View.setBigInt64(0,n,true);const x=f64View.getFloat64(0,true);"
             "return Number.isNaN(x)?Number.NaN:x;};"
             "const utf8Bytes=s=>{if(typeof s!=='string')throw new Error('invalid-string-value');let n=0;"
             "for(let i=0;i<s.length;i++){const u=s.charCodeAt(i);if(u<=127)n++;else if(u<=2047)n+=2;"
             "else if(u>=55296&&u<=56319){if(i+1>=s.length)throw new Error('invalid-string-value');"
             "const v=s.charCodeAt(++i);if(v<56320||v>57343)throw new Error('invalid-string-value');n+=4;}"
             "else if(u>=56320&&u<=57343)throw new Error('invalid-string-value');else n+=3;}return n;};"
             "const validateString=s=>{if(utf8Bytes(s)>65536)throw new Error('invalid-string-value');return s;};"
             "const boundedString=validateString;const utf8Length=s=>BigInt(utf8Bytes(validateString(s)));"
             "const charge=()=>{fuel--;if(fuel<0)throw new Error('fuel-exhausted');};"
             "const quot=(a,b)=>{if(b===0n)throw new Error('division-by-zero');"
             "if(a===-9223372036854775808n&&b===-1n)throw new Error('signed-division-overflow');return a/b;};"
             "const callCapability=(id,value)=>{const f=grants[id];"
             "if(typeof f!=='function')throw new Error('capability-denied:'+id);return i64(BigInt(f(value)));};\n"
             function-source "\n"
             "return Object.freeze({" (str/join "," (map (fn [f] (str "'" f "':" (js-name f))) exports)) "});\n}\n")]
    (verify-output! source))))
