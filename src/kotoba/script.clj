(ns kotoba.script
  "Checked KIR -> restricted ES module backend. No source reader lives here."
  (:require [clojure.string :as str])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile]
           [com.google.javascript.jscomp CompilerOptions$LanguageMode]
           [com.google.javascript.rhino Node]))

(def artifact-schema "kotoba-js-artifact/v1")
(def supported-kir-formats #{:kotoba.kir/v3 :kotoba.kir/v4})
(def ^:private value-types #{:i64 :string :keyword})
(def ^:private max-string-literal-bytes 4096)
(def ^:private max-string-value-bytes 65536)
(def ^:private max-keyword-bytes 512)

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

(defn- utf8-byte-count
  "Count UTF-8 bytes while rejecting unpaired UTF-16 surrogates. Java's
  default encoder replaces malformed input, so validation must be explicit."
  [^String value]
  (loop [index 0 total 0]
    (if (= index (count value))
      total
      (let [unit (int (.charAt value index))]
        (cond
          (<= unit 0x7f) (recur (inc index) (inc total))
          (<= unit 0x7ff) (recur (inc index) (+ total 2))
          (<= 0xd800 unit 0xdbff)
          (if (< (inc index) (count value))
            (let [next-unit (int (.charAt value (inc index)))]
              (if (<= 0xdc00 next-unit 0xdfff)
                (recur (+ index 2) (+ total 4))
                (fail! "KIR string contains an unpaired high surrogate" {:index index})))
            (fail! "KIR string contains an unpaired high surrogate" {:index index}))
          (<= 0xdc00 unit 0xdfff)
          (fail! "KIR string contains an unpaired low surrogate" {:index index})
          :else (recur (inc index) (+ total 3)))))))

(defn- keyword-text [value]
  (when-not (keyword? value)
    (fail! "KIR keyword literal is invalid" {:node value}))
  (let [text (str value)
        bytes (utf8-byte-count text)]
    (when (> bytes max-keyword-bytes)
      (fail! "KIR keyword exceeds byte limit" {:bytes bytes :limit max-keyword-bytes}))
    text))

(defn- typed-signatures [kir]
  (let [typed? (= :kotoba.kir/v4 (:format kir))]
    (into {}
          (map (fn [{:keys [name params param-types result]}]
                 (let [types (if typed? param-types (vec (repeat (count params) :i64)))
                       result-type (if typed? result :i64)]
                   (when-not (and (symbol? name) (nil? (namespace name))
                                  (vector? params) (vector? types)
                                  (every? #(and (symbol? %) (nil? (namespace %))) params)
                                  (= (count params) (count (distinct params)))
                                  (= (count params) (count types))
                                  (every? value-types types)
                                  (contains? value-types result-type))
                     (fail! "KIR function type signature is invalid" {:function name}))
                   [name {:params params :param-types types :result result-type}])))
          (:functions kir))))

(declare infer-type)

(defn- require-type! [actual expected form]
  (when-not (= expected actual)
    (fail! "KIR expression type mismatch"
           {:expected expected :actual actual :node form})))

(defn- require-arity! [op args expected]
  (when-not (cond
              (= expected :positive) (pos? (count args))
              (set? expected) (contains? expected (count args))
              :else (= expected (count args)))
    (fail! "KIR operation arity mismatch"
           {:operation op :expected expected :actual (count args)})))

(defn- infer-call-type [op args env signatures]
  (let [types (mapv #(infer-type % env signatures) args)]
    (cond
      (contains? '#{+ - * quot bit-xor bit-and} op)
      (do (require-arity! op args (if (contains? '#{quot bit-xor bit-and} op)
                                    2 :positive))
          (doseq [[arg type] (map vector args types)] (require-type! type :i64 arg)) :i64)

      (= op '=)
      (do (require-arity! op args 2)
          (when-not (= (first types) (second types))
            (fail! "KIR equality operands have different types" {:types types :node args}))
          (when-not (contains? #{:i64 :keyword} (first types))
            (fail! "KIR equality type is unsupported" {:type (first types)}))
          :i64)

      (contains? '#{< > <= >=} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :i64 arg)) :i64)

      (= op 'pair)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :i64 arg)) :i64)
      (contains? '#{pair-first pair-second} op)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :i64)
      (= op 'cap-call)
      (do (require-arity! op args 2) (require-type! (second types) :i64 (second args)) :i64)

      (= op 'string-byte-length)
      (do (require-arity! op args 1) (require-type! (first types) :string (first args)) :i64)
      (= op 'string=?)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :string arg)) :i64)
      (= op 'string-concat)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :string arg)) :string)

      (contains? signatures op)
      (let [{expected :param-types result :result} (get signatures op)]
        (when-not (= (count expected) (count types))
          (fail! "KIR function call arity mismatch" {:function op}))
        (doseq [[arg actual wanted] (map vector args types expected)]
          (require-type! actual wanted arg))
        result)

      :else (fail! "unsupported KIR operation" {:operation op}))))

(defn- infer-type [form env signatures]
  (cond
    (integer? form) :i64
    (string? form)
    (let [bytes (utf8-byte-count form)]
      (when (> bytes max-string-literal-bytes)
        (fail! "KIR string literal exceeds byte limit"
               {:bytes bytes :limit max-string-literal-bytes}))
      :string)
    (keyword? form) (do (keyword-text form) :keyword)
    (symbol? form) (or (get env form)
                       (fail! "unbound KIR symbol" {:symbol form}))
    (seq? form)
    (let [[op & args] form]
      (case op
        let (let [[bindings body] args]
              (when-not (and (= 2 (count args)) (vector? bindings) (even? (count bindings)))
                (fail! "KIR let shape is invalid" {:node form}))
              (loop [pairs (partition 2 bindings) current env]
                (if-let [[name value] (first pairs)]
                  (recur (next pairs) (assoc current name (infer-type value current signatures)))
                  (infer-type body current signatures))))
        if (let [[test then else] args
                 _ (require-arity! op args 3)
                 test-type (infer-type test env signatures)
                 then-type (infer-type then env signatures)
                 else-type (infer-type else env signatures)]
             (require-type! test-type :i64 test)
             (when-not (= then-type else-type)
               (fail! "KIR if branches have different types"
                      {:then then-type :else else-type :node form}))
             then-type)
        do (do (when (empty? args) (fail! "KIR do requires a value" {:node form}))
               (last (mapv #(infer-type % env signatures) args)))
        (infer-call-type op args env signatures)))
    :else (fail! "unsupported KIR node" {:node form})))

(defn- validate-types! [kir]
  (let [signatures (typed-signatures kir)
        nodes (mapcat #(tree-seq coll? seq (:body %)) (:functions kir))
        literal-bytes (reduce + 0
                              (map utf8-byte-count
                                   (filter string? nodes)))
        keyword-bytes (reduce + 0 (map (comp utf8-byte-count keyword-text)
                                       (filter keyword? nodes)))]
    (when (> literal-bytes max-string-value-bytes)
      (fail! "KIR module string literals exceed byte limit"
             {:bytes literal-bytes :limit max-string-value-bytes}))
    (when (> keyword-bytes max-string-value-bytes)
      (fail! "KIR module keyword literals exceed byte limit"
             {:bytes keyword-bytes :limit max-string-value-bytes}))
    (doseq [{:keys [name params param-types result body]} (:functions kir)]
      (let [types (if (= :kotoba.kir/v4 (:format kir))
                    param-types (vec (repeat (count params) :i64)))
            actual (infer-type body (zipmap params types) signatures)
            expected (if (= :kotoba.kir/v4 (:format kir)) result :i64)]
        (require-type! actual expected name)))
    signatures))

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
      (= op 'string-byte-length) (str "BigInt(utf8Bytes(" (a (first args)) "))")
      (= op 'string=?) (str "((" (a (first args)) "===" (a (second args)) ")?1n:0n)")
      (= op 'string-concat) (str "assertString(" (a (first args)) "+" (a (second args)) ")")
      (contains? functions op)
      (str (js-name op) "(" (str/join "," (map a args)) ")")
      :else (fail! "unsupported KIR operation" {:operation op}))))

(defn emit-expr [form env functions]
  (cond
    (integer? form) (bigint-literal form)
    (string? form) (js-string form)
    (keyword? form) (js-string (keyword-text form))
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

(defn- sha256? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- module-seal-source [graph-digest source-digests]
  (when (not= (some? graph-digest) (some? source-digests))
    (fail! "module graph digest and source digests must be supplied together" {}))
  (when graph-digest
    (when-not (sha256? graph-digest)
      (fail! "module graph digest is not canonical SHA-256" {}))
    (when-not (and (map? source-digests) (pos? (count source-digests))
                   (<= (count source-digests) 256)
                   (every? symbol? (keys source-digests))
                   (every? sha256? (vals source-digests)))
      (fail! "module source digests are invalid" {}))
    (str ",moduleGraphDigest:" (js-string graph-digest)
         ",moduleSourceDigests:Object.freeze({"
         (str/join "," (map (fn [[module digest]]
                              (str (js-string (str module)) ":" (js-string digest)))
                            (sort-by (comp str key) source-digests)))
         "})")))

(defn- supply-chain-seal-source
  [package-lock-digest trust-policy-digest package-receipt-digest]
  (let [digests [package-lock-digest trust-policy-digest package-receipt-digest]
        supplied (count (filter some? digests))]
    (when-not (or (zero? supplied) (= 3 supplied))
      (fail! "package lock, trust policy, and package receipt digests must be supplied together" {}))
    (when (pos? supplied)
      (when-not (every? sha256? digests)
        (fail! "supply-chain digest is not canonical SHA-256" {}))
      (str ",packageLockDigest:" (js-string package-lock-digest)
           ",trustPolicyDigest:" (js-string trust-policy-digest)
           ",packageReceiptDigest:" (js-string package-receipt-digest)))))

(defn emit
  "Emit a restricted ESM string from checked `:kotoba.kir/v3` data."
  ([kir] (emit kir {}))
  ([kir {:keys [source-digest kir-digest compiler-version
                module-graph-digest module-source-digests
                package-lock-digest trust-policy-digest
                package-receipt-digest]}]
  (when-not (contains? supported-kir-formats (:format kir))
    (fail! "unsupported or unchecked KIR format" {:format (:format kir)}))
  (let [function-names (mapv :name (:functions kir))
        functions (set function-names)
        _ (when-not (= (count function-names) (count functions))
            (fail! "KIR function names are not unique" {:functions function-names}))
        signatures (validate-types! kir)
        exports (vec (or (:exports kir) function-names))
        _ (when-not (and (= (count exports) (count (distinct exports)))
                         (seq exports)
                         (every? functions exports))
            (fail! "KIR exports are invalid" {:exports exports}))
        entry (:entry kir)
        _ (when (and entry (not (contains? functions entry)))
            (fail! "KIR entry is missing" {:entry entry}))
        caps (capability-ids kir)
        function-source
        (str/join "\n"
                  (map (fn [{:keys [name params body]}]
                         (let [env (into {} (map (juxt identity js-name) params))
                               {:keys [param-types result]} (get signatures name)
                               guards (apply str
                                             (map (fn [param type]
                                                    (str (js-name param) "="
                                                         (case type
                                                           :string "assertString("
                                                           :keyword "assertKeyword("
                                                           "assertI64(")
                                                         (js-name param) ");"))
                                                  params param-types))]
                           (str "function " (js-name name) "("
                                (str/join "," (map js-name params)) "){charge();" guards "return "
                                (case result
                                  :string "assertString("
                                  :keyword "assertKeyword("
                                  "assertI64(")
                                (emit-expr body env functions) ");}")))
                       (:functions kir)))
        source
        (str "export const kotobaArtifact=Object.freeze({schema:'" artifact-schema
             "',kirFormat:'" (name (:format kir)) "',entry:" (js-string (some-> entry str))
             ",valueProfile:'" (if (= :kotoba.kir/v4 (:format kir)) "typed-v1" "i64-v1") "'"
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",stringLimits:Object.freeze({literalBytes:" max-string-literal-bytes
                    ",moduleLiteralBytes:" max-string-value-bytes
                    ",valueBytes:" max-string-value-bytes "})"
                    ",keywordLimits:Object.freeze({valueBytes:" max-keyword-bytes "})"))
             ",sourceDigest:" (js-string source-digest)
             ",kirDigest:" (js-string kir-digest)
             ",compilerVersion:" (js-string compiler-version)
             (module-seal-source module-graph-digest module-source-digests)
             (supply-chain-seal-source package-lock-digest trust-policy-digest
                                       package-receipt-digest)
             ",requiredCapabilities:Object.freeze([" (str/join "," caps) "])});\n"
             "export function instantiateKotoba(grants=Object.freeze({})){\n"
             "const grantIds=Object.keys(grants).map(Number).sort((a,b)=>a-b);"
             "const required=[" (str/join "," caps) "];"
             "if(grantIds.length!==required.length||grantIds.some((v,i)=>v!==required[i]))"
             "throw new Error('capability-grant-mismatch');"
             "const i64=n=>BigInt.asIntN(64,n);"
             "const assertI64=v=>{if(typeof v!=='bigint'||i64(v)!==v)throw new Error('invalid-i64');return v;};"
             "const utf8Bytes=s=>{let n=0;for(let i=0;i<s.length;i++){const u=s.charCodeAt(i);"
             "if(u<=127)n++;else if(u<=2047)n+=2;else if(u>=55296&&u<=56319){"
             "if(i+1>=s.length)throw new Error('invalid-utf16');const l=s.charCodeAt(++i);"
             "if(l<56320||l>57343)throw new Error('invalid-utf16');n+=4;}"
             "else if(u>=56320&&u<=57343)throw new Error('invalid-utf16');else n+=3;}return n;};"
             "const assertString=v=>{if(typeof v!=='string')throw new Error('invalid-string');"
             "if(utf8Bytes(v)>" max-string-value-bytes ")throw new Error('string-too-large');return v;};"
             "const assertKeyword=v=>{if(typeof v!=='string'||v.length<2||v[0]!==':'||"
             "v.includes('::')||/\\s|[\\[\\]{}()\"',;@`~^\\\\]/u.test(v))"
             "throw new Error('invalid-keyword');if(utf8Bytes(v)>" max-keyword-bytes
             ")throw new Error('keyword-too-large');return v;};"
             "let fuel=256;"
             "const charge=()=>{fuel--;if(fuel<0)throw new Error('fuel-exhausted');};"
             "const quot=(a,b)=>{if(b===0n)throw new Error('division-by-zero');"
             "if(a===-9223372036854775808n&&b===-1n)throw new Error('signed-division-overflow');return a/b;};"
             "const callCapability=(id,value)=>{const f=grants[id];"
             "if(typeof f!=='function')throw new Error('capability-denied:'+id);return i64(BigInt(f(value)));};\n"
             function-source "\n"
             "return Object.freeze({" (str/join "," (map (fn [f] (str "'" f "':" (js-name f))) exports)) "});\n}\n")]
    (verify-output! source))))
