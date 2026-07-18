(ns kotoba.script
  "Checked KIR -> restricted ES module backend. No source reader lives here."
  (:require [clojure.string :as str])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile]
           [com.google.javascript.jscomp CompilerOptions$LanguageMode]
           [com.google.javascript.rhino Node]))

(def artifact-schema "kotoba-js-artifact/v1")
(def supported-kir-formats #{:kotoba.kir/v3 :kotoba.kir/v4})
(def ^:private value-types #{:i64 :string :keyword :map :bool :option-i64 :result-i64 :vector-i64})
(def ^:private max-string-literal-bytes 4096)
(def ^:private max-string-value-bytes 65536)
(def ^:private max-keyword-bytes 512)
(def ^:private max-map-entries 128)
(def ^:private max-vector-items 128)
(def ^:private max-type-depth 8)
(def ^:private max-type-nodes 64)
(def ^:private max-variant-cases 32)
(def ^:private max-heterogeneous-vector-items 32)
(def ^:private max-set-items 32)
(def ^:private max-record-fields 32)

(declare fail!)

(defn- validate-value-type!
  ([type] (validate-value-type! type 0 (volatile! 0)))
  ([type depth nodes]
   (vswap! nodes inc)
   (when (> @nodes max-type-nodes)
     (fail! "KIR value type exceeds node limit" {:limit max-type-nodes}))
   (when (> depth max-type-depth)
     (fail! "KIR value type exceeds depth limit" {:limit max-type-depth}))
   (cond
     (contains? value-types type) type
     (and (vector? type) (= 3 (count type)) (= :result (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         (validate-value-type! (nth type 2) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :option (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :vector (first type)))
     (let [item-types (second type)]
       (when-not (and (vector? item-types)
                      (<= (count item-types) max-heterogeneous-vector-items))
         (fail! "heterogeneous vector types must be a bounded vector"
                {:type type :limit max-heterogeneous-vector-items}))
       (vswap! nodes inc)
       (when (> @nodes max-type-nodes)
         (fail! "KIR value type exceeds node limit" {:limit max-type-nodes}))
       (doseq [item-type item-types]
         (validate-value-type! item-type (inc depth) nodes))
       type)
     (and (vector? type) (= 2 (count type)) (= :set (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 3 (count type)) (= :record (first type)))
     (let [[_ type-id fields] type]
       (when-not (and (keyword? type-id) (namespace type-id))
         (fail! "record type id must be a qualified keyword" {:type type}))
       (when-not (and (vector? fields) (seq fields) (<= (count fields) max-record-fields)
                      (every? #(and (vector? %) (= 2 (count %)) (keyword? (first %))) fields)
                      (= (count fields) (count (distinct (map first fields)))))
         (fail! "record fields must be a non-empty unique bounded vector" {:type type}))
       (vswap! nodes + (+ 2 (* 2 (count fields))))
       (when (> @nodes max-type-nodes)
         (fail! "KIR value type exceeds node limit" {:limit max-type-nodes}))
       (doseq [[_ field-type] fields]
         (validate-value-type! field-type (inc depth) nodes))
       type)
     (and (vector? type) (= 3 (count type)) (= :variant (first type)))
     (let [[_ type-id cases] type]
       (when-not (and (keyword? type-id) (namespace type-id))
         (fail! "variant type id must be a qualified keyword" {:type type}))
       (when-not (and (vector? cases) (seq cases) (<= (count cases) max-variant-cases)
                      (every? #(and (vector? %) (= 2 (count %)) (keyword? (first %))) cases)
                      (= (count cases) (count (distinct (map first cases)))))
         (fail! "variant cases must be a non-empty unique bounded vector" {:type type}))
       (vswap! nodes + (+ 2 (* 2 (count cases))))
       (when (> @nodes max-type-nodes)
         (fail! "KIR value type exceeds node limit" {:limit max-type-nodes}))
       (doseq [[_ payload-type] cases]
         (validate-value-type! payload-type (inc depth) nodes))
       type)
     :else (fail! "KIR value type is outside the safe profile" {:type type}))))

(defn- parametric-result-type? [type]
  (and (vector? type) (= 3 (count type)) (= :result (first type))))

(defn- variant-type? [type]
  (and (vector? type) (= 3 (count type)) (= :variant (first type))))

(defn- generic-option-type? [type]
  (and (vector? type) (= 2 (count type)) (= :option (first type))))

(defn- heterogeneous-vector-type? [type]
  (and (vector? type) (= 2 (count type)) (= :vector (first type))))

(defn- typed-set-type? [type]
  (and (vector? type) (= 2 (count type)) (= :set (first type))))

(defn- record-type? [type]
  (and (vector? type) (= 3 (count type)) (= :record (first type))))

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

(declare validate-value-type! parametric-result-type? variant-type? generic-option-type?
         heterogeneous-vector-type? typed-set-type? record-type?)

(defn- type-js [type]
  (validate-value-type! type)
  (cond
    (keyword? type) (pr-str (name type))
    (parametric-result-type? type)
    (str "Object.freeze(['result'," (type-js (second type)) ","
         (type-js (nth type 2)) "])" )
    (generic-option-type? type)
    (str "Object.freeze(['option'," (type-js (second type)) "])" )
    (heterogeneous-vector-type? type)
    (str "Object.freeze(['vector',Object.freeze(["
         (str/join "," (map type-js (second type))) "])])")
    (typed-set-type? type)
    (str "Object.freeze(['set'," (type-js (second type)) "])")
    (record-type? type)
    (str "Object.freeze(['record'," (pr-str (str (second type))) ",Object.freeze(["
         (str/join "," (map (fn [[field field-type]]
                              (str "Object.freeze([" (pr-str (str field)) ","
                                   (type-js field-type) "] )"))
                            (nth type 2))) "])])")
    :else
    (str "Object.freeze(['variant'," (pr-str (str (second type))) ",Object.freeze(["
         (str/join "," (map (fn [[tag payload-type]]
                              (str "Object.freeze([" (pr-str (str tag)) ","
                                   (type-js payload-type) "] )"))
                            (nth type 2))) "])])")))

(defn- guard-expr [type expression]
  (if (or (parametric-result-type? type) (variant-type? type) (generic-option-type? type)
          (heterogeneous-vector-type? type) (typed-set-type? type) (record-type? type))
    (str "assertTypedValue(" (type-js type) "," expression ",0,{nodes:0})")
    (str (case type
           :string "assertString("
           :keyword "assertKeyword("
           :map "assertMap("
           :bool "assertBool("
           :option-i64 "assertOptionI64("
           :result-i64 "assertResultI64("
           :vector-i64 "assertVectorI64("
           "assertI64(") expression ")")))

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
                                  (every? #(do (validate-value-type! %) true) types)
                                  (do (validate-value-type! result-type) true))
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
          (when-not (contains? #{:i64 :keyword :bool :option-i64 :result-i64 :vector-i64} (first types))
            (fail! "KIR equality type is unsupported" {:type (first types)}))
          :i64)

      (= op 'bool-not)
      (do (require-arity! op args 1)
          (require-type! (first types) :bool (first args))
          :bool)

      (= op 'option-some)
      (do (require-arity! op args 1)
          (require-type! (first types) :i64 (first args))
          :option-i64)

      (= op 'option-none)
      (do (require-arity! op args 0) :option-i64)

      (= op 'option-some?)
      (do (require-arity! op args 1)
          (require-type! (first types) :option-i64 (first args))
          :bool)

      (= op 'option-value)
      (do (require-arity! op args 2)
          (require-type! (first types) :option-i64 (first args))
          (require-type! (second types) :i64 (second args))
          :i64)

      (contains? '#{result-ok result-err} op)
      (do (require-arity! op args 1)
          (require-type! (first types) :i64 (first args))
          :result-i64)

      (= op 'result-ok?)
      (do (require-arity! op args 1)
          (require-type! (first types) :result-i64 (first args))
          :bool)

      (contains? '#{result-value result-error} op)
      (do (require-arity! op args 2)
          (require-type! (first types) :result-i64 (first args))
          (require-type! (second types) :i64 (second args))
          :i64)

      (= op 'vector-new)
      (do (doseq [[arg type] (map vector args types)] (require-type! type :i64 arg))
          (when (> (count args) max-vector-items)
            (fail! "KIR vector literal exceeds item limit"
                   {:items (count args) :limit max-vector-items}))
          :vector-i64)

      (= op 'vector-count)
      (do (require-arity! op args 1)
          (require-type! (first types) :vector-i64 (first args)) :i64)

      (= op 'vector-get)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :vector-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2)) :i64)

      (= op 'vector-at)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :i64)

      (= op 'vector-drop)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :vector-i64)

      (= op 'vector-assoc)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :vector-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2)) :vector-i64)

      (= op 'vector-conj)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :vector-i64)

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

      (= op 'map-new)
      (do (when (odd? (count args))
            (fail! "KIR map-new requires key/value pairs" {:node args}))
          (doseq [[[key-form value-form] [key-type value-type]]
                  (map vector (partition 2 args) (partition 2 types))]
            (require-type! key-type :keyword key-form)
            (require-type! value-type :i64 value-form))
          (when (> (quot (count args) 2) max-map-entries)
            (fail! "KIR map literal exceeds entry limit" {:entries (quot (count args) 2)}))
          :map)

      (= op 'map-get)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :map (nth args 0))
          (require-type! (nth types 1) :keyword (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2))
          :i64)

      (= op 'map-assoc)
      (do (when-not (and (>= (count args) 3) (odd? (count args)))
            (fail! "KIR map-assoc requires map and key/value pairs" {:node args}))
          (require-type! (first types) :map (first args))
          (doseq [[[key-form value-form] [key-type value-type]]
                  (map vector (partition 2 (rest args)) (partition 2 (rest types)))]
            (require-type! key-type :keyword key-form)
            (require-type! value-type :i64 value-form))
          :map)

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
    (boolean? form) :bool
    (nil? form) :option-i64
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
             (when-not (contains? #{:i64 :bool} test-type)
               (fail! "KIR if test must be bool or legacy i64"
                      {:actual test-type :node test}))
             (when-not (= then-type else-type)
               (fail! "KIR if branches have different types"
                      {:then then-type :else else-type :node form}))
             then-type)
        do (do (when (empty? args) (fail! "KIR do requires a value" {:node form}))
               (last (mapv #(infer-type % env signatures) args)))
        result-ok-of
        (let [[type payload] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result constructor requires [:result ok-type err-type]" {:type type}))
          (require-type! (infer-type payload env signatures) (second type) payload)
          type)
        result-err-of
        (let [[type payload] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result constructor requires [:result ok-type err-type]" {:type type}))
          (require-type! (infer-type payload env signatures) (nth type 2) payload)
          type)
        result-ok?-of
        (let [[type result] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result projection requires [:result ok-type err-type]" {:type type}))
          (require-type! (infer-type result env signatures) type result)
          :bool)
        result-value-of
        (let [[type result fallback] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result projection requires [:result ok-type err-type]" {:type type}))
          (require-type! (infer-type result env signatures) type result)
          (require-type! (infer-type fallback env signatures) (second type) fallback)
          (second type))
        result-error-of
        (let [[type result fallback] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result projection requires [:result ok-type err-type]" {:type type}))
          (require-type! (infer-type result env signatures) type result)
          (require-type! (infer-type fallback env signatures) (nth type 2) fallback)
          (nth type 2))
        result-match-of
        (let [[type result ok-name ok-body err-name err-body] args]
          (require-arity! op args 6)
          (validate-value-type! type)
          (when-not (parametric-result-type? type)
            (fail! "result match requires [:result ok-type err-type]" {:type type}))
          (when-not (and (symbol? ok-name) (nil? (namespace ok-name))
                         (symbol? err-name) (nil? (namespace err-name)))
            (fail! "result match binders must be unqualified symbols" {:node form}))
          (require-type! (infer-type result env signatures) type result)
          (let [ok-type (infer-type ok-body (assoc env ok-name (second type)) signatures)
                err-type (infer-type err-body (assoc env err-name (nth type 2)) signatures)]
            (when-not (= ok-type err-type)
              (fail! "result match branches have different types"
                     {:ok ok-type :err err-type :node form}))
            ok-type))
        variant-new
        (let [[type tag payload] args
              cases (when (variant-type? type) (nth type 2))
              payload-type (some (fn [[case-tag case-type]]
                                   (when (= case-tag tag) case-type)) cases)]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (variant-type? type)
            (fail! "variant constructor requires a variant descriptor" {:type type}))
          (when-not payload-type
            (fail! "variant constructor tag is not declared" {:tag tag :type type}))
          (require-type! (infer-type payload env signatures) payload-type payload)
          type)
        variant-match
        (let [[type value branches] args
              cases (when (variant-type? type) (nth type 2))]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (variant-type? type)
            (fail! "variant match requires a variant descriptor" {:type type}))
          (when-not (and (vector? branches)
                         (= (mapv first cases) (mapv first branches))
                         (every? #(and (vector? %) (= 3 (count %))
                                       (symbol? (second %)) (nil? (namespace (second %))))
                                 branches))
            (fail! "variant match branches must exactly cover declared cases in order"
                   {:type type :branches branches}))
          (require-type! (infer-type value env signatures) type value)
          (let [branch-types
                (mapv (fn [[[tag payload-type] [_ binder body]]]
                        (infer-type body (assoc env binder payload-type) signatures))
                      (map vector cases branches))]
            (when-not (apply = branch-types)
              (fail! "variant match branches have different types" {:types branch-types}))
            (first branch-types)))
        option-some-of
        (let [[type payload] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (generic-option-type? type)
            (fail! "generic option constructor requires [:option payload-type]" {:type type}))
          (require-type! (infer-type payload env signatures) (second type) payload)
          type)
        option-none-of
        (let [[type] args]
          (require-arity! op args 1)
          (validate-value-type! type)
          (when-not (generic-option-type? type)
            (fail! "generic option constructor requires [:option payload-type]" {:type type}))
          type)
        option-some?-of
        (let [[type value] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (generic-option-type? type)
            (fail! "generic option projection requires [:option payload-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          :bool)
        option-value-of
        (let [[type value fallback] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (generic-option-type? type)
            (fail! "generic option projection requires [:option payload-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type fallback env signatures) (second type) fallback)
          (second type))
        option-match
        (let [[type value none-body some-name some-body] args]
          (require-arity! op args 5)
          (validate-value-type! type)
          (when-not (and (generic-option-type? type) (symbol? some-name)
                         (nil? (namespace some-name)))
            (fail! "option match requires option type and unqualified some binder" {:node form}))
          (require-type! (infer-type value env signatures) type value)
          (let [none-type (infer-type none-body env signatures)
                some-type (infer-type some-body (assoc env some-name (second type)) signatures)]
            (when-not (= none-type some-type)
              (fail! "option match branches have different types"
                     {:none none-type :some some-type :node form}))
            none-type))
        hetero-vector-new
        (let [[type & items] args
              item-types (when (heterogeneous-vector-type? type) (second type))]
          (validate-value-type! type)
          (when-not (and (heterogeneous-vector-type? type)
                         (= (count item-types) (count items)))
            (fail! "heterogeneous vector constructor must exactly match its descriptor"
                   {:type type :items (count items)}))
          (doseq [[item item-type] (map vector items item-types)]
            (require-type! (infer-type item env signatures) item-type item))
          type)
        hetero-vector-count
        (let [[type value] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (heterogeneous-vector-type? type)
            (fail! "heterogeneous vector count requires [:vector [item-types...]]"
                   {:type type}))
          (require-type! (infer-type value env signatures) type value)
          :i64)
        hetero-vector-at
        (let [[type value index] args
              item-types (when (heterogeneous-vector-type? type) (second type))]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (and (heterogeneous-vector-type? type) (integer? index)
                         (<= 0 index) (< index (count item-types)))
            (fail! "heterogeneous vector index must be an in-range integer literal"
                   {:type type :index index}))
          (require-type! (infer-type value env signatures) type value)
          (nth item-types index))
        hetero-vector-assoc
        (let [[type value index item] args
              item-types (when (heterogeneous-vector-type? type) (second type))]
          (require-arity! op args 4)
          (validate-value-type! type)
          (when-not (and (heterogeneous-vector-type? type) (integer? index)
                         (<= 0 index) (< index (count item-types)))
            (fail! "heterogeneous vector index must be an in-range integer literal"
                   {:type type :index index}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type item env signatures) (nth item-types index) item)
          type)
        hetero-vector-equal
        (let [[type left right] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (heterogeneous-vector-type? type)
            (fail! "heterogeneous vector equality requires [:vector [item-types...]]"
                   {:type type}))
          (require-type! (infer-type left env signatures) type left)
          (require-type! (infer-type right env signatures) type right)
          :i64)
        typed-set-new
        (let [[type & items] args]
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set constructor requires [:set item-type]" {:type type}))
          (when (> (count items) max-set-items)
            (fail! "typed set constructor exceeds item limit"
                   {:items (count items) :limit max-set-items}))
          (doseq [item items]
            (require-type! (infer-type item env signatures) (second type) item))
          type)
        typed-set-count
        (let [[type value] args]
          (require-arity! op args 2)
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set count requires [:set item-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          :i64)
        typed-set-contains
        (let [[type value item] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set membership requires [:set item-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type item env signatures) (second type) item)
          :bool)
        typed-set-conj
        (let [[type value item] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set insertion requires [:set item-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type item env signatures) (second type) item)
          type)
        typed-set-disj
        (let [[type value item] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set removal requires [:set item-type]" {:type type}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type item env signatures) (second type) item)
          type)
        typed-set-equal
        (let [[type left right] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (typed-set-type? type)
            (fail! "typed set equality requires [:set item-type]" {:type type}))
          (require-type! (infer-type left env signatures) type left)
          (require-type! (infer-type right env signatures) type right)
          :i64)
        record-new
        (let [[type & values] args
              fields (when (record-type? type) (nth type 2))]
          (validate-value-type! type)
          (when-not (and (record-type? type) (= (count fields) (count values)))
            (fail! "record constructor must exactly match its descriptor"
                   {:type type :values (count values)}))
          (doseq [[[field field-type] value] (map vector fields values)]
            (require-type! (infer-type value env signatures) field-type field))
          type)
        record-get
        (let [[type value field] args
              fields (when (record-type? type) (nth type 2))
              field-type (some (fn [[declared-field declared-type]]
                                 (when (= declared-field field) declared-type)) fields)]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (and (record-type? type) (keyword? field) field-type)
            (fail! "record field must be a declared keyword literal"
                   {:type type :field field}))
          (require-type! (infer-type value env signatures) type value)
          field-type)
        record-assoc
        (let [[type value field replacement] args
              fields (when (record-type? type) (nth type 2))
              field-type (some (fn [[declared-field declared-type]]
                                 (when (= declared-field field) declared-type)) fields)]
          (require-arity! op args 4)
          (validate-value-type! type)
          (when-not (and (record-type? type) (keyword? field) field-type)
            (fail! "record field must be a declared keyword literal"
                   {:type type :field field}))
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type replacement env signatures) field-type replacement)
          type)
        record-equal
        (let [[type left right] args]
          (require-arity! op args 3)
          (validate-value-type! type)
          (when-not (record-type? type)
            (fail! "record equality requires a record descriptor" {:type type}))
          (require-type! (infer-type left env signatures) type left)
          (require-type! (infer-type right env signatures) type right)
          :i64)
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
      (= op '=) (str "(valueEqual(" (a (first args)) "," (a (second args)) ")?1n:0n)")
      (contains? '#{< > <= >=} op)
      (let [js-op (case op < "<" > ">" <= "<=" >= ">=")]
        (str "((" (str/join (str " " js-op " ") (map a args)) ")?1n:0n)"))
      (= op 'bool-not) (str "(!" (a (first args)) ")")
      (= op 'option-some) (str "optionSome(" (a (first args)) ")")
      (= op 'option-none) "optionNone"
      (= op 'option-some?) (str "assertOptionI64(" (a (first args)) ")[0]")
      (= op 'option-value) (str "optionValue(" (a (first args)) ",()=>" (a (second args)) ")")
      (= op 'result-ok) (str "resultOk(" (a (first args)) ")")
      (= op 'result-err) (str "resultErr(" (a (first args)) ")")
      (= op 'result-ok?) (str "assertResultI64(" (a (first args)) ")[0]")
      (= op 'result-value) (str "resultValue(" (a (first args)) ",()=>" (a (second args)) ")")
      (= op 'result-error) (str "resultError(" (a (first args)) ",()=>" (a (second args)) ")")
      (= op 'result-ok-of) (str "makeParametricResult(" (type-js (first args)) ",true,"
                                (a (second args)) ")")
      (= op 'result-err-of) (str "makeParametricResult(" (type-js (first args)) ",false,"
                                 (a (second args)) ")")
      (= op 'result-ok?-of) (str "assertParametricResult(" (type-js (first args)) ","
                                 (a (second args)) ")[0]")
      (= op 'result-value-of) (str "parametricResultValue(" (type-js (first args)) ","
                                   (a (second args)) ",()=>" (a (nth args 2)) ")")
      (= op 'result-error-of) (str "parametricResultError(" (type-js (first args)) ","
                                   (a (second args)) ",()=>" (a (nth args 2)) ")")
      (= op 'result-match-of)
      (let [[type result ok-name ok-body err-name err-body] args]
        (str "parametricResultMatch(" (type-js type) "," (a result) ","
             "(" (js-name ok-name) ")=>" (emit-expr ok-body (assoc env ok-name (js-name ok-name)) functions) ","
             "(" (js-name err-name) ")=>" (emit-expr err-body (assoc env err-name (js-name err-name)) functions) ")"))
      (= op 'variant-new)
      (let [[type tag payload] args]
        (str "makeVariant(" (type-js type) "," (pr-str (str tag)) "," (a payload) ")"))
      (= op 'variant-match)
      (let [[type value branches] args]
        (str "matchVariant(" (type-js type) "," (a value) ",Object.freeze(["
             (str/join "," (map (fn [[tag binder body]]
                                  (str "Object.freeze([" (pr-str (str tag)) ",(" (js-name binder)
                                       ")=>" (emit-expr body (assoc env binder (js-name binder)) functions) "])"))
                                branches)) "]))"))
      (= op 'option-some-of)
      (str "makeGenericOption(" (type-js (first args)) ",true," (a (second args)) ")")
      (= op 'option-none-of)
      (str "makeGenericOption(" (type-js (first args)) ",false,null)")
      (= op 'option-some?-of)
      (str "assertGenericOption(" (type-js (first args)) "," (a (second args)) ")[1]")
      (= op 'option-value-of)
      (str "genericOptionValue(" (type-js (first args)) "," (a (second args)) ",()=>"
           (a (nth args 2)) ")")
      (= op 'option-match)
      (let [[type value none-body some-name some-body] args]
        (str "matchGenericOption(" (type-js type) "," (a value) ",()=>" (a none-body) ","
             "(" (js-name some-name) ")=>"
             (emit-expr some-body (assoc env some-name (js-name some-name)) functions) ")"))
      (= op 'hetero-vector-new)
      (str "makeHeterogeneousVector(" (type-js (first args)) ",["
           (str/join "," (map a (rest args))) "])")
      (= op 'hetero-vector-count)
      (str "BigInt(assertHeterogeneousVector(" (type-js (first args)) ","
           (a (second args)) ").length-1)")
      (= op 'hetero-vector-at)
      (str "heterogeneousVectorAt(" (type-js (first args)) "," (a (second args)) ","
           (nth args 2) ")")
      (= op 'hetero-vector-assoc)
      (str "heterogeneousVectorAssoc(" (type-js (first args)) "," (a (second args)) ","
           (nth args 2) "," (a (nth args 3)) ")")
      (= op 'hetero-vector-equal)
      (str "(heterogeneousVectorEqual(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")?1n:0n)")
      (= op 'typed-set-new)
      (str "makeTypedSet(" (type-js (first args)) ",["
           (str/join "," (map a (rest args))) "])")
      (= op 'typed-set-count)
      (str "BigInt(assertTypedSet(" (type-js (first args)) "," (a (second args)) ")[1].length)")
      (= op 'typed-set-contains)
      (str "typedSetContains(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-set-conj)
      (str "typedSetConj(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-set-disj)
      (str "typedSetDisj(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-set-equal)
      (str "(typedSetEqual(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")?1n:0n)")
      (= op 'record-new)
      (str "makeRecord(" (type-js (first args)) ",["
           (str/join "," (map a (rest args))) "])")
      (= op 'record-get)
      (str "recordGet(" (type-js (first args)) "," (a (second args)) ","
           (pr-str (str (nth args 2))) ")")
      (= op 'record-assoc)
      (str "recordAssoc(" (type-js (first args)) "," (a (second args)) ","
           (pr-str (str (nth args 2))) "," (a (nth args 3)) ")")
      (= op 'record-equal)
      (str "(recordEqual(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")?1n:0n)")
      (= op 'vector-new) (str "makeVector([" (str/join "," (map a args)) "])")
      (= op 'vector-count) (str "BigInt(assertVectorI64(" (a (first args)) ").length)")
      (= op 'vector-get) (str "vectorGet(" (a (nth args 0)) "," (a (nth args 1)) ",()=>"
                              (a (nth args 2)) ")")
      (= op 'vector-at) (str "vectorAt(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'vector-drop) (str "vectorDrop(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'vector-assoc) (str "vectorAssoc(" (a (nth args 0)) "," (a (nth args 1)) ","
                                (a (nth args 2)) ")")
      (= op 'vector-conj) (str "vectorConj(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'pair) (str "Object.freeze([" (a (first args)) "," (a (second args)) "])")
      (= op 'pair-first) (str (a (first args)) "[0]")
      (= op 'pair-second) (str (a (first args)) "[1]")
      (= op 'cap-call) (str "callCapability(" (first args) "," (a (second args)) ")")
      (= op 'string-byte-length) (str "BigInt(utf8Bytes(" (a (first args)) "))")
      (= op 'string=?) (str "((" (a (first args)) "===" (a (second args)) ")?1n:0n)")
      (= op 'string-concat) (str "assertString(" (a (first args)) "+" (a (second args)) ")")
      (= op 'map-new)
      (str "makeMap([" (str/join "," (map (fn [[key value]]
                                               (str "[" (a key) "," (a value) "]"))
                                             (partition 2 args))) "])")
      (= op 'map-get) (str "mapGet(" (a (nth args 0)) "," (a (nth args 1)) ",()=>"
                           (a (nth args 2)) ")")
      (= op 'map-assoc)
      (str "mapAssoc(" (a (first args)) ",["
           (str/join "," (map (fn [[key value]] (str "[" (a key) "," (a value) "]"))
                              (partition 2 (rest args)))) "])")
      (contains? functions op)
      (str (js-name op) "(" (str/join "," (map a args)) ")")
      :else (fail! "unsupported KIR operation" {:operation op}))))

(defn emit-expr [form env functions]
  (cond
    (integer? form) (bigint-literal form)
    (string? form) (js-string form)
    (keyword? form) (js-string (keyword-text form))
    (boolean? form) (if form "true" "false")
    (nil? form) "optionNone"
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
             (str "(()=>{const t=" (emit-expr test env functions)
                  ";return (typeof t==='boolean'?t:t!==0n)?"
                  (emit-expr then env functions) ":"
                  (emit-expr else env functions) ";})()"))
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
                                                         (guard-expr type (js-name param)) ";"))
                                                  params param-types))]
                           (str "function " (js-name name) "("
                                (str/join "," (map js-name params)) "){charge();" guards "return "
                                (guard-expr result (emit-expr body env functions)) ";}")))
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
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",mapLimits:Object.freeze({entries:" max-map-entries "})"
                    ",vectorLimits:Object.freeze({items:" max-vector-items "})"
                    ",booleanProfile:'strict-v1',optionProfile:'tagged-i64-v1'"
                    ",genericOptionProfile:'typed-tagged-v1'"
                    ",resultProfile:'tagged-i64-i64-v1'"
                    ",parametricAdtLimits:Object.freeze({depth:" max-type-depth
                    ",nodes:" max-type-nodes ",variantCases:" max-variant-cases "})"))
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",heterogeneousVectorLimits:Object.freeze({items:"
                    max-heterogeneous-vector-items "})"))
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",typedSetLimits:Object.freeze({items:" max-set-items "})"))
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",recordLimits:Object.freeze({fields:" max-record-fields "})"))
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
             "const assertBool=v=>{if(typeof v!=='boolean')throw new Error('invalid-bool');return v;};"
             "const optionNone=Object.freeze([false]);"
             "const optionSome=v=>Object.freeze([true,assertI64(v)]);"
             "const assertOptionI64=v=>{if(!Array.isArray(v)||(v.length!==1&&v.length!==2)||"
             "typeof v[0]!=='boolean'||(v[0]&&v.length!==2)||(!v[0]&&v.length!==1))"
             "throw new Error('invalid-option-i64');return v[0]?optionSome(v[1]):optionNone;};"
             "const optionValue=(v,fallback)=>{v=assertOptionI64(v);return v[0]?v[1]:assertI64(fallback());};"
             "const resultOk=v=>Object.freeze([true,assertI64(v)]);"
             "const resultErr=e=>Object.freeze([false,assertI64(e)]);"
             "const assertResultI64=v=>{if(!Array.isArray(v)||v.length!==2||typeof v[0]!=='boolean'||"
             "typeof v[1]!=='bigint'||i64(v[1])!==v[1])"
             "throw new Error('invalid-result-i64');return v[0]?resultOk(v[1]):resultErr(v[1]);};"
             "const resultValue=(v,fallback)=>{v=assertResultI64(v);return v[0]?v[1]:assertI64(fallback());};"
             "const resultError=(v,fallback)=>{v=assertResultI64(v);return v[0]?assertI64(fallback()):v[1];};"
             "const sameType=(a,b)=>a===b||(Array.isArray(a)&&Array.isArray(b)&&a.length===b.length&&a.every((x,i)=>sameType(x,b[i])));"
             "const assertTypedValue=(t,v,d,s)=>{if(++s.nodes>" max-type-nodes
             ")throw new Error('adt-node-limit');if(d>" max-type-depth
             ")throw new Error('adt-depth-limit');if(t==='i64')return assertI64(v);"
             "if(t==='string')return assertString(v);if(t==='keyword')return assertKeyword(v);"
             "if(t==='map')return assertMap(v);if(t==='bool')return assertBool(v);"
             "if(t==='option-i64')return assertOptionI64(v);if(t==='result-i64')return assertResultI64(v);"
             "if(t==='vector-i64')return assertVectorI64(v);"
             "if(Array.isArray(t)&&t.length===2&&t[0]==='vector'&&Array.isArray(t[1])){"
             "if(t[1].length>" max-heterogeneous-vector-items
             "||!Array.isArray(v)||v.length!==t[1].length+1||!sameType(v[0],t))"
             "throw new Error('invalid-heterogeneous-vector');const out=[t];"
             "for(let i=0;i<t[1].length;i++)out.push(assertTypedValue(t[1][i],v[i+1],d+1,s));"
             "return Object.freeze(out);}"
             "if(Array.isArray(t)&&t.length===2&&t[0]==='set'){"
             "if(!Array.isArray(v)||v.length!==2||!sameType(v[0],t)||!Array.isArray(v[1])||v[1].length>"
             max-set-items ")throw new Error('invalid-typed-set');"
             "const items=v[1].map(x=>assertTypedValue(t[1],x,d+1,s));"
             "items.sort((a,b)=>compareTyped(t[1],a,b));"
             "for(let i=1;i<items.length;i++)if(compareTyped(t[1],items[i-1],items[i])===0)"
             "throw new Error('duplicate-set-item');return Object.freeze([t,Object.freeze(items)]);}"
             "if(Array.isArray(t)&&t.length===3&&t[0]==='record'){"
             "if(!Array.isArray(t[2])||t[2].length<1||t[2].length>" max-record-fields
             "||!Array.isArray(v)||v.length!==t[2].length+1||!sameType(v[0],t))"
             "throw new Error('invalid-record');const out=[t];"
             "for(let i=0;i<t[2].length;i++)out.push(assertTypedValue(t[2][i][1],v[i+1],d+1,s));"
             "return Object.freeze(out);}"
             "if(Array.isArray(t)&&t.length===2&&t[0]==='option'){"
             "if(!Array.isArray(v)||!sameType(v[0],t)||typeof v[1]!=='boolean'||"
             "(v[1]&&v.length!==3)||(!v[1]&&v.length!==2))throw new Error('invalid-generic-option');"
             "return v[1]?Object.freeze([t,true,assertTypedValue(t[1],v[2],d+1,s)]):Object.freeze([t,false]);}"
             "if(Array.isArray(t)&&t.length===3&&t[0]==='result'){"
             "if(!Array.isArray(v)||v.length!==2||typeof v[0]!=='boolean')throw new Error('invalid-parametric-result');"
             "const p=v[0]?t[1]:t[2];return Object.freeze([v[0],assertTypedValue(p,v[1],d+1,s)]);}"
             "if(Array.isArray(t)&&t.length===3&&t[0]==='variant'){"
             "if(!Array.isArray(v)||v.length!==3||!sameType(v[0],t)||typeof v[1]!=='string')throw new Error('invalid-variant');"
             "const c=t[2].find(c=>c[0]===v[1]);if(!c)throw new Error('unknown-variant-case');"
             "return Object.freeze([t,v[1],assertTypedValue(c[1],v[2],d+1,s)]);}"
             "throw new Error('invalid-type-descriptor');};"
             "const assertParametricResult=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeParametricResult=(t,tag,payload)=>assertParametricResult(t,[tag,payload]);"
             "const parametricResultValue=(t,v,fallback)=>{v=assertParametricResult(t,v);"
             "return v[0]?v[1]:assertTypedValue(t[1],fallback(),1,{nodes:0});};"
             "const parametricResultError=(t,v,fallback)=>{v=assertParametricResult(t,v);"
             "return v[0]?assertTypedValue(t[2],fallback(),1,{nodes:0}):v[1];};"
             "const parametricResultMatch=(t,v,ok,err)=>{v=assertParametricResult(t,v);"
             "return v[0]?ok(v[1]):err(v[1]);};"
             "const makeVariant=(t,tag,payload)=>assertTypedValue(t,[t,tag,payload],0,{nodes:0});"
             "const matchVariant=(t,v,branches)=>{v=assertTypedValue(t,v,0,{nodes:0});"
             "const b=branches.find(b=>b[0]===v[1]);if(!b)throw new Error('unknown-variant-case');return b[1](v[2]);};"
             "const assertGenericOption=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeGenericOption=(t,some,payload)=>assertGenericOption(t,some?[t,true,payload]:[t,false]);"
             "const genericOptionValue=(t,v,fallback)=>{v=assertGenericOption(t,v);"
             "return v[1]?v[2]:assertTypedValue(t[1],fallback(),1,{nodes:0});};"
             "const matchGenericOption=(t,v,none,some)=>{v=assertGenericOption(t,v);return v[1]?some(v[2]):none();};"
             "const assertHeterogeneousVector=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeHeterogeneousVector=(t,items)=>assertHeterogeneousVector(t,[t,...items]);"
             "const heterogeneousVectorAt=(t,v,i)=>assertHeterogeneousVector(t,v)[i+1];"
             "const heterogeneousVectorAssoc=(t,v,i,item)=>{v=assertHeterogeneousVector(t,v);"
             "const out=v.slice();out[i+1]=item;return assertHeterogeneousVector(t,out);};"
             "const heterogeneousVectorEqual=(t,a,b)=>sameType(assertHeterogeneousVector(t,a),assertHeterogeneousVector(t,b));"
             "const cmp=(a,b)=>a<b?-1:a>b?1:0;"
             "const compareList=(types,a,b)=>{for(let i=0;i<types.length;i++){const c=compareTyped(types[i],a[i],b[i]);if(c)return c;}return 0;};"
             "const compareTyped=(t,a,b)=>{if(t==='i64'||t==='string'||t==='keyword')return cmp(a,b);"
             "if(t==='bool')return a===b?0:(a?1:-1);"
             "if(t==='option-i64'){if(a[0]!==b[0])return a[0]?1:-1;return a[0]?cmp(a[1],b[1]):0;}"
             "if(t==='result-i64'){if(a[0]!==b[0])return a[0]?1:-1;return cmp(a[1],b[1]);}"
             "if(t==='vector-i64'){const n=Math.min(a.length,b.length);for(let i=0;i<n;i++){const c=cmp(a[i],b[i]);if(c)return c;}return cmp(a.length,b.length);}"
             "if(t==='map'){const n=Math.min(a.length,b.length);for(let i=0;i<n;i++){let c=cmp(a[i][0],b[i][0]);if(c)return c;c=cmp(a[i][1],b[i][1]);if(c)return c;}return cmp(a.length,b.length);}"
             "if(Array.isArray(t)&&t[0]==='option'){if(a[1]!==b[1])return a[1]?1:-1;return a[1]?compareTyped(t[1],a[2],b[2]):0;}"
             "if(Array.isArray(t)&&t[0]==='result'){if(a[0]!==b[0])return a[0]?1:-1;return compareTyped(a[0]?t[1]:t[2],a[1],b[1]);}"
             "if(Array.isArray(t)&&t[0]==='variant'){const ai=t[2].findIndex(c=>c[0]===a[1]),bi=t[2].findIndex(c=>c[0]===b[1]);"
             "if(ai!==bi)return cmp(ai,bi);return compareTyped(t[2][ai][1],a[2],b[2]);}"
             "if(Array.isArray(t)&&t[0]==='vector')return compareList(t[1],a.slice(1),b.slice(1));"
             "if(Array.isArray(t)&&t[0]==='set'){const ai=a[1],bi=b[1],n=Math.min(ai.length,bi.length);"
             "for(let i=0;i<n;i++){const c=compareTyped(t[1],ai[i],bi[i]);if(c)return c;}return cmp(ai.length,bi.length);}"
             "if(Array.isArray(t)&&t[0]==='record')return compareList(t[2].map(f=>f[1]),a.slice(1),b.slice(1));"
             "throw new Error('unordered-value-type');};"
             "const assertTypedSet=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeTypedSet=(t,items)=>assertTypedSet(t,[t,items]);"
             "const typedSetContains=(t,v,item)=>{v=assertTypedSet(t,v);item=assertTypedValue(t[1],item,1,{nodes:0});"
             "return v[1].some(x=>compareTyped(t[1],x,item)===0);};"
             "const typedSetConj=(t,v,item)=>{v=assertTypedSet(t,v);item=assertTypedValue(t[1],item,1,{nodes:0});"
             "if(v[1].some(x=>compareTyped(t[1],x,item)===0))return v;"
             "if(v[1].length>=" max-set-items ")throw new Error('set-too-large');return makeTypedSet(t,[...v[1],item]);};"
             "const typedSetDisj=(t,v,item)=>{v=assertTypedSet(t,v);item=assertTypedValue(t[1],item,1,{nodes:0});"
             "return makeTypedSet(t,v[1].filter(x=>compareTyped(t[1],x,item)!==0));};"
             "const typedSetEqual=(t,a,b)=>{a=assertTypedSet(t,a);b=assertTypedSet(t,b);"
             "return a[1].length===b[1].length&&a[1].every((x,i)=>compareTyped(t[1],x,b[1][i])===0);};"
             "const assertRecord=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeRecord=(t,values)=>assertRecord(t,[t,...values]);"
             "const recordFieldIndex=(t,field)=>{const i=t[2].findIndex(f=>f[0]===field);"
             "if(i<0)throw new Error('unknown-record-field');return i;};"
             "const recordGet=(t,v,field)=>{v=assertRecord(t,v);return v[recordFieldIndex(t,field)+1];};"
             "const recordAssoc=(t,v,field,value)=>{v=assertRecord(t,v);const i=recordFieldIndex(t,field);"
             "const out=v.slice();out[i+1]=assertTypedValue(t[2][i][1],value,1,{nodes:0});return assertRecord(t,out);};"
             "const recordEqual=(t,a,b)=>{a=assertRecord(t,a);b=assertRecord(t,b);"
             "return compareList(t[2].map(f=>f[1]),a.slice(1),b.slice(1))===0;};"
             "const valueEqual=(a,b)=>{if(Array.isArray(a)||Array.isArray(b)){"
             "if((Array.isArray(a)&&typeof a[0]==='boolean')||(Array.isArray(b)&&typeof b[0]==='boolean')){"
             "if(Array.isArray(a)&&a.length===2&&Array.isArray(b)&&b.length===2){"
             "a=assertResultI64(a);b=assertResultI64(b);return a[0]===b[0]&&a[1]===b[1];}"
             "a=assertOptionI64(a);b=assertOptionI64(b);return a[0]===b[0]&&(!a[0]||a[1]===b[1]);}"
             "a=assertVectorI64(a);b=assertVectorI64(b);return a.length===b.length&&a.every((v,i)=>v===b[i]);}"
             "return a===b;};"
             "const makeVector=items=>{if(!Array.isArray(items))throw new Error('invalid-vector-i64');"
             "if(items.length>" max-vector-items ")throw new Error('vector-too-large');"
             "return Object.freeze(items.map(assertI64));};"
             "const assertVectorI64=v=>makeVector(v);"
             "const vectorGet=(v,i,fallback)=>{v=assertVectorI64(v);i=assertI64(i);"
             "return i>=0n&&i<BigInt(v.length)?v[Number(i)]:assertI64(fallback());};"
             "const vectorAt=(v,i)=>{v=assertVectorI64(v);i=assertI64(i);"
             "if(i<0n||i>=BigInt(v.length))throw new Error('vector-index-out-of-range');return v[Number(i)];};"
             "const vectorDrop=(v,n)=>{v=assertVectorI64(v);n=assertI64(n);"
             "if(n<0n||n>BigInt(v.length))throw new Error('vector-drop-out-of-range');return makeVector(v.slice(Number(n)));};"
             "const vectorAssoc=(v,i,item)=>{v=assertVectorI64(v);i=assertI64(i);"
             "if(i<0n||i>=BigInt(v.length))throw new Error('vector-index-out-of-range');"
             "const out=v.slice();out[Number(i)]=assertI64(item);return makeVector(out);};"
             "const vectorConj=(v,item)=>{v=assertVectorI64(v);if(v.length>=" max-vector-items
             ")throw new Error('vector-too-large');return makeVector([...v,assertI64(item)]);};"
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
             "const makeMap=entries=>{if(!Array.isArray(entries)||entries.length>" max-map-entries
             ")throw new Error('map-too-large');const seen=new Set();const out=[];"
             "for(const entry of entries){if(!Array.isArray(entry)||entry.length!==2)throw new Error('invalid-map');"
             "const k=assertKeyword(entry[0]);const v=assertI64(entry[1]);"
             "if(seen.has(k))throw new Error('duplicate-map-key');seen.add(k);out.push(Object.freeze([k,v]));}"
             "out.sort((a,b)=>a[0]<b[0]?-1:a[0]>b[0]?1:0);return Object.freeze(out);};"
             "const assertMap=v=>makeMap(v);"
             "const mapGet=(m,k,fallback)=>{m=assertMap(m);k=assertKeyword(k);"
             "for(const e of m)if(e[0]===k)return e[1];return fallback();};"
             "const mapAssoc=(m,entries)=>{m=assertMap(m);const merged=new Map(m);"
             "for(const e of entries){if(!Array.isArray(e)||e.length!==2)throw new Error('invalid-map');"
             "merged.set(assertKeyword(e[0]),assertI64(e[1]));}return makeMap(Array.from(merged));};"
             "let fuel=256;"
             "const charge=()=>{fuel--;if(fuel<0)throw new Error('fuel-exhausted');};"
             "const quot=(a,b)=>{if(b===0n)throw new Error('division-by-zero');"
             "if(a===-9223372036854775808n&&b===-1n)throw new Error('signed-division-overflow');return a/b;};"
             "const callCapability=(id,value)=>{const f=grants[id];"
             "if(typeof f!=='function')throw new Error('capability-denied:'+id);return i64(BigInt(f(value)));};\n"
             function-source "\n"
             "return Object.freeze({" (str/join "," (map (fn [f] (str "'" f "':" (js-name f))) exports)) "});\n}\n")]
    (verify-output! source))))
