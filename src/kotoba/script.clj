(ns kotoba.script
  "Checked KIR -> restricted ES module backend. No source reader lives here."
  (:require [clojure.string :as str])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile]
           [com.google.javascript.jscomp CompilerOptions$LanguageMode]
           [com.google.javascript.rhino Node]))

(def artifact-schema "kotoba-js-artifact/v1")
(def floating-point-policy "ieee-754-f32-f64-v7")
(def supported-kir-formats #{:kotoba.kir/v3 :kotoba.kir/v4})
(def ^:private value-types #{:i64 :f32 :f64 :string :keyword :map :bool :option-i64 :result-i64
                             :vector-i64 :vector-f64 :string-index :disjoint-set-i64 :document})
(def ^:private max-string-literal-bytes 4096)
(def ^:private max-string-value-bytes 65536)
(def ^:private max-keyword-bytes 512)
(def ^:private max-map-entries 128)
(def ^:private max-vector-items 16384)
(def ^:private max-type-depth 8)
(def ^:private max-type-nodes 64)
(def ^:private max-variant-cases 32)
(def ^:private max-heterogeneous-vector-items 32)
(def ^:private max-set-items 32)
(def ^:private max-typed-map-entries 31)
(def ^:private max-record-fields 32)
(def ^:private max-compact-graph-items 128)
(def ^:private max-string-index-key-bytes 65536)
(def ^:private max-document-depth 8)
(def ^:private max-document-nodes 256)
(def ^:private max-document-container-items 32)
(def ^:private max-document-utf8-bytes 65536)
(def ^:private max-xml-nodes 2048)
(def ^:private max-xml-depth 32)
(def ^:private max-xml-attributes 32)
(def ^:private max-xml-path-segments 32)
(def ^:private max-decimal-f64-bytes 64)
(def ^:private max-decimal-f64x3-bytes 194)

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
     (contains? value-types type)
     type
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
     (do (when (contains? #{:f32 :f64} (second type))
           (fail! "direct floating set items are outside the structured scalar ABI" {:type type}))
         (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 3 (count type)) (= :map (first type)))
     (do (when (some #{:f32 :f64} [(second type) (nth type 2)])
           (fail! "direct floating map keys or values are outside the structured scalar ABI" {:type type}))
         (validate-value-type! (second type) (inc depth) nodes)
         (validate-value-type! (nth type 2) (inc depth) nodes)
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

(defn- canonical-typed-map-type? [type]
  (and (vector? type) (= 3 (count type)) (= :map (first type))))

(defn- record-type? [type]
  (and (vector? type) (= 3 (count type)) (= :record (first type))))

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
    :else (Double/toString n)))

(defn- js-string [value]
  (if (string? value) (pr-str value) "null"))

(declare validate-value-type! parametric-result-type? variant-type? generic-option-type?
         heterogeneous-vector-type? typed-set-type? canonical-typed-map-type? record-type?)

(defn- type-js [type]
  (validate-value-type! type)
  (cond
    (keyword? type) (pr-str (if (= type :document) "doc" (name type)))
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
    (canonical-typed-map-type? type)
    (str "Object.freeze(['map'," (type-js (second type)) ","
         (type-js (nth type 2)) "])")
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
          (heterogeneous-vector-type? type) (typed-set-type? type)
          (canonical-typed-map-type? type) (record-type? type))
    (str "assertTypedValue(" (type-js type) "," expression ",0,{nodes:0})")
    (str (case type
           :f32 "assertF32("
           :f64 "assertF64("
           :string "assertString("
           :keyword "assertKeyword("
           :map "assertMap("
           :bool "assertBool("
           :option-i64 "assertOptionI64("
           :result-i64 "assertResultI64("
           :vector-i64 "assertVectorI64("
           :vector-f64 "assertVectorF64("
           :string-index "assertStringIndex("
           :disjoint-set-i64 "assertDisjointSetI64("
           :document "assertDoc("
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

      (contains? '#{i32-wrap u32-wrap xorshift32} op)
      (do (require-arity! op args 1)
          (require-type! (first types) :i64 (first args)) :i64)

      (contains? '#{i32-wrapping-add i32-wrapping-mul i32-xor} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :i64 arg)) :i64)

      (contains? '#{i32-shift-left i32-shift-right u32-shift-right} op)
      (do (require-arity! op args 2)
          (require-type! (first types) :i64 (first args))
          (when-not (and (integer? (second args)) (<= 0 (second args) 31))
            (fail! "i32 shift count must be an integer literal in [0,31]"
                   {:operation op :count (second args)}))
          :i64)

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

      (= op 'vector-f64-new)
      (do (doseq [[arg type] (map vector args types)] (require-type! type :f64 arg))
          (when (> (count args) max-vector-items)
            (fail! "KIR f64 vector literal exceeds item limit"
                   {:items (count args) :limit max-vector-items}))
          :vector-f64)

      (= op 'vector-f64-count)
      (do (require-arity! op args 1)
          (require-type! (first types) :vector-f64 (first args)) :i64)

      (= op 'vector-f64-get)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :vector-f64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :f64 (nth args 2)) :f64)

      (= op 'vector-f64-at)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-f64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :f64)

      (= op 'vector-f64-drop)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-f64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :vector-f64)

      (= op 'vector-f64-assoc)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :vector-f64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :f64 (nth args 2)) :vector-f64)

      (= op 'vector-f64-conj)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :vector-f64 (nth args 0))
          (require-type! (nth types 1) :f64 (nth args 1)) :vector-f64)

      (= op 'string-index-new) (do (require-arity! op args 0) :string-index)
      (= op 'string-index-count)
      (do (require-arity! op args 1) (require-type! (first types) :string-index (first args)) :i64)
      (= op 'string-index-contains)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :string-index (nth args 0))
          (require-type! (nth types 1) :string (nth args 1)) :bool)
      (= op 'string-index-get)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :string-index (nth args 0))
          (require-type! (nth types 1) :string (nth args 1)) [:option :i64])
      (= op 'string-index-assoc)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :string-index (nth args 0))
          (require-type! (nth types 1) :string (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2)) :string-index)
      (= op 'disjoint-set-i64-new)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :disjoint-set-i64)
      (= op 'disjoint-set-i64-count)
      (do (require-arity! op args 1) (require-type! (first types) :disjoint-set-i64 (first args)) :i64)
      (= op 'disjoint-set-i64-union)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :disjoint-set-i64 (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2)) [:option :disjoint-set-i64])

      (contains? '#{document-null} op)
      (do (require-arity! op args 0) :document)
      (= op 'document-bool)
      (do (require-arity! op args 1) (require-type! (first types) :bool (first args)) :document)
      (= op 'document-i64)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :document)
      (= op 'document-f64)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :document)
      (= op 'document-string)
      (do (require-arity! op args 1) (require-type! (first types) :string (first args)) :document)
      (= op 'document-keyword)
      (do (require-arity! op args 1) (require-type! (first types) :keyword (first args)) :document)
      (= op 'document-vector)
      (do (doseq [[arg type] (map vector args types)] (require-type! type :document arg))
          (when (> (count args) max-document-container-items)
            (fail! "KIR document vector exceeds item limit" {:items (count args)}))
          :document)
      (= op 'document-map)
      (do (when (odd? (count args))
            (fail! "KIR document map requires key/value pairs" {:node args}))
          (doseq [[[key-form value-form] [key-type value-type]]
                  (map vector (partition 2 args) (partition 2 types))]
            (require-type! key-type :keyword key-form)
            (require-type! value-type :document value-form))
          (when (> (quot (count args) 2) max-document-container-items)
            (fail! "KIR document map exceeds entry limit" {:entries (quot (count args) 2)}))
          :document)
      (= op 'document-count)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args)) :i64)
      (= op 'document-kind)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args)) :keyword)
      (= op 'document-equal?)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :document arg))
          :bool)
      (= op 'document-vector-at)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) [:option :document])
      (= op 'document-map-entry-at)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) [:option :document])
      (= op 'document-vector-assoc)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1))
          (require-type! (nth types 2) :document (nth args 2)) :document)
      (= op 'document-vector-conj)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :document (nth args 1)) :document)
      (= op 'document-vector-drop)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :document)
      (= op 'document-vector-remove)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :i64 (nth args 1)) :document)
      (= op 'document-contains)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :keyword (nth args 1)) :bool)
      (= op 'document-get)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :keyword (nth args 1)) [:option :document])
      (= op 'document-assoc)
      (do (require-arity! op args 3)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :keyword (nth args 1))
          (require-type! (nth types 2) :document (nth args 2)) :document)
      (= op 'document-dissoc)
      (do (require-arity! op args 2)
          (require-type! (nth types 0) :document (nth args 0))
          (require-type! (nth types 1) :keyword (nth args 1)) :document)
      (= op 'document-merge)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :document arg))
          :document)
      (= op 'document-string-value)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args))
          [:option :string])
      (= op 'document-keyword-value)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args))
          [:option :keyword])
      (= op 'document-bool-value)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args))
          [:option :bool])
      (= op 'document-i64-value)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args))
          [:option :i64])
      (= op 'document-f64-value)
      (do (require-arity! op args 1) (require-type! (first types) :document (first args))
          [:option :f64])

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
      (= op 'string-replace-all)
      (do (require-arity! op args 3)
          (doseq [[arg type] (map vector args types)] (require-type! type :string arg)) :string)
      (= op 'keyword-from-string)
      (do (require-arity! op args 1)
          (require-type! (first types) :string (first args)) :keyword)
      (= op 'keyword-name)
      (do (require-arity! op args 1)
          (require-type! (first types) :keyword (first args)) :string)
      (= op 'xml-path-count)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :string arg)) :i64)
      (= op 'xml-path-attr)
      (do (require-arity! op args 4)
          (require-type! (nth types 0) :string (nth args 0))
          (require-type! (nth types 1) :string (nth args 1))
          (require-type! (nth types 2) :i64 (nth args 2))
          (require-type! (nth types 3) :string (nth args 3))
          [:option :string])
      (= op 'decimal-f64-parse)
      (do (require-arity! op args 1)
          (require-type! (first types) :string (first args))
          [:option :f64])
      (= op 'decimal-f64x3-parse)
      (do (require-arity! op args 1)
          (require-type! (first types) :string (first args))
          [:option [:vector [:f64 :f64 :f64]]])

      (= op 'f64-to-bits)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :i64)
      (= op 'f64-from-bits)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :f64)
      (contains? '#{i64-to-f64-checked i64-to-f64-rounded} op)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :f64)
      (contains? '#{f64-to-i64-checked f64-to-i64-truncating} op)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :i64)
      (= op 'f32-to-bits)
      (do (require-arity! op args 1) (require-type! (first types) :f32 (first args)) :i64)
      (= op 'f32-from-bits)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :f32)
      (= op 'f64-to-f32-rounded)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :f32)
      (= op 'f32-to-f64-exact)
      (do (require-arity! op args 1) (require-type! (first types) :f32 (first args)) :f64)
      (contains? '#{i64-to-f32-checked i64-to-f32-rounded} op)
      (do (require-arity! op args 1) (require-type! (first types) :i64 (first args)) :f32)
      (contains? '#{f32-to-i64-checked f32-to-i64-truncating} op)
      (do (require-arity! op args 1) (require-type! (first types) :f32 (first args)) :i64)
      (contains? '#{f32-add f32-sub f32-mul f32-div f32-min f32-max} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :f32 arg)) :f32)
      (contains? '#{f32-neg f32-abs f32-sqrt} op)
      (do (require-arity! op args 1) (require-type! (first types) :f32 (first args)) :f32)
      (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :f32 arg)) :bool)
      (contains? '#{f64-add f64-sub f64-mul f64-div f64-min f64-max} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :f64 arg))
          :f64)
      (contains? '#{f64-neg f64-abs f64-sqrt} op)
      (do (require-arity! op args 1)
          (require-type! (first types) :f64 (first args))
          :f64)
      (contains? '#{f64-sin-quarter-turn f64-cos-quarter-turn} op)
      (do (require-arity! op args 1)
          (require-type! (first types) :f64 (first args))
          :f64)
      (contains? '#{f64-sin-bounded f64-cos-bounded} op)
      (do (require-arity! op args 1)
          (require-type! (first types) :f64 (first args))
          :f64)
      (= op 'f64-exp-near-zero)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :f64)
      (= op 'f64-log-near-one)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :f64)
      (= op 'f64-atan2-bounded)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :f64 arg))
          :f64)
      (contains? '#{f64-exp-bounded f64-log-bounded} op)
      (do (require-arity! op args 1) (require-type! (first types) :f64 (first args)) :f64)
      (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered} op)
      (do (require-arity! op args 2)
          (doseq [[arg type] (map vector args types)] (require-type! type :f64 arg))
          :bool)

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
    (instance? Double form) :f64
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
        typed-map-new
        (let [[type & entries] args]
          (validate-value-type! type)
          (when-not (and (canonical-typed-map-type? type) (even? (count entries))
                         (<= (/ (count entries) 2) max-typed-map-entries))
            (fail! "typed map constructor shape or entry limit is invalid" {:type type}))
          (doseq [[key value] (partition 2 entries)]
            (require-type! (infer-type key env signatures) (second type) key)
            (require-type! (infer-type value env signatures) (nth type 2) value))
          type)
        typed-map-count
        (let [[type value] args]
          (require-arity! op args 2) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value) :i64)
        typed-map-contains
        (let [[type value key] args]
          (require-arity! op args 3) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type key env signatures) (second type) key) :bool)
        typed-map-get
        (let [[type value key] args]
          (require-arity! op args 3) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type key env signatures) (second type) key)
          [:option (nth type 2)])
        typed-map-entry-at
        (let [[type value index] args]
          (require-arity! op args 3) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type index env signatures) :i64 index)
          [:option [:vector [(second type) (nth type 2)]]])
        typed-map-assoc
        (let [[type value key item] args]
          (require-arity! op args 4) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type key env signatures) (second type) key)
          (require-type! (infer-type item env signatures) (nth type 2) item) type)
        typed-map-dissoc
        (let [[type value key] args]
          (require-arity! op args 3) (validate-value-type! type)
          (require-type! (infer-type value env signatures) type value)
          (require-type! (infer-type key env signatures) (second type) key) type)
        typed-map-equal
        (let [[type left right] args]
          (require-arity! op args 3) (validate-value-type! type)
          (require-type! (infer-type left env signatures) type left)
          (require-type! (infer-type right env signatures) type right) :i64)
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
      (= op 'i32-wrap) (str "i32Wrap(" (a (first args)) ")")
      (= op 'u32-wrap) (str "u32Wrap(" (a (first args)) ")")
      (= op 'i32-wrapping-add) (str "i32Add(" (a (first args)) "," (a (second args)) ")")
      (= op 'i32-wrapping-mul) (str "i32Mul(" (a (first args)) "," (a (second args)) ")")
      (= op 'i32-xor) (str "i32Xor(" (a (first args)) "," (a (second args)) ")")
      (= op 'i32-shift-left) (str "i32Shl(" (a (first args)) "," (a (second args)) ")")
      (= op 'i32-shift-right) (str "i32Shr(" (a (first args)) "," (a (second args)) ")")
      (= op 'u32-shift-right) (str "u32Shr(" (a (first args)) "," (a (second args)) ")")
      (= op 'xorshift32) (str "xorshift32(" (a (first args)) ")")
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
      (= op 'typed-map-new)
      (str "makeTypedMap(" (type-js (first args)) ",["
           (str/join "," (map (fn [[key item]]
                                (str "[" (a key) "," (a item) "]"))
                              (partition 2 (rest args)))) "])")
      (= op 'typed-map-count)
      (str "BigInt(assertTypedMap(" (type-js (first args)) ","
           (a (second args)) ")[1].length)")
      (= op 'typed-map-contains)
      (str "typedMapContains(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-map-get)
      (str "typedMapGet(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-map-entry-at)
      (str "typedMapEntryAt(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-map-assoc)
      (str "typedMapAssoc(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) "," (a (nth args 3)) ")")
      (= op 'typed-map-dissoc)
      (str "typedMapDissoc(" (type-js (first args)) "," (a (second args)) ","
           (a (nth args 2)) ")")
      (= op 'typed-map-equal)
      (str "(typedMapEqual(" (type-js (first args)) "," (a (second args)) ","
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
      (= op 'vector-f64-new) (str "makeVectorF64([" (str/join "," (map a args)) "])")
      (= op 'vector-f64-count) (str "BigInt(assertVectorF64(" (a (first args)) ").length)")
      (= op 'vector-f64-get) (str "vectorF64Get(" (a (nth args 0)) "," (a (nth args 1)) ",()=>"
                                  (a (nth args 2)) ")")
      (= op 'vector-f64-at) (str "vectorF64At(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'vector-f64-drop) (str "vectorF64Drop(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'vector-f64-assoc) (str "vectorF64Assoc(" (a (nth args 0)) "," (a (nth args 1)) ","
                                    (a (nth args 2)) ")")
      (= op 'vector-f64-conj) (str "vectorF64Conj(" (a (nth args 0)) "," (a (nth args 1)) ")")
      (= op 'string-index-new) "makeStringIndex([])"
      (= op 'string-index-count) (str "BigInt(assertStringIndex(" (a (first args)) ").length)")
      (= op 'string-index-contains) (str "stringIndexContains(" (a (first args)) "," (a (second args)) ")")
      (= op 'string-index-get) (str "stringIndexGet(" (a (first args)) "," (a (second args)) ")")
      (= op 'string-index-assoc) (str "stringIndexAssoc(" (a (nth args 0)) "," (a (nth args 1)) "," (a (nth args 2)) ")")
      (= op 'disjoint-set-i64-new) (str "makeDisjointSetI64(" (a (first args)) ")")
      (= op 'disjoint-set-i64-count) (str "BigInt(assertDisjointSetI64(" (a (first args)) ")[0].length)")
      (= op 'disjoint-set-i64-union) (str "disjointSetI64Union(" (a (nth args 0)) "," (a (nth args 1)) "," (a (nth args 2)) ")")
      (= op 'document-null) "docNull"
      (= op 'document-bool) (str "makeDocScalar('bool'," (a (first args)) ")")
      (= op 'document-i64) (str "makeDocScalar('i64'," (a (first args)) ")")
      (= op 'document-f64) (str "makeDocScalar('f64'," (a (first args)) ")")
      (= op 'document-string) (str "makeDocScalar('string'," (a (first args)) ")")
      (= op 'document-keyword) (str "makeDocScalar('keyword'," (a (first args)) ")")
      (= op 'document-vector) (str "makeDocVector([" (str/join "," (map a args)) "])")
      (= op 'document-map)
      (str "makeDocMap(["
           (str/join "," (map (fn [[key item]] (str "[" (a key) "," (a item) "]"))
                                (partition 2 args))) "])")
      (= op 'document-count) (str "docCount(" (a (first args)) ")")
      (= op 'document-kind) (str "docKind(" (a (first args)) ")")
      (= op 'document-equal?) (str "docEqual(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-vector-at) (str "docVectorAt(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-map-entry-at) (str "docMapEntryAt(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-vector-assoc) (str "docVectorAssoc(" (a (nth args 0)) "," (a (nth args 1)) "," (a (nth args 2)) ")")
      (= op 'document-vector-conj) (str "docVectorConj(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-vector-drop) (str "docVectorDrop(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-vector-remove) (str "docVectorRemove(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-contains) (str "docContains(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-get) (str "docGet(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-assoc) (str "docAssoc(" (a (nth args 0)) "," (a (nth args 1)) "," (a (nth args 2)) ")")
      (= op 'document-dissoc) (str "docDissoc(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-merge) (str "docMerge(" (a (first args)) "," (a (second args)) ")")
      (= op 'document-string-value) (str "docScalarOption('string'," (a (first args)) ")")
      (= op 'document-keyword-value) (str "docScalarOption('keyword'," (a (first args)) ")")
      (= op 'document-bool-value) (str "docScalarOption('bool'," (a (first args)) ")")
      (= op 'document-i64-value) (str "docScalarOption('i64'," (a (first args)) ")")
      (= op 'document-f64-value) (str "docScalarOption('f64'," (a (first args)) ")")
      (= op 'pair) (str "Object.freeze([" (a (first args)) "," (a (second args)) "])")
      (= op 'pair-first) (str (a (first args)) "[0]")
      (= op 'pair-second) (str (a (first args)) "[1]")
      (= op 'cap-call) (str "callCapability(" (first args) "," (a (second args)) ")")
      (= op 'string-byte-length) (str "BigInt(utf8Bytes(" (a (first args)) "))")
      (= op 'string=?) (str "((" (a (first args)) "===" (a (second args)) ")?1n:0n)")
      (= op 'string-concat) (str "assertString(" (a (first args)) "+" (a (second args)) ")")
      (= op 'string-replace-all) (str "stringReplaceAll(" (a (first args)) ","
                                      (a (second args)) "," (a (nth args 2)) ")")
      (= op 'keyword-from-string) (str "keywordFromString(" (a (first args)) ")")
      (= op 'keyword-name) (str "keywordName(" (a (first args)) ")")
      (= op 'xml-path-count) (str "xmlPathCount(" (a (first args)) "," (a (second args)) ")")
      (= op 'xml-path-attr) (str "xmlPathAttr(" (a (nth args 0)) "," (a (nth args 1)) ","
                                 (a (nth args 2)) "," (a (nth args 3)) ")")
      (= op 'decimal-f64-parse) (str "decimalF64Parse(" (a (first args)) ")")
      (= op 'decimal-f64x3-parse) (str "decimalF64x3Parse(" (a (first args)) ")")
      (= op 'f64-to-bits) (str "f64ToBits(" (a (first args)) ")")
      (= op 'f64-from-bits) (str "f64FromBits(" (a (first args)) ")")
      (= op 'i64-to-f64-checked) (str "i64ToF64Checked(" (a (first args)) ")")
      (= op 'i64-to-f64-rounded) (str "i64ToF64Rounded(" (a (first args)) ")")
      (= op 'f64-to-i64-checked) (str "f64ToI64Checked(" (a (first args)) ")")
      (= op 'f64-to-i64-truncating) (str "f64ToI64Truncating(" (a (first args)) ")")
      (= op 'f32-to-bits) (str "f32ToBits(" (a (first args)) ")")
      (= op 'f32-from-bits) (str "f32FromBits(" (a (first args)) ")")
      (= op 'f64-to-f32-rounded) (str "f64ToF32Rounded(" (a (first args)) ")")
      (= op 'f32-to-f64-exact) (str "f32ToF64Exact(" (a (first args)) ")")
      (= op 'i64-to-f32-checked) (str "i64ToF32Checked(" (a (first args)) ")")
      (= op 'i64-to-f32-rounded) (str "i64ToF32Rounded(" (a (first args)) ")")
      (= op 'f32-to-i64-checked) (str "f32ToI64Checked(" (a (first args)) ")")
      (= op 'f32-to-i64-truncating) (str "f32ToI64Truncating(" (a (first args)) ")")
      (contains? '#{f32-add f32-sub f32-mul f32-div} op)
      (let [operator ({'f32-add "+" 'f32-sub "-" 'f32-mul "*" 'f32-div "/"} op)]
        (str "Math.fround(" (a (first args)) operator (a (second args)) ")"))
      (= op 'f32-neg) (str "Math.fround(-" (a (first args)) ")")
      (= op 'f32-abs) (str "Math.fround(Math.abs(" (a (first args)) "))")
      (= op 'f32-sqrt) (str "Math.fround(Math.sqrt(" (a (first args)) "))")
      (= op 'f32-min) (str "Math.fround(Math.min(" (a (first args)) "," (a (second args)) "))")
      (= op 'f32-max) (str "Math.fround(Math.max(" (a (first args)) "," (a (second args)) "))")
      (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge} op)
      (let [operator ({'f32-eq "===" 'f32-lt "<" 'f32-le "<=" 'f32-gt ">" 'f32-ge ">="} op)]
        (str "(" (a (first args)) operator (a (second args)) ")"))
      (= op 'f32-unordered)
      (str "(Number.isNaN(" (a (first args)) ")||Number.isNaN(" (a (second args)) "))")
      (contains? '#{f64-add f64-sub f64-mul f64-div} op)
      (let [operator ({'f64-add "+" 'f64-sub "-" 'f64-mul "*" 'f64-div "/"} op)]
        (str "assertF64(" (a (first args)) operator (a (second args)) ")"))
      (= op 'f64-neg) (str "assertF64(-" (a (first args)) ")")
      (= op 'f64-abs) (str "Math.abs(" (a (first args)) ")")
      (= op 'f64-sqrt) (str "Math.sqrt(" (a (first args)) ")")
      (= op 'f64-min) (str "Math.min(" (a (first args)) "," (a (second args)) ")")
      (= op 'f64-max) (str "Math.max(" (a (first args)) "," (a (second args)) ")")
      (= op 'f64-sin-quarter-turn) (str "f64SinQuarterTurn(" (a (first args)) ")")
      (= op 'f64-cos-quarter-turn) (str "f64CosQuarterTurn(" (a (first args)) ")")
      (= op 'f64-sin-bounded) (str "f64SinBounded(" (a (first args)) ")")
      (= op 'f64-cos-bounded) (str "f64CosBounded(" (a (first args)) ")")
      (= op 'f64-exp-near-zero) (str "f64ExpNearZero(" (a (first args)) ")")
      (= op 'f64-log-near-one) (str "f64LogNearOne(" (a (first args)) ")")
      (= op 'f64-atan2-bounded) (str "f64Atan2Bounded(" (a (first args)) "," (a (second args)) ")")
      (= op 'f64-exp-bounded) (str "f64ExpBounded(" (a (first args)) ")")
      (= op 'f64-log-bounded) (str "f64LogBounded(" (a (first args)) ")")
      (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge} op)
      (let [operator ({'f64-eq "===" 'f64-lt "<" 'f64-le "<=" 'f64-gt ">" 'f64-ge ">="} op)]
        (str "(" (a (first args)) operator (a (second args)) ")"))
      (= op 'f64-unordered)
      (str "(Number.isNaN(" (a (first args)) ")||Number.isNaN(" (a (second args)) "))")
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
    (instance? Double form) (f64-literal form)
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
                   (.isGetProp node) (.getString node)
                   (and (.isGetElem node)
                        (some-> node .getLastChild .isStringLit))
                   (some-> node .getLastChild .getString)
                   (.isStringKey node) (.getString node))]
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
  Verification is syntax-aware so safe names, comments, and string values do
  not become false positives. Escaped identifiers are normalized by parsing."
  [source]
  (when-not (string? source)
    (fail! "generated output is not text" {}))
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
             ",floatingPointPolicy:'" floating-point-policy "'"
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
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",typedMapLimits:Object.freeze({entries:" max-typed-map-entries "})"))
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",xmlSubsetLimits:Object.freeze({nodes:" max-xml-nodes
                    ",depth:" max-xml-depth ",attributesPerNode:" max-xml-attributes
                    ",pathSegments:" max-xml-path-segments "})"))
             (when (= :kotoba.kir/v4 (:format kir))
               (str ",decimalF64Limits:Object.freeze({bytes:" max-decimal-f64-bytes
                    ",vector3Bytes:" max-decimal-f64x3-bytes
                    ",finiteOnly:true,rounding:'nearest-ties-even'})"))
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
             "const i32Wrap=v=>BigInt.asIntN(32,assertI64(v));"
             "const u32Wrap=v=>BigInt.asUintN(32,assertI64(v));"
             "const i32Add=(a,b)=>BigInt.asIntN(32,i32Wrap(a)+i32Wrap(b));"
             "const i32Mul=(a,b)=>BigInt.asIntN(32,i32Wrap(a)*i32Wrap(b));"
             "const i32Xor=(a,b)=>BigInt.asIntN(32,i32Wrap(a)^i32Wrap(b));"
             "const shift32=s=>{s=assertI64(s);if(s<0n||s>31n)throw new Error('i32-shift-count-out-of-range');return s;};"
             "const i32Shl=(v,s)=>BigInt.asIntN(32,i32Wrap(v)<<shift32(s));"
             "const i32Shr=(v,s)=>i32Wrap(v)>>shift32(s);"
             "const u32Shr=(v,s)=>u32Wrap(v)>>shift32(s);"
             "const xorshift32=v=>{let x=u32Wrap(v);x=BigInt.asUintN(32,x^(x<<13n));"
             "x=BigInt.asUintN(32,x^(x>>17n));return BigInt.asUintN(32,x^(x<<5n));};"
             "const assertF64=v=>{if(typeof v!=='number')throw new Error('invalid-f64');return v;};"
             "const assertF32=v=>{if(typeof v!=='number'||!Object.is(Math.fround(v),v))"
             "throw new Error('invalid-f32');return v;};"
             "const f64Buffer=new ArrayBuffer(8);const f64View=new DataView(f64Buffer);"
             "const f64ToBits=v=>{v=assertF64(v);if(Number.isNaN(v))return 9221120237041090560n;"
             "f64View.setFloat64(0,v,true);return f64View.getBigInt64(0,true);};"
             "const f64FromBits=v=>{v=assertI64(v);f64View.setBigInt64(0,v,true);const n=f64View.getFloat64(0,true);"
             "return Number.isNaN(n)?Number.NaN:n;};"
             "const i64Min=-9223372036854775808n,i64Max=9223372036854775807n;"
             "const i64ToF64Checked=v=>{v=assertI64(v);const n=Number(v);"
             "if(BigInt(n)!==v)throw new Error('inexact-i64-to-f64');return n;};"
             "const i64ToF64Rounded=v=>Number(assertI64(v));"
             "const checkedI64Range=n=>{if(n<i64Min||n>i64Max)throw new Error('f64-to-i64-out-of-range');return n;};"
             "const f64ToI64Checked=v=>{v=assertF64(v);if(!Number.isFinite(v)||!Number.isInteger(v))"
             "throw new Error('inexact-f64-to-i64');return checkedI64Range(BigInt(v));};"
             "const f64ToI64Truncating=v=>{v=assertF64(v);if(!Number.isFinite(v))"
             "throw new Error('invalid-f64-to-i64');return checkedI64Range(BigInt(Math.trunc(v)));};"
             "const assertQuarterTurn=v=>{v=assertF64(v);if(!Number.isFinite(v)||Math.abs(v)>0.7853981633974483)"
             "throw new Error('f64-quarter-turn-domain');return v;};"
             "const f64SinQuarterTurn=v=>{v=assertQuarterTurn(v);if(v===0)return v;const z=v*v;"
             "let p=2.8114572543455206e-15;p=-7.647163731819816e-13+z*p;"
             "p=1.6059043836821613e-10+z*p;p=-2.505210838544172e-8+z*p;"
             "p=2.7557319223985893e-6+z*p;p=-0.0001984126984126984+z*p;"
             "p=0.008333333333333333+z*p;p=-0.16666666666666666+z*p;return v+(v*z)*p;};"
             "const f64CosQuarterTurn=v=>{v=assertQuarterTurn(v);const z=v*v;"
             "let p=4.779477332387385e-14;p=-1.1470745597729725e-11+z*p;"
             "p=2.08767569878681e-9+z*p;p=-2.755731922398589e-7+z*p;"
             "p=0.0000248015873015873+z*p;p=-0.001388888888888889+z*p;"
             "p=0.041666666666666664+z*p;p=-0.5+z*p;return 1+z*p;};"
             "const reduceBoundedAngle=v=>{v=assertF64(v);if(!Number.isFinite(v)||Math.abs(v)>25735.927018207585)"
             "throw new Error('f64-bounded-angle-domain');const s=v*0.6366197723675814;"
             "const n=s>=0?Math.floor(s+0.5):Math.ceil(s-0.5);"
             "const r=(v-n*1.5707963267948966)-n*6.123233995736766e-17;"
             "return [r,((n%4)+4)%4];};"
             "const f64SinBounded=v=>{const [r,q]=reduceBoundedAngle(v);"
             "if(q===0)return f64SinQuarterTurn(r);if(q===1)return f64CosQuarterTurn(r);"
             "if(q===2)return -f64SinQuarterTurn(r);return -f64CosQuarterTurn(r);};"
             "const f64CosBounded=v=>{const [r,q]=reduceBoundedAngle(v);"
             "if(q===0)return f64CosQuarterTurn(r);if(q===1)return -f64SinQuarterTurn(r);"
             "if(q===2)return -f64CosQuarterTurn(r);return f64SinQuarterTurn(r);};"
             "const f64ExpNearZero=v=>{v=assertF64(v);if(!Number.isFinite(v)||Math.abs(v)>0.5)"
             "throw new Error('f64-exp-near-zero-domain');let p=1.5619206968586225e-16;"
             "p=2.8114572543455206e-15+v*p;p=4.779477332387385e-14+v*p;"
             "p=7.647163731819816e-13+v*p;p=1.1470745597729725e-11+v*p;"
             "p=1.6059043836821613e-10+v*p;p=2.08767569878681e-9+v*p;"
             "p=2.505210838544172e-8+v*p;p=2.755731922398589e-7+v*p;"
             "p=2.7557319223985893e-6+v*p;p=0.0000248015873015873+v*p;"
             "p=0.0001984126984126984+v*p;p=0.001388888888888889+v*p;"
             "p=0.008333333333333333+v*p;p=0.041666666666666664+v*p;"
             "p=0.16666666666666666+v*p;p=0.5+v*p;p=1+v*p;return 1+v*p;};"
             "const f64LogNearOne=v=>{v=assertF64(v);if(!Number.isFinite(v)||v<0.75||v>1.5)"
             "throw new Error('f64-log-near-one-domain');const y=(v-1)/(v+1),z=y*y;"
             "let p=0.047619047619047616;p=0.05263157894736842+z*p;"
             "p=0.058823529411764705+z*p;p=0.06666666666666667+z*p;"
             "p=0.07692307692307693+z*p;p=0.09090909090909091+z*p;"
             "p=0.1111111111111111+z*p;p=0.14285714285714285+z*p;"
             "p=0.2+z*p;p=0.3333333333333333+z*p;p=1+z*p;return 2*y*p;};"
             "const f64AtanUnit=v=>{const t=v<=0.4142135623730951?v:(v-1)/(v+1),z=t*t;"
             "let p=-0.02564102564102564;p=0.02702702702702703+z*p;"
             "p=-0.02857142857142857+z*p;p=0.030303030303030304+z*p;"
             "p=-0.03225806451612903+z*p;p=0.034482758620689655+z*p;"
             "p=-0.037037037037037035+z*p;p=0.04+z*p;p=-0.043478260869565216+z*p;"
             "p=0.047619047619047616+z*p;p=-0.05263157894736842+z*p;"
             "p=0.058823529411764705+z*p;p=-0.06666666666666667+z*p;"
             "p=0.07692307692307693+z*p;p=-0.09090909090909091+z*p;"
             "p=0.1111111111111111+z*p;p=-0.14285714285714285+z*p;"
             "p=0.2+z*p;p=-0.3333333333333333+z*p;p=1+z*p;"
             "const a=t*p;return v<=0.4142135623730951?a:0.7853981633974483+a;};"
             "const f64Atan2Bounded=(y,x)=>{y=assertF64(y);x=assertF64(x);"
             "if(!Number.isFinite(y)||!Number.isFinite(x))throw new Error('f64-atan2-bounded-domain');"
             "const yn=y<0||Object.is(y,-0),xn=x<0||Object.is(x,-0);"
             "if(y===0){if(xn)return yn?-3.141592653589793:3.141592653589793;return y;}"
             "if(x===0)return yn?-1.5707963267948966:1.5707963267948966;"
             "const ay=Math.abs(y),ax=Math.abs(x),swap=ay>ax,r=swap?ax/ay:ay/ax;"
             "let a=f64AtanUnit(r);if(swap)a=1.5707963267948966-a;"
             "if(xn)a=3.141592653589793-a;if(yn)a=-a;return a;};"
             "const f64ExpBounded=v=>{v=assertF64(v);if(!Number.isFinite(v)||Math.abs(v)>354.891356446692)"
             "throw new Error('f64-exp-bounded-domain');const s=v*1.4426950408889634;"
             "const n=s>=0?Math.floor(s+0.5):Math.ceil(s-0.5);"
             "const r=(v-n*0.6931471805599453)-n*2.3190468138462996e-17;"
             "const scale=f64FromBits(BigInt(n+1023)*4503599627370496n);"
             "return f64ExpNearZero(r)*scale;};"
             "const f64LogBounded=v=>{v=assertF64(v);if(!Number.isFinite(v)||"
             "v<7.458340731200207e-155||v>1.3407807929942597e154)"
             "throw new Error('f64-log-bounded-domain');const bits=f64ToBits(v);"
             "let e=Number(bits/4503599627370496n)-1023;"
             "let m=f64FromBits((bits&4503599627370495n)+4607182418800017408n);"
             "if(m>1.5){m*=0.5;e+=1;}const k=f64LogNearOne(m);"
             "return (k+e*0.6931471805599453)+e*2.3190468138462996e-17;};"
             "const f32Buffer=new ArrayBuffer(4),f32View=new DataView(f32Buffer);"
             "const f32ToBits=v=>{v=assertF32(v);if(Number.isNaN(v))return 2143289344n;"
             "f32View.setFloat32(0,v,true);return BigInt(f32View.getInt32(0,true));};"
             "const f32FromBits=v=>{v=assertI64(v);if(v<-2147483648n||v>2147483647n)"
             "throw new Error('invalid-f32-bits');f32View.setInt32(0,Number(v),true);"
             "const n=f32View.getFloat32(0,true);return Number.isNaN(n)?Number.NaN:n;};"
             "const f64ToF32Rounded=v=>Math.fround(assertF64(v));"
             "const f32ToF64Exact=v=>assertF32(v);"
             "const i64ToF32Rounded=v=>Math.fround(Number(assertI64(v)));"
             "const i64ToF32Checked=v=>{v=assertI64(v);const n=i64ToF32Rounded(v);"
             "if(BigInt(n)!==v)throw new Error('inexact-i64-to-f32');return n;};"
             "const f32ToI64Checked=v=>{v=assertF32(v);if(!Number.isFinite(v)||!Number.isInteger(v))"
             "throw new Error('inexact-f32-to-i64');return checkedI64Range(BigInt(v));};"
             "const f32ToI64Truncating=v=>{v=assertF32(v);if(!Number.isFinite(v))"
             "throw new Error('invalid-f32-to-i64');return checkedI64Range(BigInt(Math.trunc(v)));};"
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
             ")throw new Error('adt-depth-limit');if(t==='i64')return assertI64(v);if(t==='f32')return assertF32(v);if(t==='f64')return assertF64(v);"
             "if(t==='string')return assertString(v);if(t==='keyword')return assertKeyword(v);"
             "if(t==='map')return assertMap(v);if(t==='bool')return assertBool(v);"
             "if(t==='option-i64')return assertOptionI64(v);if(t==='result-i64')return assertResultI64(v);"
             "if(t==='vector-i64')return assertVectorI64(v);"
             "if(t==='vector-f64')return assertVectorF64(v);"
             "if(t==='string-index')return assertStringIndex(v);"
             "if(t==='disjoint-set-i64')return assertDisjointSetI64(v);"
             "if(t==='doc')return assertDoc(v);"
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
             "if(Array.isArray(t)&&t.length===3&&t[0]==='map'){"
             "if(!Array.isArray(v)||v.length!==2||!sameType(v[0],t)||!Array.isArray(v[1])||v[1].length>"
             max-typed-map-entries ")throw new Error('invalid-typed-map');"
             "const entries=v[1].map(e=>{if(!Array.isArray(e)||e.length!==2)throw new Error('invalid-typed-map-entry');"
             "return Object.freeze([assertTypedValue(t[1],e[0],d+1,s),assertTypedValue(t[2],e[1],d+1,s)]);});"
             "entries.sort((a,b)=>compareTyped(t[1],a[0],b[0]));"
             "for(let i=1;i<entries.length;i++)if(compareTyped(t[1],entries[i-1][0],entries[i][0])===0)"
             "throw new Error('duplicate-map-key');return Object.freeze([t,Object.freeze(entries)]);}"
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
             "if(Array.isArray(t)&&t[0]==='map'){const ai=a[1],bi=b[1],n=Math.min(ai.length,bi.length);"
             "for(let i=0;i<n;i++){let c=compareTyped(t[1],ai[i][0],bi[i][0]);if(c)return c;"
             "c=compareTyped(t[2],ai[i][1],bi[i][1]);if(c)return c;}return cmp(ai.length,bi.length);}"
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
             "const assertTypedMap=(t,v)=>assertTypedValue(t,v,0,{nodes:0});"
             "const makeTypedMap=(t,entries)=>assertTypedMap(t,[t,entries]);"
             "const typedMapIndex=(t,v,key)=>{v=assertTypedMap(t,v);key=assertTypedValue(t[1],key,1,{nodes:0});"
             "return [v,v[1].findIndex(e=>compareTyped(t[1],e[0],key)===0),key];};"
             "const typedMapContains=(t,v,key)=>typedMapIndex(t,v,key)[1]>=0;"
             "const typedMapGet=(t,v,key)=>{const [m,i]=typedMapIndex(t,v,key),ot=Object.freeze(['option',t[2]]);"
             "return makeGenericOption(ot,i>=0,i>=0?m[1][i][1]:null);};"
             "const typedMapEntryAt=(t,v,index)=>{v=assertTypedMap(t,v);index=assertI64(index);"
             "const et=Object.freeze(['vector',Object.freeze([t[1],t[2]])]),ot=Object.freeze(['option',et]);"
             "if(index<0n||index>=BigInt(v[1].length))return makeGenericOption(ot,false,null);"
             "const e=v[1][Number(index)];return makeGenericOption(ot,true,Object.freeze([et,e[0],e[1]]));};"
             "const typedMapAssoc=(t,v,key,item)=>{const [m,i,k]=typedMapIndex(t,v,key);"
             "item=assertTypedValue(t[2],item,1,{nodes:0});if(i<0&&m[1].length>=" max-typed-map-entries
             ")throw new Error('map-too-large');const out=m[1].slice();if(i<0)out.push([k,item]);else out[i]=[k,item];"
             "return makeTypedMap(t,out);};"
             "const typedMapDissoc=(t,v,key)=>{const [m,i]=typedMapIndex(t,v,key);"
             "return i<0?m:makeTypedMap(t,m[1].filter((_,j)=>j!==i));};"
             "const typedMapEqual=(t,a,b)=>{a=assertTypedMap(t,a);b=assertTypedMap(t,b);"
             "return compareTyped(t,a,b)===0;};"
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
             "const makeVectorF64=items=>{if(!Array.isArray(items))throw new Error('invalid-vector-f64');"
             "if(items.length>" max-vector-items ")throw new Error('vector-f64-too-large');"
             "return Object.freeze(items.map(assertF64));};"
             "const assertVectorF64=v=>makeVectorF64(v);"
             "const vectorF64Get=(v,i,fallback)=>{v=assertVectorF64(v);i=assertI64(i);"
             "return i>=0n&&i<BigInt(v.length)?v[Number(i)]:assertF64(fallback());};"
             "const vectorF64At=(v,i)=>{v=assertVectorF64(v);i=assertI64(i);"
             "if(i<0n||i>=BigInt(v.length))throw new Error('vector-f64-index-out-of-range');return v[Number(i)];};"
             "const vectorF64Drop=(v,n)=>{v=assertVectorF64(v);n=assertI64(n);"
             "if(n<0n||n>BigInt(v.length))throw new Error('vector-f64-drop-out-of-range');return makeVectorF64(v.slice(Number(n)));};"
             "const vectorF64Assoc=(v,i,item)=>{v=assertVectorF64(v);i=assertI64(i);"
             "if(i<0n||i>=BigInt(v.length))throw new Error('vector-f64-index-out-of-range');"
             "const out=v.slice();out[Number(i)]=assertF64(item);return makeVectorF64(out);};"
             "const vectorF64Conj=(v,item)=>{v=assertVectorF64(v);if(v.length>=" max-vector-items
             ")throw new Error('vector-f64-too-large');return makeVectorF64([...v,assertF64(item)]);};"
             "const utf8Bytes=s=>{let n=0;for(let i=0;i<s.length;i++){const u=s.charCodeAt(i);"
             "if(u<=127)n++;else if(u<=2047)n+=2;else if(u>=55296&&u<=56319){"
             "if(i+1>=s.length)throw new Error('invalid-utf16');const l=s.charCodeAt(++i);"
             "if(l<56320||l>57343)throw new Error('invalid-utf16');n+=4;}"
             "else if(u>=56320&&u<=57343)throw new Error('invalid-utf16');else n+=3;}return n;};"
             "const assertString=v=>{if(typeof v!=='string')throw new Error('invalid-string');"
             "if(utf8Bytes(v)>" max-string-value-bytes ")throw new Error('string-too-large');return v;};"
             "const stringReplaceAll=(value,needle,replacement)=>{value=assertString(value);"
             "needle=assertString(needle);replacement=assertString(replacement);"
             "if(needle.length===0)throw new Error('empty-string-replacement-needle');"
             "return assertString(value.split(needle).join(replacement));};"
             "const assertStringIndex=v=>{if(!Array.isArray(v)||v.length>" max-compact-graph-items
             ")throw new Error('invalid-string-index');let bytes=0,previous=null;const out=v.map(e=>{"
             "if(!Array.isArray(e)||e.length!==2)throw new Error('invalid-string-index-entry');"
             "const key=assertString(e[0]),item=assertI64(e[1]);bytes+=utf8Bytes(key);"
             "if(previous!==null&&previous>=key)throw new Error('noncanonical-string-index');previous=key;"
             "return Object.freeze([key,item]);});if(bytes>" max-string-index-key-bytes
             ")throw new Error('string-index-key-budget');return Object.freeze(out);};"
             "const makeStringIndex=entries=>assertStringIndex(entries);"
             "const stringIndexPosition=(v,key)=>{v=assertStringIndex(v);key=assertString(key);"
             "return [v,key,v.findIndex(e=>e[0]===key)];};"
             "const stringIndexContains=(v,key)=>stringIndexPosition(v,key)[2]>=0;"
             "const stringIndexGet=(v,key)=>{const [index,k,i]=stringIndexPosition(v,key);"
             "return makeGenericOption(Object.freeze(['option','i64']),i>=0,i>=0?index[i][1]:undefined);};"
             "const stringIndexAssoc=(v,key,item)=>{const [index,k,i]=stringIndexPosition(v,key);item=assertI64(item);"
             "if(i<0&&index.length>=" max-compact-graph-items ")throw new Error('string-index-too-large');"
             "const out=index.slice();if(i<0)out.push(Object.freeze([k,item]));else out[i]=Object.freeze([k,item]);"
             "out.sort((a,b)=>a[0]<b[0]?-1:a[0]>b[0]?1:0);return makeStringIndex(out);};"
             "const assertDisjointSetI64=v=>{if(!Array.isArray(v)||v.length!==2||!Array.isArray(v[0])||"
             "!Array.isArray(v[1])||v[0].length!==v[1].length||v[0].length>" max-compact-graph-items
             ")throw new Error('invalid-disjoint-set-i64');const n=v[0].length;"
             "const parents=v[0].map(p=>{p=assertI64(p);if(p<0n||p>=BigInt(n))throw new Error('disjoint-parent-range');return p;});"
             "const ranks=v[1].map(r=>{r=assertI64(r);if(r<0n||r>BigInt(n))throw new Error('disjoint-rank-range');return r;});"
             "for(let s=0;s<n;s++){let c=s,f=n+1;while(Number(parents[c])!==c){if(--f===0)throw new Error('disjoint-parent-cycle');c=Number(parents[c]);}}"
             "return Object.freeze([Object.freeze(parents),Object.freeze(ranks)]);};"
             "const makeDisjointSetI64=size=>{size=assertI64(size);if(size<0n||size>" max-compact-graph-items
             "n)throw new Error('disjoint-size-range');const n=Number(size);return assertDisjointSetI64(["
             "Array.from({length:n},(_,i)=>BigInt(i)),Array.from({length:n},()=>0n)]);};"
             "const disjointSetI64Union=(v,left,right)=>{v=assertDisjointSetI64(v);left=assertI64(left);right=assertI64(right);"
             "const n=v[0].length;if(left<0n||right<0n||left>=BigInt(n)||right>=BigInt(n))throw new Error('disjoint-index-range');"
             "const root=x=>{let c=Number(x);for(let f=n+1;f>0;f--){const p=Number(v[0][c]);if(p===c)return c;c=p;}throw new Error('disjoint-parent-cycle');};"
             "const a=root(left),b=root(right),t=Object.freeze(['option','disjoint-set-i64']);if(a===b)return makeGenericOption(t,false);"
             "let child=b,parent=a;if(v[1][a]<v[1][b]){child=a;parent=b;}const parents=v[0].slice(),ranks=v[1].slice();"
             "parents[child]=BigInt(parent);if(v[1][a]===v[1][b])ranks[parent]+=1n;"
             "return makeGenericOption(t,true,assertDisjointSetI64([parents,ranks]));};"
             "const docType=Object.freeze(['option','doc']);"
             "const docNull=Object.freeze(['null']);"
             "const assertDoc=v=>{const state={nodes:0,bytes:0,seen:new WeakSet()};"
             "const walk=(x,d)=>{if(d>" max-document-depth ")throw new Error('doc-depth-limit');"
             "if(!Array.isArray(x)||x.length<1||typeof x[0]!=='string')throw new Error('invalid-doc-node');"
             "if(state.seen.has(x))throw new Error('doc-shared-or-cyclic');state.seen.add(x);"
             "if(++state.nodes>" max-document-nodes ")throw new Error('doc-node-limit');const tag=x[0];"
             "if(tag==='null'){if(x.length!==1)throw new Error('invalid-doc-null');return docNull;}"
             "if(tag==='bool'){if(x.length!==2)return (()=>{throw new Error('invalid-doc-bool')})();return Object.freeze([tag,assertBool(x[1])]);}"
             "if(tag==='i64'){if(x.length!==2)throw new Error('invalid-doc-i64');return Object.freeze([tag,assertI64(x[1])]);}"
             "if(tag==='f64'){if(x.length!==2)throw new Error('invalid-doc-f64');const n=assertF64(x[1]);if(!Number.isFinite(n))throw new Error('nonfinite-doc-f64');return Object.freeze([tag,n]);}"
             "if(tag==='string'||tag==='keyword'){if(x.length!==2)throw new Error('invalid-doc-text');"
             "const s=tag==='string'?assertString(x[1]):assertKeyword(x[1]);state.bytes+=utf8Bytes(s);"
             "if(state.bytes>" max-document-utf8-bytes ")throw new Error('doc-utf8-limit');return Object.freeze([tag,s]);}"
             "if(tag==='vector'){if(x.length!==2||!Array.isArray(x[1])||x[1].length>" max-document-container-items
             ")throw new Error('invalid-doc-vector');return Object.freeze([tag,Object.freeze(x[1].map(y=>walk(y,d+1)))]);}"
             "if(tag==='map'){if(x.length!==2||!Array.isArray(x[1])||x[1].length>" max-document-container-items
             ")throw new Error('invalid-doc-map');let previous=null;const entries=x[1].map(e=>{"
             "if(!Array.isArray(e)||e.length!==2)throw new Error('invalid-doc-entry');"
             "if(state.seen.has(e))throw new Error('doc-shared-or-cyclic');state.seen.add(e);"
             "const key=assertKeyword(e[0]);state.bytes+=utf8Bytes(key);if(state.bytes>" max-document-utf8-bytes
             ")throw new Error('doc-utf8-limit');if(previous!==null&&previous>=key)throw new Error('noncanonical-doc-map');"
             "previous=key;return Object.freeze([key,walk(e[1],d+1)]);});return Object.freeze([tag,Object.freeze(entries)]);}"
             "throw new Error('unknown-doc-tag');};return walk(v,0);};"
             "const makeDocScalar=(tag,value)=>assertDoc([tag,value]);"
             "const makeDocVector=items=>assertDoc(['vector',items]);"
             "const makeDocMap=entries=>{if(!Array.isArray(entries)||entries.length>" max-document-container-items
             ")throw new Error('invalid-doc-map');const sorted=entries.map(e=>{if(!Array.isArray(e)||e.length!==2)"
             "throw new Error('invalid-doc-entry');return [assertKeyword(e[0]),e[1]];}).sort((a,b)=>a[0]<b[0]?-1:a[0]>b[0]?1:0);"
             "for(let i=1;i<sorted.length;i++)if(sorted[i-1][0]===sorted[i][0])throw new Error('duplicate-doc-key');"
             "return assertDoc(['map',sorted]);};"
             "const docMapEntries=v=>{v=assertDoc(v);if(v[0]!=='map')throw new Error('doc-map-required');return v[1];};"
             "const docVectorEntries=v=>{v=assertDoc(v);if(v[0]!=='vector')throw new Error('doc-vector-required');return v[1];};"
             "const docCount=v=>{v=assertDoc(v);if(v[0]!=='map'&&v[0]!=='vector')throw new Error('doc-container-required');return BigInt(v[1].length);};"
             "const docKind=v=>assertKeyword(':'+assertDoc(v)[0]);"
             "const docEqual=(a,b)=>{a=assertDoc(a);b=assertDoc(b);const eq=(x,y)=>{if(x[0]!==y[0])return false;const t=x[0];if(t==='null')return true;if(t!=='vector'&&t!=='map')return x[1]===y[1];if(x[1].length!==y[1].length)return false;if(t==='vector'){for(let i=0;i<x[1].length;i++)if(!eq(x[1][i],y[1][i]))return false;return true;}for(let i=0;i<x[1].length;i++)if(x[1][i][0]!==y[1][i][0]||!eq(x[1][i][1],y[1][i][1]))return false;return true;};return eq(a,b);};"
             "const docVectorAt=(v,index)=>{const items=docVectorEntries(v);index=assertI64(index);const ok=index>=0n&&index<BigInt(items.length);return makeGenericOption(docType,ok,ok?items[Number(index)]:undefined);};"
             "const docMapEntryAt=(v,index)=>{const items=docMapEntries(v);index=assertI64(index);"
             "const ok=index>=0n&&index<BigInt(items.length);const entry=ok?items[Number(index)]:null;"
             "return makeGenericOption(docType,ok,ok?makeDocVector([makeDocScalar('keyword',entry[0]),entry[1]]):undefined);};"
             "const docVectorAssoc=(v,index,item)=>{const items=docVectorEntries(v);index=assertI64(index);item=assertDoc(item);if(index<0n||index>=BigInt(items.length))throw new Error('doc-vector-index-out-of-range');const out=[...items];out[Number(index)]=item;return makeDocVector(out);};"
             "const docVectorConj=(v,item)=>{const items=docVectorEntries(v);item=assertDoc(item);if(items.length>=32)throw new Error('doc-vector-too-large');return makeDocVector([...items,item]);};"
             "const docVectorDrop=(v,count)=>{const items=docVectorEntries(v);count=assertI64(count);if(count<0n||count>BigInt(items.length))throw new Error('doc-vector-drop-out-of-range');return makeDocVector(items.slice(Number(count)));};"
             "const docVectorRemove=(v,index)=>{const items=docVectorEntries(v);index=assertI64(index);if(index<0n||index>=BigInt(items.length))throw new Error('doc-vector-index-out-of-range');return makeDocVector(items.filter((_,i)=>i!==Number(index)));};"
             "const docPosition=(v,key)=>{const entries=docMapEntries(v);key=assertKeyword(key);return [entries,key,entries.findIndex(e=>e[0]===key)];};"
             "const docContains=(v,key)=>docPosition(v,key)[2]>=0;"
             "const docGet=(v,key)=>{const [entries,k,i]=docPosition(v,key);return makeGenericOption(docType,i>=0,i>=0?entries[i][1]:undefined);};"
             "const docAssoc=(v,key,item)=>{const [entries,k,i]=docPosition(v,key);item=assertDoc(item);"
             "if(i<0&&entries.length>=" max-document-container-items ")throw new Error('doc-map-too-large');"
             "const out=entries.map(e=>[e[0],e[1]]);if(i<0)out.push([k,item]);else out[i]=[k,item];return makeDocMap(out);};"
             "const docDissoc=(v,key)=>{const [entries,k,i]=docPosition(v,key);return i<0?assertDoc(v):makeDocMap(entries.filter((e,n)=>n!==i));};"
             "const docMerge=(a,b)=>{let out=docMapEntries(a).map(e=>[e[0],e[1]]);for(const e of docMapEntries(b)){const i=out.findIndex(x=>x[0]===e[0]);if(i<0)out.push([e[0],e[1]]);else out[i]=[e[0],e[1]];}return makeDocMap(out);};"
             "const docScalarOption=(tag,v)=>{v=assertDoc(v);const t=Object.freeze(['option',tag]);return makeGenericOption(t,v[0]===tag,v[0]===tag?v[1]:undefined);};"
             "const xmlName=/^[A-Za-z_][A-Za-z0-9_.:-]{0,127}$/u;"
             "const xmlWs=c=>c===' '||c==='\\t'||c==='\\n'||c==='\\r';"
             "const parseBoundedXml=input=>{const s=assertString(input);let i=0,nodes=0;const out=[];"
             "const skip=()=>{while(i<s.length&&xmlWs(s[i]))i++;};"
             "const comment=()=>{if(!s.startsWith('<!--',i))return false;const e=s.indexOf('-->',i+4);"
             "if(e<0||s.slice(i+4,e).includes('--'))throw new Error('invalid-xml-comment');i=e+3;return true;};"
             "const comments=()=>{let found=false;for(;;){skip();if(!comment())return found;found=true;}};"
             "const name=()=>{const b=i;while(i<s.length&&/[A-Za-z0-9_.:-]/u.test(s[i]))i++;"
             "const v=s.slice(b,i);if(!xmlName.test(v))throw new Error('invalid-xml-name');return v;};"
             "const element=(depth,parent)=>{if(depth>" max-xml-depth ")throw new Error('xml-depth-limit');"
             "if(++nodes>" max-xml-nodes ")throw new Error('xml-node-limit');"
             "if(s[i]!=='<'||s[i+1]==='/'||s[i+1]==='!'||s[i+1]==='?')throw new Error('invalid-xml-element');i++;"
             "const tag=name(),path=parent?parent+'/'+tag:tag,attrs=Object.create(null);let ac=0,emptyElement=false;"
             "for(;;){skip();if(s.startsWith('/>',i)){i+=2;emptyElement=true;break;}if(s[i]==='>'){i++;break;}"
             "const k=name();if(Object.prototype.hasOwnProperty.call(attrs,k))throw new Error('duplicate-xml-attribute');"
             "if(++ac>" max-xml-attributes ")throw new Error('xml-attribute-limit');skip();"
             "if(s[i++]!=='=')throw new Error('invalid-xml-attribute');skip();const q=s[i++];"
             "if(q.charCodeAt(0)!==34&&q.charCodeAt(0)!==39)throw new Error('invalid-xml-attribute');const b=i;"
             "while(i<s.length&&s[i]!==q){if(s[i]==='<'||s[i]==='&')throw new Error('xml-entity-or-markup-rejected');i++;}"
             "if(i>=s.length)throw new Error('unterminated-xml-attribute');attrs[k]=assertString(s.slice(b,i++));}"
             "out.push(Object.freeze({path,attrs:Object.freeze(attrs)}));if(emptyElement)return;"
             "for(;;){comments();skip();if(s.startsWith('</',i)){i+=2;const close=name();skip();"
             "if(close!==tag||s[i++]!=='>')throw new Error('xml-close-mismatch');return;}"
             "if(s[i]==='<'){element(depth+1,path);continue;}throw new Error('xml-text-rejected');}};"
             "comments();if(s.startsWith('<?xml',i)){const e=s.indexOf('?>',i+5);if(e<0)throw new Error('invalid-xml-declaration');"
             "const d=s.slice(i,e+2);if(!/^<\\?xml\\s+version=(?:\"1\\.[01]\"|'1\\.[01]')(?:\\s+encoding=(?:\"(?:UTF-8|utf-8)\"|'(?:UTF-8|utf-8)'))?\\s*\\?>$/u.test(d))"
             "throw new Error('invalid-xml-declaration');i=e+2;}comments();element(1,'');comments();skip();"
             "if(i!==s.length)throw new Error('xml-trailing-content');return Object.freeze(out);};"
             "const xmlPath=path=>{path=assertString(path);const p=path.split('/');"
             "if(p.length<1||p.length>" max-xml-path-segments "||p.some(x=>!xmlName.test(x)))throw new Error('invalid-xml-path');return path;};"
             "const xmlPathCount=(xml,path)=>{const p=xmlPath(path);return BigInt(parseBoundedXml(xml).filter(n=>n.path===p).length);};"
             "const xmlStringOption=Object.freeze(['option','string']);"
             "const xmlPathAttr=(xml,path,index,attr)=>{const p=xmlPath(path);attr=assertString(attr);"
             "if(!xmlName.test(attr))throw new Error('invalid-xml-name');index=assertI64(index);"
             "if(index<0n)throw new Error('xml-index-out-of-range');const xs=parseBoundedXml(xml).filter(n=>n.path===p);"
             "if(index>=BigInt(xs.length))return makeGenericOption(xmlStringOption,false,null);"
             "const a=xs[Number(index)].attrs;return Object.prototype.hasOwnProperty.call(a,attr)"
             "?makeGenericOption(xmlStringOption,true,a[attr]):makeGenericOption(xmlStringOption,false,null);};"
             "const decimalF64Pattern=/^[+-]?(?:(?:[0-9]+(?:\\.[0-9]*)?)|(?:\\.[0-9]+))(?:[eE][+-]?[0-9]{1,3})?$/u;"
             "const decimalF64Option=Object.freeze(['option','f64']);"
             "const decimalF64Value=value=>{if(utf8Bytes(value)>" max-decimal-f64-bytes
             "||!decimalF64Pattern.test(value))return null;const parsed=Number(value);return Number.isFinite(parsed)?parsed:null;};"
             "const decimalF64Parse=value=>{value=assertString(value);const parsed=decimalF64Value(value);"
             "return parsed!==null?makeGenericOption(decimalF64Option,true,parsed)"
             ":makeGenericOption(decimalF64Option,false,null);};"
             "const decimalF64x3Vector=Object.freeze(['vector',Object.freeze(['f64','f64','f64'])]);"
             "const decimalF64x3Option=Object.freeze(['option',decimalF64x3Vector]);"
             "const decimalF64x3Parse=value=>{value=assertString(value);if(utf8Bytes(value)>" max-decimal-f64x3-bytes
             "||!/^[0-9eE+.\\- \\t\\r\\n]+$/u.test(value))return makeGenericOption(decimalF64x3Option,false,null);"
             "const stripped=value.replace(/^[ \\t\\r\\n]+|[ \\t\\r\\n]+$/gu,'');"
             "const parts=stripped===''?[]:stripped.split(/[ \\t\\r\\n]+/u);if(parts.length!==3)"
             "return makeGenericOption(decimalF64x3Option,false,null);const items=parts.map(decimalF64Value);"
             "return items.every(item=>item!==null)?makeGenericOption(decimalF64x3Option,true,makeHeterogeneousVector(decimalF64x3Vector,items))"
             ":makeGenericOption(decimalF64x3Option,false,null);};"
             "const assertKeyword=v=>{if(typeof v!=='string'||v.length<2||v[0]!==':'||"
             "v.includes('::')||/\\s|[\\[\\]{}()\"',;`~^\\\\]/u.test(v))"
             "throw new Error('invalid-keyword');if(utf8Bytes(v)>" max-keyword-bytes
             ")throw new Error('keyword-too-large');return v;};"
             "const keywordFromString=v=>{v=assertString(v);if(v.length===0||v[0]===':'||/\\s/u.test(v))"
             "throw new Error('invalid-keyword-source');return assertKeyword(':'+v);};"
             "const keywordName=v=>{v=assertKeyword(v).slice(1);const i=v.lastIndexOf('/');"
             "return assertString(i<0?v:v.slice(i+1));};"
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
             "let fuel=512;"
             "const charge=()=>{fuel--;if(fuel<0)throw new Error('fuel-exhausted');};"
             "const quot=(a,b)=>{if(b===0n)throw new Error('division-by-zero');"
             "if(a===-9223372036854775808n&&b===-1n)throw new Error('signed-division-overflow');return a/b;};"
             "const callCapability=(id,value)=>{const f=grants[id];"
             "if(typeof f!=='function')throw new Error('capability-denied:'+id);return i64(BigInt(f(value)));};\n"
             function-source "\n"
             "return Object.freeze({" (str/join "," (map (fn [f] (str "'" f "':" (js-name f))) exports)) "});\n}\n")]
    (verify-output! source))))
