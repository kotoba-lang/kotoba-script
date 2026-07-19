(ns kotoba.script-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [clojure.java.shell :as shell]
            [kotoba.script :as script])
  (:gen-class))

(def kir
  {:format :kotoba.kir/v3 :entry 'main :effects #{}
   :functions [{:name 'add1 :params ['x] :body '(+ x 1)}
               {:name 'main :params [] :body '(add1 41)}]})

(deftest emits-and-executes-restricted-esm
  (let [source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.main()!==42n)process.exit(2);console.log('42')})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (= "42\n" (:out result)))
    (is (not (re-find #"globalThis|window|document|eval" source)))))

(deftest capabilities-fail-closed
  (let [source (script/emit {:format :kotoba.kir/v3 :entry 'main
                             :effects #{[:cap/call 7]}
                             :functions [{:name 'main :params [] :body '(cap-call 7 4)}]})]
    (is (re-find #"requiredCapabilities:Object.freeze\(\[7\]\)" source))
    (is (re-find #"capability-denied" source))))

(deftest equality-is-comparison-and-grants-are-exact
  (let [source (script/emit {:format :kotoba.kir/v3 :entry 'main :effects #{}
                             :functions [{:name 'main :params [] :body '(= 1 2)}]})]
    (is (re-find #"1n === 2n" source))
    (is (not (re-find #"1n = 2n" source)))
    (is (re-find #"capability-grant-mismatch" source))))

(deftest rejects-unchecked-or-unknown-ir
  (is (thrown? clojure.lang.ExceptionInfo (script/emit {:format :unknown})))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/emit {:format :kotoba.kir/v3 :entry 'main :effects #{}
                             :functions [{:name 'main :params [] :body '(fetch 1)}]}))))

(deftest ast-verifier-rejects-obfuscated-ambient-authority
  ;; The source spelling avoids the defense-in-depth regex; parsing must still
  ;; normalize it to the forbidden `globalThis` identifier.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"AST violates restricted subset"
                        (script/verify-output!
                         "export const x=global\\u0054his.fetch;")))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/verify-output! "export const x=({}).constructor;")))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/verify-output! "export const x=import('x');"))))

(deftest typed-exports-validate-values-and-preserve-bounded-strings
  (let [source (script/emit
                {:format :kotoba.kir/v4 :entry nil :exports ['greet 'bytes]
                 :effects #{}
                 :functions
                 [{:name 'greet :params ['name] :param-types [:string] :result :string
                   :body '(string-concat "こんにちは、" name)}
                  {:name 'bytes :params ['value] :param-types [:string] :result :i64
                   :body '(string-byte-length value)}]})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.greet('世界')!=='こんにちは、世界')process.exit(2);"
                "if(x.bytes('😀')!==4n)process.exit(3);"
                "for(const bad of [()=>x.greet(1n),()=>x.bytes('\\uD800'),()=>x.greet('x'.repeat(65537))]){"
                "try{bad();process.exit(4)}catch(e){}}console.log('typed-ok')})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (= "typed-ok\n" (:out result)))
    (is (re-find #"validateString" source))
    (is (re-find #"validateI64" source))))

(deftest typed-f64-exports-preserve-ieee-bits
  (let [source (script/emit
                {:format :kotoba.kir/v4 :entry nil :exports ['bits 'from-bits 'negative-zero]
                 :effects #{}
                 :functions
                 [{:name 'bits :params ['value] :param-types [:f64] :result :i64
                   :body '(f64-to-bits value)}
                  {:name 'from-bits :params ['value] :param-types [:i64] :result :f64
                   :body '(f64-from-bits value)}
                  {:name 'negative-zero :params [] :param-types [] :result :f64
                   :body -0.0}]})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.bits(1.5)!==4609434218613702656n)process.exit(2);"
                "if(!Object.is(x.fromBits?x.fromBits(0n):x['from-bits'](0n),0))process.exit(3);"
                "if(!Object.is(x['negative-zero'](),-0))process.exit(4);"
                "if(x.bits(Number.NaN)!==9221120237041090560n)process.exit(5);"
                "try{x.bits(1n);process.exit(6)}catch(e){}console.log('f64-ok')})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (= "f64-ok\n" (:out result)))
    (is (re-find #"f64ToBits" source))
    (is (re-find #"validateF64" source))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'kotoba.script-test)]
    (System/exit (if (pos? (+ fail error)) 1 0))))
