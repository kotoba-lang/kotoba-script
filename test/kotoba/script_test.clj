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

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'kotoba.script-test)]
    (System/exit (if (pos? (+ fail error)) 1 0))))
