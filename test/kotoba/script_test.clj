(ns kotoba.script-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
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

(deftest module-graph-identity-is-frozen-into-the-esm-artifact
  (let [a (apply str (repeat 64 "a"))
        b (apply str (repeat 64 "b"))
        c (apply str (repeat 64 "c"))
        source (script/emit kir {:module-graph-digest a
                                 :module-source-digests
                                 {'example.text b 'example.app c}})]
    (is (str/includes? source (str "moduleGraphDigest:\"" a "\"")))
    (is (re-find #"moduleSourceDigests:Object.freeze\(\{\"example.app\":" source))
    (is (< (.indexOf source "\"example.app\"") (.indexOf source "\"example.text\"")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"supplied together"
                          (script/emit kir {:module-graph-digest a})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"canonical SHA-256"
                          (script/emit kir {:module-graph-digest "bad"
                                            :module-source-digests {'example.app c}})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"source digests are invalid"
                          (script/emit kir {:module-graph-digest a
                                            :module-source-digests {"example.app" c}})))))

(deftest supply-chain-identity-is-frozen-into-the-esm-artifact
  (let [a (apply str (repeat 64 "a"))
        b (apply str (repeat 64 "b"))
        c (apply str (repeat 64 "c"))
        source (script/emit kir {:package-lock-digest a
                                 :trust-policy-digest b
                                 :package-receipt-digest c})]
    (is (str/includes? source (str "packageLockDigest:\"" a "\"")))
    (is (str/includes? source (str "trustPolicyDigest:\"" b "\"")))
    (is (str/includes? source (str "packageReceiptDigest:\"" c "\"")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be supplied together"
                          (script/emit kir {:package-lock-digest a})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"canonical SHA-256"
                          (script/emit kir {:package-lock-digest a
                                            :trust-policy-digest "bad"
                                            :package-receipt-digest c})))))

(deftest equality-is-comparison-and-grants-are-exact
  (let [source (script/emit {:format :kotoba.kir/v3 :entry 'main :effects #{}
                             :functions [{:name 'main :params [] :body '(= 1 2)}]})]
    (is (re-find #"valueEqual\(1n,2n\)" source))
    (is (not (re-find #"1n = 2n" source)))
    (is (re-find #"capability-grant-mismatch" source))))

(deftest explicit-exports-hide-internal-functions
  (let [source (script/emit (assoc kir :exports ['main]))
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.main()!==42n||Object.hasOwn(x,'add1'))process.exit(2)})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (not (re-find #"'add1':k\$add1" source))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exports are invalid"
                        (script/emit (assoc kir :exports ['missing])))))

(deftest library-module-needs-exports-but-not-an-entry
  (let [source (script/emit {:format :kotoba.kir/v3 :entry nil :exports ['add1]
                             :effects #{}
                             :functions [{:name 'add1 :params ['x] :body '(+ x 1)}]})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(m.kotobaArtifact.entry!==null||x.add1(41n)!==42n)process.exit(2)})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (re-find #"entry:null" source)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exports are invalid"
                        (script/emit {:format :kotoba.kir/v3 :entry nil :exports []
                                      :effects #{} :functions []}))))

(deftest typed-bounded-strings-preserve-values-through-the-frozen-api
  (let [typed-kir {:format :kotoba.kir/v4 :entry nil :exports ['greet 'byte-length]
                   :effects #{}
                   :functions [{:name 'greet :params ['name] :param-types [:string]
                                :result :string :effects #{}
                                :body '(string-concat "こんにちは、" name)}
                               {:name 'byte-length :params ['value] :param-types [:string]
                                :result :i64 :effects #{}
                                :body '(string-byte-length value)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.greet('言葉')!=='こんにちは、言葉')process.exit(2);"
                "if(x['byte-length']('言葉')!==6n)process.exit(3);"
                "try{x.greet(1n);process.exit(4)}catch(e){if(e.message!=='invalid-string')process.exit(5)}"
                "try{x.greet('x'.repeat(65536));process.exit(6)}catch(e){if(e.message!=='string-too-large')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (re-find #"kirFormat:'v4'" source))
    (is (re-find #"valueProfile:'typed-v1'" source))
    (is (re-find #"stringLimits:Object.freeze\(\{literalBytes:4096,moduleLiteralBytes:65536,valueBytes:65536\}\)"
                 source))
    (is (re-find #"invalid-utf16" source)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"type mismatch"
                        (script/emit {:format :kotoba.kir/v4 :entry nil :exports ['bad]
                                      :effects #{}
                                      :functions [{:name 'bad :params [] :param-types []
                                                   :result :string :effects #{} :body 1}]})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeds byte limit"
                        (script/emit {:format :kotoba.kir/v4 :entry nil :exports ['large]
                                      :effects #{}
                                      :functions [{:name 'large :params [] :param-types []
                                                   :result :string :effects #{}
                                                   :body (apply str (repeat 4097 "x"))}]})))
  (let [unpaired (String. (char-array [(char 0xd800)]))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unpaired high surrogate"
                          (script/emit {:format :kotoba.kir/v4 :entry nil :exports ['bad]
                                        :effects #{}
                                        :functions [{:name 'bad :params [] :param-types []
                                                     :result :string :effects #{} :body unpaired}]})))))

(deftest typed-keywords-preserve-canonical-text-without-hashing
  (let [typed-kir {:format :kotoba.kir/v4 :entry nil :exports ['identity 'same?]
                   :effects #{}
                   :functions [{:name 'identity :params ['value] :param-types [:keyword]
                                :result :keyword :effects #{} :body 'value}
                               {:name 'same? :params ['left 'right]
                                :param-types [:keyword :keyword]
                                :result :i64 :effects #{} :body '(= left right)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.identity(':安全/確認')!==':安全/確認')process.exit(2);"
                "if(x['same?'](':a',':a')!==1n||x['same?'](':a',':b')!==0n)process.exit(3);"
                "try{x.identity('not-a-keyword');process.exit(4)}"
                "catch(e){if(e.message!=='invalid-keyword')process.exit(5)}"
                "try{x.identity(1n);process.exit(6)}"
                "catch(e){if(e.message!=='invalid-keyword')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "assertKeyword"))
    (is (str/includes? source "keywordLimits:Object.freeze({valueBytes:512})")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different types"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params [] :param-types []
                                       :result :i64 :effects #{} :body '(= :a 1)}]}))))

(deftest typed-bounded-maps-use-canonical-persistent-keyword-entries
  (let [typed-kir {:format :kotoba.kir/v4 :entry nil :exports ['lookup 'update]
                   :effects #{}
                   :functions [{:name 'lookup :params ['value] :param-types [:map]
                                :result :i64 :effects #{} :body '(map-get value :a 0)}
                               {:name 'update :params ['value] :param-types [:map]
                                :result :map :effects #{}
                                :body '(map-assoc value :b 2 :a 3)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "const before=[[':a',1n]];const after=x.update(before);"
                "if(x.lookup(before)!==1n||x.lookup(after)!==3n)process.exit(2);"
                "if(before[0][1]!==1n||after.length!==2||after[0][0]!==':a'||after[1][0]!==':b')process.exit(3);"
                "try{x.lookup([[':a',1n],[':a',2n]]);process.exit(4)}"
                "catch(e){if(e.message!=='duplicate-map-key')process.exit(5)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "mapLimits:Object.freeze({entries:128})"))
    (is (str/includes? source "const makeMap="))))

(deftest typed-booleans-and-options-never-use-js-truthiness-or-null-sentinels
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['negate 'present? 'with-default 'same-option?]
         :effects #{}
         :functions
         [{:name 'negate :params ['value] :param-types [:bool]
           :result :bool :effects #{} :body '(bool-not value)}
          {:name 'present? :params ['value] :param-types [:option-i64]
           :result :bool :effects #{} :body '(option-some? value)}
          {:name 'with-default :params ['value] :param-types [:option-i64]
           :result :i64 :effects #{} :body '(option-value value 9)}
          {:name 'same-option? :params ['left 'right]
           :param-types [:option-i64 :option-i64]
           :result :i64 :effects #{} :body '(= left right)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "const none=Object.freeze([false]);const some=Object.freeze([true,7n]);"
                "if(x.negate(true)!==false||x['present?'](none)!==false||x['present?'](some)!==true)process.exit(2);"
                "if(x['with-default'](none)!==9n||x['with-default'](some)!==7n)process.exit(3);"
                "if(x['same-option?'](none,[false])!==1n||x['same-option?'](some,[true,7n])!==1n)process.exit(4);"
                "for(const bad of [null,undefined,0n,[false,1n],[true]]){try{x['present?'](bad);process.exit(5)}"
                "catch(e){if(e.message!=='invalid-option-i64')process.exit(6)}}"
                "try{x.negate(0n);process.exit(7)}catch(e){if(e.message!=='invalid-bool')process.exit(8)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const optionNone=Object.freeze([false])"))
    (is (str/includes? source "booleanProfile:'strict-v1'"))
    (is (str/includes? source "optionProfile:'tagged-i64-v1'"))
    (is (not (str/includes? source "undefined?"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different types"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params [] :param-types []
                                       :result :i64 :effects #{}
                                       :body '(= (option-none) false)}]}))))

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
