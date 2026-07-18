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

(deftest bounded-result-i64-has-closed-tags-payloads-and-lazy-fallbacks
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['ok? 'value 'error 'same?]
         :effects #{}
         :functions
         [{:name 'ok? :params ['result] :param-types [:result-i64]
           :result :bool :effects #{} :body '(result-ok? result)}
          {:name 'value :params ['result 'fallback] :param-types [:result-i64 :i64]
           :result :i64 :effects #{} :body '(result-value result fallback)}
          {:name 'error :params ['result 'fallback] :param-types [:result-i64 :i64]
           :result :i64 :effects #{} :body '(result-error result fallback)}
          {:name 'same? :params ['left 'right] :param-types [:result-i64 :result-i64]
           :result :i64 :effects #{} :body '(= left right)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});const ok=[true,7n],err=[false,12n];"
                "if(x['ok?'](ok)!==true||x['ok?'](err)!==false)process.exit(2);"
                "if(x.value(ok,99n)!==7n||x.value(err,99n)!==99n)process.exit(3);"
                "if(x.error(err,99n)!==12n||x.error(ok,99n)!==99n)process.exit(4);"
                "if(x['same?'](ok,[true,7n])!==1n||x['same?'](err,[false,13n])!==0n)process.exit(5);"
                "for(const bad of [null,undefined,[true],[false],['ok',1n],[true,1]]){try{x['ok?'](bad);process.exit(6)}"
                "catch(e){if(e.message!=='invalid-result-i64')process.exit(7)}}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "resultProfile:'tagged-i64-i64-v1'"))
    (is (str/includes? source "const assertResultI64="))))

(deftest parametric-result-validates-nested-payload-types-and-budgets
  (let [text-result [:result :string :i64]
        nested-result [:result :string [:result :i64 :bool]]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['text 'nested 'nested-error?]
             :effects #{}
             :functions
             [{:name 'text :params ['r] :param-types [text-result]
               :result :string :effects #{}
               :body (list 'result-value-of text-result 'r "fallback")}
              {:name 'nested :params [] :param-types [] :result nested-result :effects #{}
               :body (list 'result-err-of nested-result
                           (list 'result-ok-of [:result :i64 :bool] 7))}
              {:name 'nested-error? :params ['r] :param-types [nested-result]
               :result :bool :effects #{}
               :body (list 'result-ok?-of [:result :i64 :bool]
                           (list 'result-error-of nested-result 'r
                                 (list 'result-err-of [:result :i64 :bool] false)))}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.text([true,'安全'])!=='安全'||x.text([false,9n])!=='fallback')process.exit(2);"
                "const n=x.nested();if(n[0]!==false||n[1][0]!==true||n[1][1]!==7n||!Object.isFrozen(n[1]))process.exit(3);"
                "if(x['nested-error?'](n)!==true)process.exit(4);"
                "for(const bad of [[true,9n],[false,[true,'wrong']],[false,[true,7]]]){try{x['nested-error?'](bad);process.exit(5)}"
                "catch(e){if(!['invalid-string','invalid-i64'].includes(e.message))process.exit(6)}}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "parametricAdtLimits:Object.freeze({depth:8,nodes:64,variantCases:32})")))
  (let [too-deep (nth (iterate (fn [t] [:result :i64 t]) :bool) 9)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"depth limit"
                          (script/emit
                           {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                            :functions [{:name 'bad :params ['x] :param-types [too-deep]
                                         :result :bool :effects #{} :body true}]})))))

(deftest result-match-is-exhaustive-typed-and-lazy
  (let [type [:result :string :i64]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['describe] :effects #{}
             :functions
             [{:name 'describe :params ['r] :param-types [type]
               :result :string :effects #{}
               :body (list 'result-match-of type 'r 'text 'text 'code 'code)}]}
        ;; Use an i64-returning match for executable behavior; the string body
        ;; above separately proves binder typing without coercing error codes.
        executable (assoc-in kir [:functions 0]
                             {:name 'describe :params ['r] :param-types [type]
                              :result :i64 :effects #{}
                              :body (list 'result-match-of type 'r 'text
                                          '(string-byte-length text) 'code 'code)})
        source (script/emit executable)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.describe([true,'安全'])!==6n||x.describe([false,17n])!==17n)process.exit(2)})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const parametricResultMatch="))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different types"
                          (script/emit kir)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"binders"
                          (script/emit
                           (assoc-in executable [:functions 0 :body]
                                     (list 'result-match-of type 'r :bad 1 'code 'code)))))))

(deftest closed-variants-own-identity-payloads-and-exhaustive-matches
  (let [type [:variant :demo/status [[:ready :i64] [:failed :string]]]
        branches [[:ready 'n '(+ n 1)] [:failed 'message '(string-byte-length message)]]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['ready 'describe] :effects #{}
             :functions
             [{:name 'ready :params [] :param-types [] :result type :effects #{}
               :body (list 'variant-new type :ready 7)}
              {:name 'describe :params ['value] :param-types [type] :result :i64 :effects #{}
               :body (list 'variant-match type 'value branches)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "const t=Object.freeze(['variant',':demo/status',Object.freeze([Object.freeze([':ready','i64']),Object.freeze([':failed','string'])])]);"
                "const r=x.ready();if(r[1]!==':ready'||r[2]!==7n||!Object.isFrozen(r))process.exit(2);"
                "if(x.describe([t,':ready',9n])!==10n||x.describe([t,':failed','安全'])!==6n)process.exit(3);"
                "try{x.describe([t,':unknown',1n]);process.exit(4)}catch(e){if(e.message!=='unknown-variant-case')process.exit(5)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "variantCases:32"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exactly cover"
                          (script/emit (assoc-in kir [:functions 1 :body]
                                                 (list 'variant-match type 'value (vec (reverse branches)))))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"expression type mismatch"
                          (script/emit (assoc-in kir [:functions 0 :body]
                                                 (list 'variant-new type :ready "wrong"))))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"qualified keyword"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params [] :param-types []
                                       :result [:variant :status [[:ready :i64]]]
                                       :effects #{} :body 0}]}))))

(deftest generic-options-preserve-none-type-identity-and-exhaustive-matching
  (let [type [:option :string]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['some 'none 'describe] :effects #{}
             :functions
             [{:name 'some :params [] :param-types [] :result type :effects #{}
               :body (list 'option-some-of type "安全")}
              {:name 'none :params [] :param-types [] :result type :effects #{}
               :body (list 'option-none-of type)}
              {:name 'describe :params ['value] :param-types [type] :result :i64 :effects #{}
               :body (list 'option-match type 'value 7 'text '(string-byte-length text))}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),t=Object.freeze(['option','string']);"
                "const s=x.some(),n=x.none();if(s[1]!==true||s[2]!=='安全'||n[1]!==false)process.exit(2);"
                "if(x.describe([t,true,'安全'])!==6n||x.describe([t,false])!==7n)process.exit(3);"
                "try{x.describe([Object.freeze(['option','i64']),false]);process.exit(4)}"
                "catch(e){if(e.message!=='invalid-generic-option')process.exit(5)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "genericOptionProfile:'typed-tagged-v1'"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different types"
                          (script/emit
                          (assoc-in kir [:functions 2 :body]
                                     (list 'option-match type 'value 7 'text 'text)))))))

(deftest heterogeneous-vectors-seal-position-types-and-exact-length
  (let [type [:vector [:i64 :string :bool]]
        kir {:format :kotoba.kir/v4 :entry nil
             :exports ['make 'name 'rename 'count-items 'same?] :effects #{}
             :functions
             [{:name 'make :params [] :param-types [] :result type :effects #{}
               :body (list 'hetero-vector-new type 7 "安全" true)}
              {:name 'name :params ['value] :param-types [type] :result :string :effects #{}
               :body (list 'hetero-vector-at type 'value 1)}
              {:name 'rename :params ['value] :param-types [type] :result type :effects #{}
               :body (list 'hetero-vector-assoc type 'value 1 "確認")}
              {:name 'count-items :params ['value] :param-types [type] :result :i64 :effects #{}
               :body (list 'hetero-vector-count type 'value)}
              {:name 'same? :params ['left 'right] :param-types [type type]
               :result :i64 :effects #{}
               :body (list 'hetero-vector-equal type 'left 'right)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "const t=Object.freeze(['vector',Object.freeze(['i64','string','bool'])]);"
                "const before=x.make(),after=x.rename(before);"
                "if(before[1]!==7n||before[2]!=='安全'||before[3]!==true||x.name(before)!=='安全')process.exit(2);"
                "if(after[2]!=='確認'||before[2]!=='安全'||x['count-items'](before)!==3n||!Object.isFrozen(after))process.exit(3);"
                "if(x['same?'](before,[t,7n,'安全',true])!==1n||x['same?'](before,after)!==0n)process.exit(8);"
                "try{x.name([Object.freeze(['vector',Object.freeze(['string','string','bool'])]),7n,'安全',true]);process.exit(4)}"
                "catch(e){if(e.message!=='invalid-heterogeneous-vector')process.exit(5)}"
                "try{x.name([t,7n,'安全']);process.exit(6)}catch(e){if(e.message!=='invalid-heterogeneous-vector')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "heterogeneousVectorLimits:Object.freeze({items:32})"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exactly match"
                          (script/emit (assoc-in kir [:functions 0 :body]
                                                 (list 'hetero-vector-new type 7 "安全")))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"in-range integer literal"
                          (script/emit (assoc-in kir [:functions 1 :body]
                                                 (list 'hetero-vector-at type 'value 3))))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"bounded vector"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params [] :param-types []
                                       :result [:vector (vec (repeat 33 :i64))]
                                       :effects #{} :body 0}]}))))

(deftest typed-sets-have-canonical-order-unique-items-and-persistent-updates
  (let [type [:set :i64]
        option-type [:option :string]
        nested-type [:set [:option :string]]
        kir {:format :kotoba.kir/v4 :entry nil
             :exports ['make 'nested 'duplicate 'contains? 'add 'remove 'same?] :effects #{}
             :functions
             [{:name 'make :params [] :param-types [] :result type :effects #{}
               :body (list 'typed-set-new type 3 1 2)}
              {:name 'nested :params [] :param-types [] :result nested-type :effects #{}
               :body (list 'typed-set-new nested-type
                           (list 'option-some-of option-type "b")
                           (list 'option-none-of option-type)
                           (list 'option-some-of option-type "a"))}
              {:name 'duplicate :params [] :param-types [] :result type :effects #{}
               :body (list 'typed-set-new type 1 1)}
              {:name 'contains? :params ['value 'item] :param-types [type :i64]
               :result :bool :effects #{}
               :body (list 'typed-set-contains type 'value 'item)}
              {:name 'add :params ['value 'item] :param-types [type :i64]
               :result type :effects #{}
               :body (list 'typed-set-conj type 'value 'item)}
              {:name 'remove :params ['value 'item] :param-types [type :i64]
               :result type :effects #{}
               :body (list 'typed-set-disj type 'value 'item)}
              {:name 'same? :params ['left 'right] :param-types [type type]
               :result :i64 :effects #{}
               :body (list 'typed-set-equal type 'left 'right)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),t=Object.freeze(['set','i64']);"
                "const a=x.make(),b=x.add(a,4n),c=x.remove(b,2n);"
                "if(a[1].join(',')!=='1,2,3'||!x['contains?'](a,2n)||x['contains?'](a,9n))process.exit(2);"
                "if(a[1].length!==3||b[1].join(',')!=='1,2,3,4'||c[1].join(',')!=='1,3,4'||!Object.isFrozen(c[1]))process.exit(3);"
                "if(x['same?'](a,[t,[3n,2n,1n]])!==1n||x['same?'](a,b)!==0n)process.exit(4);"
                "const n=x.nested();if(n[1].length!==3||n[1][0][1]!==false||n[1][1][2]!=='a'||n[1][2][2]!=='b')process.exit(9);"
                "try{x.duplicate();process.exit(5)}catch(e){if(e.message!=='duplicate-set-item')process.exit(6)}"
                "try{x['contains?']([Object.freeze(['set','string']),['1']],1n);process.exit(7)}"
                "catch(e){if(e.message!=='invalid-typed-set')process.exit(8)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "typedSetLimits:Object.freeze({items:32})"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"item limit"
                          (script/emit
                           (assoc-in kir [:functions 0 :body]
                                     (list* 'typed-set-new type (range 33))))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"expression type mismatch"
                          (script/emit
                           (assoc-in kir [:functions 0 :body]
                                     (list 'typed-set-new type "wrong")))))))

(deftest bounded-records-seal-schema-field-order-and-persistent-updates
  (let [type [:record :demo/person [[:name :string] [:age :i64]
                                     [:nickname [:option :string]]]]
        other-type [:record :demo/account [[:name :string] [:age :i64]
                                            [:nickname [:option :string]]]]
        option-type [:option :string]
        kir {:format :kotoba.kir/v4 :entry nil
             :exports ['make 'name-of 'birthday 'same?] :effects #{}
             :functions
             [{:name 'make :params [] :param-types [] :result type :effects #{}
               :body (list 'record-new type "Kotoba" 7
                           (list 'option-none-of option-type))}
              {:name 'name-of :params ['value] :param-types [type]
               :result :string :effects #{}
               :body (list 'record-get type 'value :name)}
              {:name 'birthday :params ['value 'age] :param-types [type :i64]
               :result type :effects #{}
               :body (list 'record-assoc type 'value :age 'age)}
              {:name 'same? :params ['left 'right] :param-types [type type]
               :result :i64 :effects #{}
               :body (list 'record-equal type 'left 'right)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),v=x.make(),u=x.birthday(v,8n);"
                "if(x['name-of'](v)!=='Kotoba'||v[2]!==7n||u[2]!==8n||v[2]!==7n)process.exit(2);"
                "if(!Object.isFrozen(v)||!Object.isFrozen(v[0])||x['same?'](v,u)!==0n||x['same?'](v,v)!==1n)process.exit(3);"
                "const ot=Object.freeze(['record',':demo/account',Object.freeze([Object.freeze([':name','string']),Object.freeze([':age','i64']),Object.freeze([':nickname',Object.freeze(['option','string'])])])]);"
                "try{x['name-of']([ot,'Kotoba',7n,[Object.freeze(['option','string']),false]]);process.exit(4)}"
                "catch(e){if(e.message!=='invalid-record')process.exit(5)}"
                "try{x['name-of']([v[0],'Kotoba']);process.exit(6)}catch(e){if(e.message!=='invalid-record')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "recordLimits:Object.freeze({fields:32})"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"declared keyword literal"
                          (script/emit
                           (assoc-in kir [:functions 1 :body]
                                     (list 'record-get type 'value :missing)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"expression type mismatch"
                          (script/emit
                           (assoc-in kir [:functions 0 :body]
                                     (list 'record-new type 7 7
                                           (list 'option-none-of option-type))))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique bounded vector"
                          (script/emit
                           (assoc-in kir [:functions 0 :result]
                                     [:record :demo/bad [[:x :i64] [:x :string]]]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique bounded vector"
                          (script/emit
                           (assoc-in kir [:functions 0 :result]
                                     [:record :demo/large
                                      (mapv (fn [i] [(keyword (str "f" i)) :i64])
                                            (range 33))]))))
    (is (not= type other-type))))

(deftest bounded-vector-i64-is-frozen-indexed-and-persistent
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['count-items 'lookup 'require-item 'drop-items 'update 'append 'same?]
         :effects #{}
         :functions
         [{:name 'count-items :params ['value] :param-types [:vector-i64]
           :result :i64 :effects #{} :body '(vector-count value)}
          {:name 'lookup :params ['value 'index] :param-types [:vector-i64 :i64]
           :result :i64 :effects #{} :body '(vector-get value index 99)}
          {:name 'require-item :params ['value 'index] :param-types [:vector-i64 :i64]
           :result :i64 :effects #{} :body '(vector-at value index)}
          {:name 'drop-items :params ['value 'count] :param-types [:vector-i64 :i64]
           :result :vector-i64 :effects #{} :body '(vector-drop value count)}
          {:name 'update :params ['value 'index 'item]
           :param-types [:vector-i64 :i64 :i64] :result :vector-i64 :effects #{}
           :body '(vector-assoc value index item)}
          {:name 'append :params ['value 'item] :param-types [:vector-i64 :i64]
           :result :vector-i64 :effects #{} :body '(vector-conj value item)}
          {:name 'same? :params ['left 'right] :param-types [:vector-i64 :vector-i64]
           :result :i64 :effects #{} :body '(= left right)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});const before=[1n,2n];"
                "const after=x.update(before,0n,7n);const appended=x.append(after,8n);"
                "if(x['count-items'](before)!==2n||x.lookup(before,9n)!==99n||x.lookup(before,-1n)!==99n||x.lookup(before,9223372036854775807n)!==99n)process.exit(2);"
                "if(x['require-item'](before,1n)!==2n||x['drop-items']([1n,2n,3n],1n).join(',')!=='2,3')process.exit(9);"
                "if(before[0]!==1n||after[0]!==7n||appended.length!==3||!Object.isFrozen(appended))process.exit(3);"
                "if(x['same?']([1n,2n],[1n,2n])!==1n||x['same?']([1n],[2n])!==0n)process.exit(8);"
                "for(const bad of [null,[1],[1n,'x']]){try{x['count-items'](bad);process.exit(4)}"
                "catch(e){if(e.message!=='invalid-vector-i64'&&e.message!=='invalid-i64')process.exit(5)}}"
                "try{x.update(before,2n,3n);process.exit(6)}catch(e){if(e.message!=='vector-index-out-of-range')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "vectorLimits:Object.freeze({items:128})"))
    (is (str/includes? source "const makeVector=")))
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['at] :effects #{}
             :functions [{:name 'at :params ['v] :param-types [:vector-i64]
                          :result :i64 :effects #{} :body '(vector-at v 2)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh "node" "--input-type=module" "-e"
                         (str "import('data:text/javascript;base64," encoded
                              "').then(m=>{try{m.instantiateKotoba({}).at([1n]);process.exit(2)}"
                              "catch(e){if(e.message!=='vector-index-out-of-range')process.exit(3)}})"))]
    (is (zero? (:exit result)) (:err result)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"item limit"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['too-large] :effects #{}
                          :functions [{:name 'too-large :params [] :param-types []
                                       :result :vector-i64 :effects #{}
                                       :body (apply list 'vector-new (range 129))}]}))))

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
