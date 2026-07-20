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
    (is (str/includes? source "let fuel=512;"))
    (is (not (re-find #"globalThis|window|document|eval" source)))))

(deftest i32-wrapping-shift-and-xorshift-profile-is-explicit
  (let [functions
        (mapv (fn [[name params body]]
                {:name name :params params :param-types (vec (repeat (count params) :i64))
                 :result :i64 :effects #{} :body body})
              [['signed ['x] '(i32-wrap x)]
               ['unsigned ['x] '(u32-wrap x)]
               ['add ['x 'y] '(i32-wrapping-add x y)]
               ['mul ['x 'y] '(i32-wrapping-mul x y)]
               ['xor ['x 'y] '(i32-xor x y)]
               ['shl ['x] '(i32-shift-left x 31)]
               ['shr ['x] '(i32-shift-right x 31)]
               ['ushr ['x] '(u32-shift-right x 1)]
               ['next ['x] '(xorshift32 x)]])
        source (script/emit {:format :kotoba.kir/v4 :entry nil
                             :exports (mapv :name functions) :effects #{}
                             :functions functions})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.signed(4294967295n)!==-1n||x.unsigned(-1n)!==4294967295n)process.exit(2);"
                "if(x.add(2147483647n,1n)!==-2147483648n||x.mul(2147483647n,2n)!==-2n)process.exit(3);"
                "if(x.xor(-1n,2147483647n)!==-2147483648n||x.shl(1n)!==-2147483648n)process.exit(4);"
                "if(x.shr(-2147483648n)!==-1n||x.ushr(-1n)!==2147483647n)process.exit(5);"
                "if(x.next(1n)!==270369n||x.next(270369n)!==67634689n||x.next(67634689n)!==2647435461n)process.exit(6);"
                "})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const xorshift32="))
    (is (str/includes? source "BigInt.asIntN(32")))
  (doseq [count [-1 32]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"integer literal"
                          (script/emit
                           {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                            :functions [{:name 'bad :params ['x] :param-types [:i64]
                                         :result :i64 :effects #{}
                                         :body (list 'i32-shift-left 'x count)}]})))))

(deftest floating-point-f64-bit-profile-is-explicit-and-sealed
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['bits 'from-bits 'negative-zero 'infinity 'nan-bits]
         :effects #{}
         :functions
         [{:name 'bits :params ['value] :param-types [:f64]
           :result :i64 :effects #{} :body '(f64-to-bits value)}
          {:name 'from-bits :params ['value] :param-types [:i64]
           :result :f64 :effects #{} :body '(f64-from-bits value)}
          {:name 'negative-zero :params [] :param-types []
           :result :f64 :effects #{} :body -0.0}
          {:name 'infinity :params [] :param-types []
           :result :f64 :effects #{} :body Double/POSITIVE_INFINITY}
          {:name 'nan-bits :params [] :param-types []
           :result :i64 :effects #{} :body '(f64-to-bits ##NaN)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.bits(1.5)!==4609434218613702656n)process.exit(2);"
                "if(x.bits(-0)!==-9223372036854775808n)process.exit(3);"
                "if(!Object.is(x['negative-zero'](),-0))process.exit(4);"
                "if(x.infinity()!==Infinity)process.exit(5);"
                "if(x['nan-bits']()!==9221120237041090560n)process.exit(6);"
                "if(!Number.isNaN(x['from-bits'](9221120237041090560n)))process.exit(7);"
                "try{x.bits(1n);process.exit(8)}catch(e){}console.log('f64-ok')})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (= "ieee-754-f32-f64-v7" script/floating-point-policy))
    (is (str/includes? source "floatingPointPolicy:'ieee-754-f32-f64-v7'"))
    (is (zero? (:exit result)) (:err result))
    (is (= "f64-ok\n" (:out result)))
    (is (str/includes? source "f64ToBits")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"type mismatch"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params ['x] :param-types [:f64]
                                       :result :i64 :effects #{} :body 'x}]}))))

(deftest f64-arithmetic-has-explicit-ieee-special-value-semantics
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['add 'divide 'neg 'absolute 'equal 'less 'unordered 'nan-bits]
         :effects #{}
         :functions
         [{:name 'add :params ['x 'y] :param-types [:f64 :f64]
           :result :f64 :effects #{} :body '(f64-add x y)}
          {:name 'divide :params ['x 'y] :param-types [:f64 :f64]
           :result :f64 :effects #{} :body '(f64-div x y)}
          {:name 'neg :params ['x] :param-types [:f64]
           :result :f64 :effects #{} :body '(f64-neg x)}
          {:name 'absolute :params ['x] :param-types [:f64]
           :result :f64 :effects #{} :body '(f64-abs x)}
          {:name 'equal :params ['x 'y] :param-types [:f64 :f64]
           :result :bool :effects #{} :body '(f64-eq x y)}
          {:name 'less :params ['x 'y] :param-types [:f64 :f64]
           :result :bool :effects #{} :body '(f64-lt x y)}
          {:name 'unordered :params ['x 'y] :param-types [:f64 :f64]
           :result :bool :effects #{} :body '(f64-unordered x y)}
          {:name 'nan-bits :params [] :param-types []
           :result :i64 :effects #{}
           :body '(f64-to-bits (f64-div (f64-from-bits 0) (f64-from-bits 0)))}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({});"
                     "if(x.add(0.1,0.2)!==0.30000000000000004)process.exit(2);"
                     "if(x.divide(1,0)!==Infinity||!Number.isNaN(x.divide(0,0)))process.exit(3);"
                     "if(!Object.is(x.neg(0),-0)||!Object.is(x.absolute(-0),0))process.exit(4);"
                     "if(!x.equal(0,-0)||x.equal(NaN,NaN)||!x.less(-1,0))process.exit(5);"
                     "if(!x.unordered(NaN,1)||x.unordered(1,2))process.exit(6);"
                     "if(x['nan-bits']()!==9221120237041090560n)process.exit(7)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest direct-floating-ordered-collections-fail-closed
  (doseq [type [[:set :f64] [:map :keyword :f64] [:map :f32 :i64]]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"direct floating"
         (script/emit
          {:format :kotoba.kir/v4 :entry nil :exports [] :effects #{}
           :functions [{:name 'value :params [] :param-types [] :result type
                        :effects #{} :body 0}]})))))

(deftest f64-i64-conversions-distinguish-exact-rounded-and-truncating
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['to-f64 'rounded 'to-i64 'truncating]
         :effects #{}
         :functions
         [{:name 'to-f64 :params ['x] :param-types [:i64]
           :result :f64 :effects #{} :body '(i64-to-f64-checked x)}
          {:name 'rounded :params ['x] :param-types [:i64]
           :result :f64 :effects #{} :body '(i64-to-f64-rounded x)}
          {:name 'to-i64 :params ['x] :param-types [:f64]
           :result :i64 :effects #{} :body '(f64-to-i64-checked x)}
          {:name 'truncating :params ['x] :param-types [:f64]
           :result :i64 :effects #{} :body '(f64-to-i64-truncating x)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({});"
                     "if(x['to-f64'](9007199254740992n)!==9007199254740992)process.exit(2);"
                     "try{x['to-f64'](9007199254740993n);process.exit(3)}catch(e){}"
                     "if(x.rounded(9007199254740993n)!==9007199254740992)process.exit(4);"
                     "if(x['to-i64'](-0)!==0n||x['to-i64'](42)!==42n)process.exit(5);"
                     "for(const v of [1.5,NaN,Infinity,9223372036854775808])"
                     "{try{x['to-i64'](v);process.exit(6)}catch(e){}}"
                     "if(x.truncating(1.9)!==1n||x.truncating(-1.9)!==-1n)process.exit(7);"
                     "for(const v of [NaN,Infinity,-Infinity,9223372036854775808])"
                     "{try{x.truncating(v);process.exit(8)}catch(e){}}})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest f32-is-an-explicitly-rounded-scalar-profile
  (let [typed-kir
        {:format :kotoba.kir/v4 :entry nil
         :exports ['from-bits 'bits 'rounded 'widen 'add 'divide 'unordered
                   'to-f32 'rounded-i64 'to-i64 'truncating]
         :effects #{}
         :functions
         [{:name 'from-bits :params ['x] :param-types [:i64]
           :result :f32 :effects #{} :body '(f32-from-bits x)}
          {:name 'bits :params ['x] :param-types [:f32]
           :result :i64 :effects #{} :body '(f32-to-bits x)}
          {:name 'rounded :params ['x] :param-types [:f64]
           :result :f32 :effects #{} :body '(f64-to-f32-rounded x)}
          {:name 'widen :params ['x] :param-types [:f32]
           :result :f64 :effects #{} :body '(f32-to-f64-exact x)}
          {:name 'add :params ['x 'y] :param-types [:f32 :f32]
           :result :f32 :effects #{} :body '(f32-add x y)}
          {:name 'divide :params ['x 'y] :param-types [:f32 :f32]
           :result :f32 :effects #{} :body '(f32-div x y)}
          {:name 'unordered :params ['x 'y] :param-types [:f32 :f32]
           :result :bool :effects #{} :body '(f32-unordered x y)}
          {:name 'to-f32 :params ['x] :param-types [:i64]
           :result :f32 :effects #{} :body '(i64-to-f32-checked x)}
          {:name 'rounded-i64 :params ['x] :param-types [:i64]
           :result :f32 :effects #{} :body '(i64-to-f32-rounded x)}
          {:name 'to-i64 :params ['x] :param-types [:f32]
           :result :i64 :effects #{} :body '(f32-to-i64-checked x)}
          {:name 'truncating :params ['x] :param-types [:f32]
           :result :i64 :effects #{} :body '(f32-to-i64-truncating x)}]}
        source (script/emit typed-kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),one=x['from-bits'](1065353216n),"
                     "negz=x['from-bits'](-2147483648n),nan=x['from-bits'](2143289344n);"
                     "if(one!==1||!Object.is(negz,-0)||x.bits(nan)!==2143289344n)process.exit(2);"
                     "const tenth=x.rounded(0.1);if(x.bits(tenth)!==1036831949n)process.exit(3);"
                     "if(x.widen(tenth)!==0.10000000149011612)process.exit(4);"
                     "if(x.bits(x.add(one,tenth))!==1066192077n)process.exit(5);"
                     "if(x.divide(one,x['from-bits'](0n))!==Infinity||!x.unordered(nan,one))process.exit(6);"
                     "if(x['to-f32'](16777216n)!==16777216)process.exit(7);"
                     "try{x['to-f32'](16777217n);process.exit(8)}catch(e){}"
                     "if(x['rounded-i64'](16777217n)!==16777216)process.exit(9);"
                     "if(x['to-i64'](one)!==1n||x.truncating(x.rounded(1.9))!==1n)process.exit(10);"
                     "try{x['to-i64'](tenth);process.exit(11)}catch(e){}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest floating-sqrt-min-max-have-sealed-special-value-semantics
  (let [kir {:format :kotoba.kir/v4 :entry nil
             :exports ['f32-s 'f32-lo 'f32-hi 'f64-s 'f64-lo 'f64-hi]
             :effects #{}
             :functions
             [{:name 'f32-s :params ['x] :param-types [:f32] :result :f32
               :effects #{} :body '(f32-sqrt x)}
              {:name 'f32-lo :params ['x 'y] :param-types [:f32 :f32] :result :f32
               :effects #{} :body '(f32-min x y)}
              {:name 'f32-hi :params ['x 'y] :param-types [:f32 :f32] :result :f32
               :effects #{} :body '(f32-max x y)}
              {:name 'f64-s :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-sqrt x)}
              {:name 'f64-lo :params ['x 'y] :param-types [:f64 :f64] :result :f64
               :effects #{} :body '(f64-min x y)}
              {:name 'f64-hi :params ['x 'y] :param-types [:f64 :f64] :result :f64
               :effects #{} :body '(f64-max x y)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),p=Math.fround(0),n=Math.fround(-0);"
                     "if(x['f32-s'](Math.fround(4))!==2||!Number.isNaN(x['f32-s'](Math.fround(-1))))process.exit(2);"
                     "if(!Object.is(x['f32-lo'](p,n),-0)||!Object.is(x['f32-hi'](p,n),0))process.exit(3);"
                     "if(!Number.isNaN(x['f32-lo'](NaN,p))||!Number.isNaN(x['f32-hi'](p,NaN)))process.exit(4);"
                     "if(x['f64-s'](4)!==2||!Number.isNaN(x['f64-s'](-1)))process.exit(5);"
                     "if(!Object.is(x['f64-lo'](0,-0),-0)||!Object.is(x['f64-hi'](0,-0),0))process.exit(6);"
                     "if(!Number.isNaN(x['f64-lo'](NaN,0))||!Number.isNaN(x['f64-hi'](0,NaN)))process.exit(7)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest bounded-trigonometry-has-a-deterministic-error-contract
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['sin 'cos] :effects #{}
             :functions
             [{:name 'sin :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-sin-quarter-turn x)}
              {:name 'cos :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-cos-quarter-turn x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),q=Math.PI/4;"
                     "if(x.sin(q)!==0.7071067811865475||x.cos(q)!==0.7071067811865476)process.exit(2);"
                     "if(!Object.is(x.sin(-0),-0)||x.cos(-0)!==1)process.exit(3);"
                     "for(let i=0;i<=128;i++){const v=-q+2*q*i/128;"
                     "if(Math.abs(x.sin(v)-Math.sin(v))>4e-15||Math.abs(x.cos(v)-Math.cos(v))>4e-15)process.exit(4);}"
                     "for(const v of [NaN,Infinity,-Infinity,q+Number.EPSILON,-q-Number.EPSILON])"
                     "{for(const f of [x.sin,x.cos]){try{f(v);process.exit(5)}catch(e){"
                     "if(e.message!=='f64-quarter-turn-domain')process.exit(6)}}}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest bounded-wide-angle-trigonometry-has-explicit-range-reduction
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['sin 'cos] :effects #{}
             :functions
             [{:name 'sin :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-sin-bounded x)}
              {:name 'cos :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-cos-bounded x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),limit=8192*Math.PI;"
                     "for(const v of [0,-0,Math.PI/2,-Math.PI/2,Math.PI,-Math.PI,limit,-limit]){"
                     "if(Math.abs(x.sin(v)-Math.sin(v))>5e-12||Math.abs(x.cos(v)-Math.cos(v))>5e-12)process.exit(2);}"
                     "for(let i=0;i<=128;i++){const v=-limit+2*limit*i/128;"
                     "if(Math.abs(x.sin(v)-Math.sin(v))>5e-12||Math.abs(x.cos(v)-Math.cos(v))>5e-12)process.exit(3);}"
                     "if(!Object.is(x.sin(-0),-0)||x.cos(-0)!==1)process.exit(4);"
                     "for(const v of [NaN,Infinity,-Infinity,limit+Number.EPSILON*limit,-limit-Number.EPSILON*limit])"
                     "{for(const f of [x.sin,x.cos]){try{f(v);process.exit(5)}catch(e){"
                     "if(e.message!=='f64-bounded-angle-domain')process.exit(6)}}}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest bounded-exp-and-log-have-fixed-polynomial-contracts
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['exp 'log] :effects #{}
             :functions
             [{:name 'exp :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-exp-near-zero x)}
              {:name 'log :params ['x] :param-types [:f64] :result :f64
               :effects #{} :body '(f64-log-near-one x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({});"
                     "for(let i=0;i<=128;i++){const v=-0.5+i/128;"
                     "if(Math.abs(x.exp(v)-Math.exp(v))>4e-15)process.exit(2);}"
                     "for(let i=0;i<=128;i++){const v=0.75+0.75*i/128;"
                     "if(Math.abs(x.log(v)-Math.log(v))>4e-15)process.exit(3);}"
                     "if(x.exp(0)!==1||!Object.is(x.log(1),0))process.exit(4);"
                     "for(const [f,values,message] of [[x.exp,[NaN,Infinity,-Infinity,0.5000000000000001],"
                     "'f64-exp-near-zero-domain'],[x.log,[NaN,Infinity,-Infinity,0,0.7499999999999999,1.5000000000000002],"
                     "'f64-log-near-one-domain']]){for(const v of values){try{f(v);process.exit(5)}catch(e){"
                     "if(e.message!==message)process.exit(6)}}}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest bounded-atan2-has-a-fixed-finite-domain-contract
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['atan2] :effects #{}
             :functions [{:name 'atan2 :params ['y 'x] :param-types [:f64 :f64]
                          :result :f64 :effects #{} :body '(f64-atan2-bounded y x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const f=m.instantiateKotoba({}).atan2;"
                     "for(const y of [-1e300,-7,-1,-0,0,1,7,1e300])for(const x of [-1e300,-9,-1,-0,0,1,9,1e300]){"
                     "if(Math.abs(f(y,x)-Math.atan2(y,x))>2e-15)process.exit(2);}"
                     "if(!Object.is(f(-0,1),-0)||f(0,-1)!==Math.PI||f(-0,-1)!==-Math.PI)process.exit(3);"
                     "for(const p of [[NaN,1],[1,NaN],[Infinity,1],[1,-Infinity]])try{f(...p);process.exit(4)}"
                     "catch(e){if(e.message!=='f64-atan2-bounded-domain')process.exit(5)}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

(deftest wide-exp-and-log-use-fixed-binary-scaling
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['exp 'log] :effects #{}
             :functions [{:name 'exp :params ['x] :param-types [:f64] :result :f64
                          :effects #{} :body '(f64-exp-bounded x)}
                         {:name 'log :params ['x] :param-types [:f64] :result :f64
                          :effects #{} :body '(f64-log-bounded x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),limit=512*Math.LN2;"
                     "for(let i=0;i<=128;i++){const v=-limit+2*limit*i/128,a=x.exp(v),b=Math.exp(v);"
                     "if(Math.abs(a-b)/b>1e-13)process.exit(2);}"
                     "for(let i=-512;i<=512;i+=8){const v=2**i;"
                     "if(Math.abs(x.log(v)-Math.log(v))>1e-13)process.exit(3);}"
                     "if(x.exp(0)!==1||!Object.is(x.log(1),0))process.exit(4);"
                     "for(const [f,vals,msg] of [[x.exp,[NaN,Infinity,-Infinity,limit+Number.EPSILON*limit],"
                     "'f64-exp-bounded-domain'],[x.log,[NaN,Infinity,-Infinity,0,2**-513,2**513],"
                     "'f64-log-bounded-domain']])for(const v of vals)try{f(v);process.exit(5)}catch(e){"
                     "if(e.message!==msg)process.exit(6)}})"
                     ".catch(e=>{console.error(e.message);process.exit(99)})"))]
    (is (zero? (:exit result)) (:err result))))

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

(deftest bounded-string-replacement-is-literal-and-fails-closed
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['replace]
             :effects #{}
             :functions [{:name 'replace :params ['value 'needle 'replacement]
                          :param-types [:string :string :string]
                          :result :string :effects #{}
                          :body '(string-replace-all value needle replacement)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "if(x.replace('a.$a.$','.','$')!=='a$$a$$')process.exit(2);"
                "try{x.replace('abc','','x');process.exit(3)}"
                "catch(e){if(e.message!=='empty-string-replacement-needle')process.exit(4)}"
                "try{x.replace('x'.repeat(40000),'x','xx');process.exit(5)}"
                "catch(e){if(e.message!=='string-too-large')process.exit(6)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "stringReplaceAll")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"type mismatch"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['bad] :effects #{}
                          :functions [{:name 'bad :params ['value]
                                       :param-types [:string] :result :string :effects #{}
                                       :body '(string-replace-all value 1 "x")}]}))))

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

(deftest structured-f64-values-preserve-nan-and-signed-zero
  (let [vector-type [:vector [:f64 :f64]]
        record-type [:record :geometry/point [[:x :f64] [:y :f64]]]
        kir {:format :kotoba.kir/v4 :entry nil
             :exports ['make-vector 'vector-x 'make-point 'point-y 'move-x]
             :effects #{}
             :functions
             [{:name 'make-vector :params [] :param-types [] :result vector-type :effects #{}
               :body (list 'hetero-vector-new vector-type -0.0 ##NaN)}
              {:name 'vector-x :params ['value] :param-types [vector-type]
               :result :f64 :effects #{}
               :body (list 'hetero-vector-at vector-type 'value 0)}
              {:name 'make-point :params [] :param-types [] :result record-type :effects #{}
               :body (list 'record-new record-type 1.25 -0.0)}
              {:name 'point-y :params ['value] :param-types [record-type]
               :result :f64 :effects #{}
               :body (list 'record-get record-type 'value :y)}
              {:name 'move-x :params ['value 'x] :param-types [record-type :f64]
               :result record-type :effects #{}
               :body (list 'record-assoc record-type 'value :x 'x)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded "').then(m=>{"
                     "const x=m.instantiateKotoba({}),v=x['make-vector'](),p=x['make-point']();"
                     "if(!Object.is(x['vector-x'](v),-0)||!Number.isNaN(v[2]))process.exit(2);"
                     "if(!Object.is(x['point-y'](p),-0)||x['move-x'](p,2.5)[1]!==2.5)process.exit(3);"
                     "try{x['point-y']([p[0],1.0,'bad']);process.exit(4)}"
                     "catch(e){if(e.message!=='invalid-f64')process.exit(5)}})"))]
    (is (zero? (:exit result)) (:err result))))
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
                "const fixture=Array.from({length:12171},(_,i)=>BigInt(i&255));"
                "if(x['count-items'](fixture)!==12171n||x['require-item'](fixture,12170n)!==138n)process.exit(10);"
                "const after=x.update(before,0n,7n);const appended=x.append(after,8n);"
                "if(x['count-items'](before)!==2n||x.lookup(before,9n)!==99n||x.lookup(before,-1n)!==99n||x.lookup(before,9223372036854775807n)!==99n)process.exit(2);"
                "if(x['require-item'](before,1n)!==2n||x['drop-items']([1n,2n,3n],1n).join(',')!=='2,3')process.exit(9);"
                "if(before[0]!==1n||after[0]!==7n||appended.length!==3||!Object.isFrozen(appended))process.exit(3);"
                "if(x['same?']([1n,2n],[1n,2n])!==1n||x['same?']([1n],[2n])!==0n)process.exit(8);"
                "for(const bad of [null,[1],[1n,'x']]){try{x['count-items'](bad);process.exit(4)}"
                "catch(e){if(e.message!=='invalid-vector-i64'&&e.message!=='invalid-i64')process.exit(5)}}"
                "try{x['count-items'](Array.from({length:16385},()=>0n));process.exit(11)}"
                "catch(e){if(e.message!=='vector-too-large')process.exit(12)}"
                "try{x.update(before,2n,3n);process.exit(6)}catch(e){if(e.message!=='vector-index-out-of-range')process.exit(7)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "vectorLimits:Object.freeze({items:16384})"))
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
                                       :body (apply list 'vector-new (range 16385))}]}))))

(deftest bounded-vector-f64-preserves-ieee-values-and-persistence
  (let [kir {:format :kotoba.kir/v4 :entry nil
             :exports ['make 'count-items 'lookup 'require-item 'drop-items 'update 'append]
             :effects #{}
             :functions
             [{:name 'make :params [] :param-types [] :result :vector-f64 :effects #{}
               :body '(vector-f64-new -0.0 ##NaN 1.5)}
              {:name 'count-items :params ['value] :param-types [:vector-f64]
               :result :i64 :effects #{} :body '(vector-f64-count value)}
              {:name 'lookup :params ['value 'index 'fallback]
               :param-types [:vector-f64 :i64 :f64] :result :f64 :effects #{}
               :body '(vector-f64-get value index fallback)}
              {:name 'require-item :params ['value 'index] :param-types [:vector-f64 :i64]
               :result :f64 :effects #{} :body '(vector-f64-at value index)}
              {:name 'drop-items :params ['value 'count] :param-types [:vector-f64 :i64]
               :result :vector-f64 :effects #{} :body '(vector-f64-drop value count)}
              {:name 'update :params ['value 'index 'item]
               :param-types [:vector-f64 :i64 :f64] :result :vector-f64 :effects #{}
               :body '(vector-f64-assoc value index item)}
              {:name 'append :params ['value 'item] :param-types [:vector-f64 :f64]
               :result :vector-f64 :effects #{} :body '(vector-f64-conj value item)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),v=x.make();"
                "if(x['count-items'](v)!==3n||!Object.is(v[0],-0)||!Number.isNaN(v[1])||v[2]!==1.5)process.exit(2);"
                "const changed=x.update(v,2n,-0),appended=x.append(changed,Infinity),dropped=x['drop-items'](appended,1n);"
                "if(v[2]!==1.5||!Object.is(changed[2],-0)||appended[3]!==Infinity||!Object.isFrozen(dropped))process.exit(3);"
                "if(!Number.isNaN(x.lookup(v,1n,7))||x.lookup(v,99n,-2.5)!==-2.5)process.exit(4);"
                "try{x['require-item'](v,-1n);process.exit(5)}catch(e){if(e.message!=='vector-f64-index-out-of-range')process.exit(6)}"
                "for(const bad of [null,[1n],[undefined]]){try{x['count-items'](bad);process.exit(7)}catch(e){}}"
                "try{x['count-items'](Array.from({length:16385},()=>0));process.exit(8)}"
                "catch(e){if(e.message!=='vector-f64-too-large')process.exit(9)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const makeVectorF64="))
    (is (str/includes? source "vectorF64Get(")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"item limit"
                        (script/emit
                         {:format :kotoba.kir/v4 :entry nil :exports ['too-large] :effects #{}
                          :functions [{:name 'too-large :params [] :param-types []
                                       :result :vector-f64 :effects #{}
                                       :body (apply list 'vector-f64-new
                                                    (repeat 16385 0.0))}]}))))

(deftest compact-string-index-and-disjoint-set-are-bounded-and-persistent
  (let [functions
        [{:name 'index :params [] :param-types [] :result :string-index :effects #{}
          :body '(string-index-assoc (string-index-assoc (string-index-new) "b" 2) "a" 1)}
         {:name 'lookup :params [] :param-types [] :result :i64 :effects #{}
          :body '(option-value-of [:option :i64] (string-index-get (index) "b") 99)}
         {:name 'joined :params [] :param-types [] :result :disjoint-set-i64 :effects #{}
          :body '(option-value-of [:option :disjoint-set-i64]
                   (disjoint-set-i64-union (disjoint-set-i64-new 3) 0 2)
                   (disjoint-set-i64-new 0))}
         {:name 'cycle :params [] :param-types [] :result :bool :effects #{}
          :body '(option-some?-of [:option :disjoint-set-i64]
                   (disjoint-set-i64-union (joined) 2 0))}]
        source (script/emit {:format :kotoba.kir/v4 :entry nil
                             :exports (mapv :name functions) :effects #{} :functions functions})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh "node" "--input-type=module" "-e"
                         (str "import('data:text/javascript;base64," encoded
                              "').then(m=>{const x=m.instantiateKotoba({}),i=x.index(),d=x.joined();"
                              "if(x.lookup()!==2n||i.length!==2||i[0][0]!=='a'||d[0].length!==3||x.cycle()!==false)process.exit(2);"
                              "if(!Object.isFrozen(i)||!Object.isFrozen(i[0])||!Object.isFrozen(d[0]))process.exit(3);"
                              "for(const f of [()=>x.index(Array(129).fill(['x',0n])),()=>x.joined(129n)]){try{f();}catch(e){}}})"))]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const assertStringIndex="))
    (is (str/includes? source "const disjointSetI64Union="))))

(deftest bounded-canonical-documents-are-persistent-and-reject-host-objects
  (let [functions
        [{:name 'doc :params [] :param-types [] :result :document :effects #{}
          :body '(document-map :type (document-string "Annotation")
                               :target (document-string "urn:test")
                               :enabled (document-bool true))}
         {:name 'type-name :params [] :param-types [] :result :string :effects #{}
          :body '(option-value-of [:option :string]
                   (document-string-value
                    (option-value-of [:option :document]
                      (document-get (doc) :type) (document-null)))
                   "missing")}
         {:name 'updated-count :params [] :param-types [] :result :i64 :effects #{}
          :body '(document-count
                   (document-dissoc
                    (document-merge (doc)
                      (document-map :creator (document-string "alice")))
                    :enabled))}
         {:name 'external-count :params ['value] :param-types [:document]
          :result :i64 :effects #{} :body '(document-count value)}
         {:name 'kind :params ['value] :param-types [:document]
          :result :keyword :effects #{} :body '(document-kind value)}
         {:name 'same :params ['left 'right] :param-types [:document :document]
          :result :bool :effects #{} :body '(document-equal? left right)}
         {:name 'repeated-nulls :params [] :param-types [] :result :document :effects #{}
          :body '(document-map :a (document-null) :b (document-null))}
         {:name 'repeated-value :params [] :param-types [] :result :document :effects #{}
          :body '(let [item (document-map :name (document-string "same"))]
                   (document-vector item item))}]
        source (script/emit {:format :kotoba.kir/v4 :entry nil
                             :exports (mapv :name functions) :effects #{} :functions functions})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result
        (shell/sh
         "node" "--input-type=module" "-e"
         (str "import('data:text/javascript;base64," encoded
              "').then(m=>{const x=m.instantiateKotoba({}),d=x.doc();"
              "if(x['type-name']()!=='Annotation'||x['updated-count']()!==3n||x.kind(d)!==':map')process.exit(2);"
              "const same=['map',[[':items',['vector',[['i64',1n],['string','x']]]]]],different=['map',[[':items',['vector',[['i64',2n],['string','x']]]]]];if(x.same(same,same)!==true||x.same(same,different)!==false||x.same(['f64',-0],['f64',0])!==true)process.exit(7);"
              "const nulls=x['repeated-nulls'](),repeated=x['repeated-value']();if(nulls[1].length!==2||nulls[1][0][1][0]!=='null'||repeated[1].length!==2||repeated[1][0][1][0][1][1]!=='same'||repeated[1][0]===repeated[1][1])process.exit(8);"
              "for(const [v,k] of [[['null'],':null'],[['bool',true],':bool'],[['i64',1n],':i64'],[['f64',1],':f64'],[['string','x'],':string'],[['keyword',':x'],':keyword'],[['vector',[]],':vector']])if(x.kind(v)!==k)process.exit(6);"
              "if(!Object.isFrozen(d)||!Object.isFrozen(d[1])||!Object.isFrozen(d[1][0]))process.exit(3);"
              "for(const bad of [{type:'Annotation'},['map',[[':b',['null']],[':a',['null']]]],['f64',Infinity]]){let rejected=false;"
              "try{x['external-count'](bad)}catch(e){rejected=true}if(!rejected)process.exit(4);}"
              "const cycle=['vector',[]];cycle[1].push(cycle);let rejected=false;"
              "try{x['external-count'](cycle)}catch(e){rejected=true}if(!rejected)process.exit(5);"
              "const child=['string','shared'],shared=['vector',[child,child]];rejected=false;try{x['external-count'](shared)}catch(e){rejected=true}if(!rejected)process.exit(9);})"))]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const assertDoc="))
    (is (str/includes? source "const docMerge="))
    (is (str/includes? source "const docKind="))
    (is (str/includes? source "const docEqual="))))

(deftest bounded-document-vectors-have-safe-persistent-operations
  (let [functions
        [{:name 'items :params [] :param-types [] :result :document :effects #{}
          :body '(document-vector (document-i64 1) (document-i64 2))}
         {:name 'first-item :params [] :param-types [] :result :i64 :effects #{}
          :body '(option-value-of [:option :i64]
                   (document-i64-value
                    (option-value-of [:option :document]
                      (document-vector-at (items) 0) (document-null))) -1)}
         {:name 'changed :params [] :param-types [] :result :document :effects #{}
          :body '(document-vector-conj
                   (document-vector-assoc (items) 1 (document-i64 7))
                   (document-i64 9))}
         {:name 'kind :params [] :param-types [] :result :keyword :effects #{}
          :body '(option-value-of [:option :keyword]
                   (document-keyword-value (document-keyword :fixed)) :missing)}
         {:name 'tail :params [] :param-types [] :result :document :effects #{}
          :body '(document-vector-drop (changed) 1)}
         {:name 'removed :params [] :param-types [] :result :document :effects #{}
          :body '(document-vector-remove (changed) 1)}
         {:name 'entry :params [] :param-types [] :result :document :effects #{}
          :body '(option-value-of [:option :document]
                   (document-map-entry-at
                     (document-map :z (document-i64 9) :a (document-string "first")) 0)
                   (document-null))}
         {:name 'key-name :params [] :param-types [] :result :string :effects #{}
          :body '(keyword-name :rdf/type)}
         {:name 'bad-assoc :params [] :param-types [] :result :document :effects #{}
          :body '(document-vector-assoc (items) -1 (document-null))}]
        source (script/emit {:format :kotoba.kir/v4 :entry nil
                             :exports (mapv :name functions) :effects #{} :functions functions})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({}),v=x.changed(),t=x.tail();"
                     "const r=x.removed(),e=x.entry();if(x['first-item']()!==1n||x.kind()!==':fixed'||x['key-name']()!=='type'||v[1].length!==3||v[1][1][1]!==7n||t[1].length!==2||r[1].length!==2||r[1][1][1]!==9n)process.exit(2);"
                     "if(e[0]!=='vector'||e[1][0][1]!==':a'||e[1][1][1]!=='first')process.exit(6);"
                     "if(!Object.isFrozen(v)||!Object.isFrozen(v[1])||!Object.isFrozen(t))process.exit(3);"
                     "try{x['bad-assoc']();process.exit(4)}catch(e){if(e.message!=='doc-vector-index-out-of-range')process.exit(5)}"
                     "})"))]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const docVectorAt="))
    (is (str/includes? source "const docVectorConj="))))

(deftest bounded-keywords-can-be-created-from-safe-runtime-text
  (let [source (script/emit
                {:format :kotoba.kir/v4 :entry nil :exports ['context-key]
                 :effects #{}
                 :functions [{:name 'context-key :params [] :param-types []
                              :result :keyword :effects #{}
                              :body '(keyword-from-string "@context")}]})
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh "node" "--input-type=module" "-e"
                         (str "import('data:text/javascript;base64," encoded
                              "').then(m=>{if(m.instantiateKotoba({})['context-key']()!==':@context')process.exit(2)})"))]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "const keywordFromString="))))

(deftest rejects-unchecked-or-unknown-ir
  (is (thrown? clojure.lang.ExceptionInfo (script/emit {:format :unknown})))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/emit {:format :kotoba.kir/v3 :entry 'main :effects #{}
                             :functions [{:name 'main :params [] :body '(fetch 1)}]}))))

(deftest ast-verifier-rejects-obfuscated-ambient-authority
  ;; Parsing normalizes the escaped spelling to the forbidden `globalThis`
  ;; identifier before the authority check.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"AST violates restricted subset"
                        (script/verify-output!
                         "export const x=global\\u0054his.fetch;")))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/verify-output! "export const x=({}).constructor;")))
  (is (thrown? clojure.lang.ExceptionInfo
               (script/verify-output! "export const x=import('x');"))))

(deftest verifier-distinguishes-generated-identifiers-from-ambient-authority
  (let [kir {:format :kotoba.kir/v3 :entry nil :exports ['within-window]
             :effects #{}
             :functions
             [{:name 'within-window
               :params ['window-ms 'document-count 'process-id]
               :param-types [:i64 :i64 :i64]
               :result :i64
               :effects #{}
               :body '(if (<= document-count window-ms) process-id 0)}]}
        source (script/emit kir)]
    (is (string? source))
    (is (str/includes? source "k$window$002dms"))
    (is (str/includes? source "k$document$002dcount"))
    (is (str/includes? source "k$process$002did")))
  (is (= "export const x='window document process globalThis';"
         (script/verify-output!
          "export const x='window document process globalThis';")))
  (doseq [source ["export const x=window;"
                  "export const x=document;"
                  "export const x=process;"
                  "export const x={'__proto__':1};"]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (script/verify-output! source)))))

(deftest bounded-typed-maps-execute-with-canonical-keys-and-typed-absence
  (let [type [:map :keyword :i64]
        option-type [:option :i64]
        entry-type [:option [:vector [:keyword :i64]]]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['present 'missing 'updated 'entry]
             :effects #{}
             :functions
             [{:name 'present :params [] :param-types [] :result option-type :effects #{}
               :body (list 'typed-map-get type
                           (list 'typed-map-new type :b 2 :a 1) :b)}
              {:name 'missing :params [] :param-types [] :result option-type :effects #{}
               :body (list 'typed-map-get type (list 'typed-map-new type) :x)}
              {:name 'updated :params [] :param-types [] :result :i64 :effects #{}
               :body (list 'typed-map-count type
                           (list 'typed-map-dissoc type
                                 (list 'typed-map-assoc type
                                       (list 'typed-map-new type :a 1) :b 2)
                                 :a))}
              {:name 'entry :params [] :param-types [] :result entry-type :effects #{}
               :body (list 'typed-map-entry-at type
                           (list 'typed-map-new type :b 2 :a 1) 0)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        result (shell/sh
                "node" "--input-type=module" "-e"
                (str "import('data:text/javascript;base64," encoded
                     "').then(m=>{const x=m.instantiateKotoba({});"
                     "const p=x.present(),n=x.missing(),e=x.entry();"
                     "if(p[1]!==true||p[2]!==2n||n[1]!==false||x.updated()!==1n||"
                     "e[1]!==true||e[2][1]!==':a'||e[2][2]!==1n)process.exit(2)})"))]
    (is (zero? (:exit result)) (:err result))
    (is (str/includes? source "typedMapLimits:Object.freeze({entries:31})"))))

(deftest bounded-xml-subset-has-exact-path-and-typed-absence-semantics
  (let [option-string [:option :string]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['count-path 'attr]
             :effects #{}
             :functions
             [{:name 'count-path :params ['xml 'path] :param-types [:string :string]
               :result :i64 :effects #{} :body '(xml-path-count xml path)}
              {:name 'attr :params ['xml 'path 'index 'attribute]
               :param-types [:string :string :i64 :string]
               :result option-string :effects #{}
               :body '(xml-path-attr xml path index attribute)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        xml "<?xml version=\"1.0\" encoding=\"utf-8\"?><!-- bounded --><robot name=\"cart\"><link name=\"base\"><inertial><mass value=\"1.5\"/></inertial></link><link name='tip'/><joint name=\"slide\" type=\"prismatic\"><parent link=\"base\"/><child link=\"tip\"/></joint></robot>"
        xml64 (.encodeToString (java.util.Base64/getEncoder) (.getBytes xml "UTF-8"))
        invalid ["<!DOCTYPE robot><robot/>"
                 "<robot><link name=\"a&amp;b\"/></robot>"
                 "<robot>text</robot>"
                 "<robot a=\"1\" a=\"2\"/>"
                 "<robot><link/></wrong>"
                 "<?unsafe x?><robot/>"
                 "<robot><![CDATA[x]]></robot>"
                 "<robot/>trailing"
                 (str (apply str (repeat 33 "<n>"))
                      (apply str (repeat 33 "</n>")))
                 (str "<robot "
                      (str/join " " (map #(str "a" % "=\"x\"") (range 33)))
                      "/>")
                 (str "<robot>" (apply str (repeat 2048 "<n/>")) "</robot>")]
        invalid64 (mapv #(.encodeToString (java.util.Base64/getEncoder)
                                         (.getBytes ^String % "UTF-8")) invalid)
        invalid-js (str "[" (str/join "," (map pr-str invalid64)) "]")
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),xml=Buffer.from('" xml64 "','base64').toString();"
                "if(x['count-path'](xml,'robot/link')!==2n)process.exit(2);"
                "if(x['count-path'](xml,'robot/link/inertial/mass')!==1n)process.exit(3);"
                "const tip=x.attr(xml,'robot/link',1n,'name'),mass=x.attr(xml,'robot/link/inertial/mass',0n,'value'),missing=x.attr(xml,'robot/link',0n,'missing');"
                "if(!tip[1]||tip[2]!=='tip'||!mass[1]||mass[2]!=='1.5'||missing[1])process.exit(4);"
                "for(const b of " invalid-js "){let trapped=false;try{x['count-path'](Buffer.from(b,'base64').toString(),'robot')}catch(e){trapped=true}if(!trapped)process.exit(5)}"
                "for(const f of [()=>x['count-path'](xml,'robot//link'),"
                "()=>x['count-path'](xml,'" (str/join "/" (repeat 33 "n")) "'),"
                "()=>x['count-path']('<r a=\"" (apply str (repeat 65536 "x")) "\"/>','r'),"
                "()=>x.attr(xml,'robot/link',-1n,'name')]){let trapped=false;try{f()}catch(e){trapped=true}if(!trapped)process.exit(6)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (str (:err result) "\n" (:out result)))
    (is (str/includes? source "xmlSubsetLimits:Object.freeze({nodes:2048,depth:32,attributesPerNode:32,pathSegments:32})"))
    (is (not (re-find #"DOMParser|document|fetch|XMLHttpRequest" source)))))

(deftest bounded-decimal-f64-parser-is-finite-typed-and-preserves-negative-zero
  (let [kir {:format :kotoba.kir/v4 :entry nil :exports ['parse]
             :effects #{}
             :functions [{:name 'parse :params ['value] :param-types [:string]
                          :result [:option :f64] :effects #{}
                          :body '(decimal-f64-parse value)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        valid ["0" "-0" "+1.5" "-2.4" ".5" "1." "6.022e23"
               "1e-324" "5e-324" "1.7976931348623157e308"]
        invalid ["" " " "NaN" "Infinity" "-Infinity" "0x10" "1_000"
                 "1e0000" "1e309" (apply str (repeat 65 "1"))]
        valid-js (str "[" (str/join "," (map pr-str valid)) "]")
        invalid-js (str "[" (str/join "," (map pr-str invalid)) "]")
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({});"
                "for(const s of " valid-js "){const v=x.parse(s);if(!v[1]||!Number.isFinite(v[2]))process.exit(2)}"
                "if(!Object.is(x.parse('-0')[2],-0)||x.parse('1e-324')[2]!==0||x.parse('5e-324')[2]!==Number.MIN_VALUE)process.exit(3);"
                "for(const s of " invalid-js "){const v=x.parse(s);if(v[1])process.exit(4)}})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (str (:err result) "\n" (:out result)))
    (is (str/includes? source
                       "decimalF64Limits:Object.freeze({bytes:64,vector3Bytes:194,finiteOnly:true,rounding:'nearest-ties-even'})"))
    (is (not (re-find #"parseFloat|eval|Function" source)))))

(deftest bounded-decimal-f64x3-parser-is-fixed-width-and-atomic
  (let [result-type [:option [:vector [:f64 :f64 :f64]]]
        kir {:format :kotoba.kir/v4 :entry nil :exports ['parse]
             :effects #{}
             :functions [{:name 'parse :params ['value] :param-types [:string]
                          :result result-type :effects #{}
                          :body '(decimal-f64x3-parse value)}]}
        source (script/emit kir)
        encoded (.encodeToString (java.util.Base64/getEncoder) (.getBytes source "UTF-8"))
        js (str "import('data:text/javascript;base64," encoded
                "').then(m=>{const x=m.instantiateKotoba({}),ok=x.parse(' -0  1.5\\t5e-324 ');"
                "if(!ok[1]||!Object.is(ok[2][1],-0)||ok[2][2]!==1.5||ok[2][3]!==Number.MIN_VALUE)process.exit(2);"
                "for(const s of ['', '1 2', '1 2 3 4', '1,2,3', '1 NaN 3', '1 1e309 3', '1　2　3', ' '.repeat(195)])"
                "if(x.parse(s)[1])process.exit(3)})")
        result (shell/sh "node" "--input-type=module" "-e" js)]
    (is (zero? (:exit result)) (str (:err result) "\n" (:out result)))
    (is (str/includes? source "vector3Bytes:194"))
    (is (not (re-find #"parseFloat|eval|Function" source)))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'kotoba.script-test)]
    (System/exit (if (pos? (+ fail error)) 1 0))))
