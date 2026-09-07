(ns test.nbb.parity
  "JVM-free parity test for the ported emitter (`src/kotoba/script.cljc`).

  Two claims, each against a golden the JVM emitter wrote
  (`scripts/gen-parity-golden.clj`, `test/fixtures/parity/`):

  1. `emit` on nbb produces the SAME BYTES as `emit` on the JVM for every KIR
     fixture the JVM test namespace exports (with the fixture's `.opts.edn`
     as emit options when present);
  2. `java-double-string` reproduces `Double.toString` for every f64 in
     `f64.edn`, given as raw bits so the fixture cannot be mis-read.

  And one claim the JVM tests already make, re-made here because the cljs
  verifier is a different instrument: the emitted module runs under Node and
  answers 42n, and a module carrying an ambient global is REFUSED by the token
  scan. Run from the repo root: `npm run test-nbb` (or `nbb test/nbb/parity.cljs`).
  The last line is `SCANNED <n> failed <k>`; SCANNED 0 is exit 2, not a pass."
  (:require ["node:fs" :as fs]
            ["node:child_process" :as cp]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [kotoba.script :as script]))

(def dir "test/fixtures/parity")
(def failures (atom []))
(def scanned (atom 0))

(defn- fail! [what detail] (swap! failures conj [what detail]) (println "FAIL" what detail))
(defn- ok! [what] (println "ok  " what))

(defn- bits->double [hex]
  (let [buf (js/DataView. (js/ArrayBuffer. 8))]
    (.setBigUint64 buf 0 (js/BigInt.asUintN 64 (js/BigInt hex)))
    (.getFloat64 buf 0)))

;; The generator writes whole-number doubles (`-0.0`, `##Inf`) as
;; `#kotoba.parity/f64-bits "<hex>"`: read as plain EDN the cljs reader turns
;; `-0.0` into the integer 0, which the emitter would then type as i64.
(def ^:private fixture-readers {'kotoba.parity/f64-bits (fn [hex] (bits->double (str "0x" hex)))})
(defn- read-fixture [path] (reader/read-string {:readers fixture-readers} (fs/readFileSync path "utf8")))

;; 1. emit parity -- every `<name>.kir.edn`, with `<name>.opts.edn` as emit
;; options when the JVM fixture var carried `^{:emit-opts …}`.
(def golden-count (atom 0))
(def golden-bytes (atom 0))
(doseq [f (sort (js->clj (fs/readdirSync dir))) :when (str/ends-with? f ".kir.edn")]
  (swap! scanned inc)
  (swap! golden-count inc)
  (let [name (subs f 0 (- (count f) 8))
        kir (read-fixture (str dir "/" f))
        opts-path (str dir "/" name ".opts.edn")
        opts (when (fs/existsSync opts-path) (read-fixture opts-path))
        golden (fs/readFileSync (str dir "/" name ".mjs") "utf8")
        _ (swap! golden-bytes + (count golden))
        emitted (try (if opts (script/emit kir opts) (script/emit kir))
                     (catch :default e (str "THREW " (ex-message e) " " (pr-str (ex-data e)))))]
    (if (= golden emitted)
      (ok! (str "emit parity " name " (" (count golden) " bytes)"))
      (let [i (or (some (fn [i] (when (not= (.charAt golden i) (.charAt emitted i)) i)) (range (min (count golden) (count emitted)))) (min (count golden) (count emitted)))]
        (fail! (str "emit parity " name) (str "first difference at byte " i ": jvm=" (pr-str (subs golden (max 0 (- i 30)) (min (count golden) (+ i 40)))) " nbb=" (pr-str (subs emitted (max 0 (- i 30)) (min (count emitted) (+ i 40))))))))))

;; 2. Double.toString parity. Bits arrive as hex strings so the EDN reader
;; cannot round them through a double. Finite non-zero values go through
;; `java-double-string` directly (a whole-number double in HAND-BUILT KIR is
;; read as i64 on cljs -- documented on `f64-value?` -- so `f64-literal` is not
;; the seam to test here); NaN and the infinities go through `f64-literal`.
(let [pairs (reader/read-string (fs/readFileSync (str dir "/f64.edn") "utf8"))]
  (doseq [[hex expected] pairs]
    (swap! scanned inc)
    (let [d (bits->double (str "0x" hex))
          got (try (script/java-double-string d) (catch :default e (str "THREW " (ex-message e))))]
      (if (= expected got)
        (ok! (str "f64 " expected))
        (fail! (str "f64 bits " hex) (str "expected " expected " got " got))))))
(doseq [[d expected] [[js/Number.NaN "Number.NaN"] [js/Number.POSITIVE_INFINITY "Number.POSITIVE_INFINITY"]
                      [js/Number.NEGATIVE_INFINITY "Number.NEGATIVE_INFINITY"] [0.5 "0.5"]]]
  (swap! scanned inc)
  (let [src (script/emit {:format :kotoba.kir/v4 :entry nil :exports ['v] :effects #{}
                          :functions [{:name 'v :params [] :param-types [] :result :f64 :body d}]})]
    (if (str/includes? src expected)
      (ok! (str "f64-literal " expected))
      (fail! (str "f64-literal " expected) "not found in emitted module"))))

;; 3. the module runs, and the token scan refuses
(let [kir (read-fixture (str dir "/kir.kir.edn"))
      src (script/emit kir)
      tmp (str (.tmpdir (js/require "node:os")) "/kotoba-script-parity-" (.-pid js/process) ".mjs")]
  (swap! scanned inc)
  (fs/writeFileSync tmp src)
  (let [r (cp/spawnSync "node" #js ["--input-type=module" "-e" (str "import('" tmp "').then(m=>{const x=m.instantiateKotoba({});console.log(String(x.main()))})")] #js {:encoding "utf8"})]
    (fs/rmSync tmp #js {:force true})
    (if (= "42" (str/trim (str (.-stdout r))))
      (ok! "module runs under Node: main() = 42")
      (fail! "module run" (str (.-stdout r) (.-stderr r))))))
(doseq [[label bad expect] [["ambient global" "export function f(){return globalThis;}" :ambient-global]
                            ["forbidden property" "export function f(a){return a.__proto__;}" :forbidden-property]
                            ["dynamic import" "export function f(){return import('x');}" :dynamic-import]
                            ["string literal is data, not code" "export function f(){return 'globalThis.__proto__';}" nil]]]
  (swap! scanned inc)
  (let [outcome (try (script/verify-output! bad) :accepted (catch :default e (:kind (ex-data e))))]
    (if (= expect (if (= :accepted outcome) nil outcome))
      (ok! (str "verifier " label " -> " (or expect "accepted")))
      (fail! (str "verifier " label) (str "expected " (or expect "accepted") " got " outcome)))))

(println (str "GOLDENS " @golden-count " bytes " @golden-bytes))
(println (str "SCANNED " @scanned " failed " (count @failures)))
(.exit js/process (cond (zero? @scanned) 2 (seq @failures) 1 :else 0))
