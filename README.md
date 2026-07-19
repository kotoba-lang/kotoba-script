# kotoba-script

Restricted JavaScript backend for Kotoba. This package accepts only checked
`kotoba.kir/v3` or typed `kotoba.kir/v4` data produced by
`kotoba-lang/compiler`; it does not parse
`.kotoba` source and does not own language semantics.

```text
.kotoba -> kotoba-lang/compiler -> checked KIR -> kotoba-script -> .mjs
```

Generated modules expose `kotobaArtifact` and `instantiateKotoba(grants)`.
They use no ambient browser/Node authority and execute capability effects only
through explicitly supplied grant functions.

Every emitted artifact declares
`floatingPointPolicy: 'ieee-754-f32-f64-v5'`. KIR v4 admits scalar `:f32` and
`:f64` parameters and results. Both profiles provide exact bit conversion,
explicit add/subtract/multiply/divide/negate/absolute operations, ordered
comparisons, and an unordered predicate. Binary32 values are created only by
explicit rounding, integer conversion, or bit construction; decimal literals
remain binary64. Finite values, infinities, and both zero signs preserve their
IEEE-754 bits; NaN payloads are intentionally unobservable and canonicalized
to quiet NaN `0x7fc00000` or `0x7ff8000000000000`. Division follows IEEE-754,
including infinities, NaN, and signed zero. There are no implicit integer/float
conversions, fused operations, remainder, or general transcendentals in
this profile.

`f32-sqrt`/`f64-sqrt` and `f32-min`/`f32-max`/`f64-min`/`f64-max`
propagate NaN. Minimum selects negative zero and maximum selects positive zero
when the operands are opposite zero signs. Binary32 results round after the
named operation.

Qualified `f64-sin-quarter-turn` and `f64-cos-quarter-turn` accept only finite
radians in `[-pi/4, pi/4]`, evaluate fixed degree-17/16 Horner polynomials
without FMA, and guarantee absolute error at most `4e-15`. Inputs outside the
sealed domain trap instead of invoking ambient host transcendental functions.

`f64-sin-bounded` and `f64-cos-bounded` extend that contract to finite
`[-8192*pi, 8192*pi]` using fixed split-`pi/2` range reduction, ties-away-from-
zero quadrant selection, and the same quarter-turn kernels. Their absolute
error bound is `5e-12`; larger or non-finite angles trap.

`f64-exp-near-zero` accepts finite `[-0.5,0.5]` and
`f64-log-near-one` accepts finite `[0.75,1.5]`. Fixed degree-18 Taylor and
degree-21 atanh-series Horner kernels provide a `4e-15` absolute-error bound;
out-of-domain inputs trap and no host `Math.exp`/`Math.log` call is emitted.
JavaScript is the checked output representation, not the source of Kotoba
semantics.

Numeric conversion is never implicit. `i64-to-f64-checked` rejects integers
that binary64 cannot represent exactly, while `i64-to-f64-rounded` explicitly
requests IEEE round-to-nearest conversion. `f64-to-i64-checked` accepts only
finite integral values in signed-i64 range; `f64-to-i64-truncating` explicitly
requests truncation toward zero and still rejects NaN, infinities, and
out-of-range results. The corresponding `i64-to-f32-*` and `f32-to-i64-*`
operations use the same checked-versus-lossy naming. `f64-to-f32-rounded` and
`f32-to-f64-exact` make width changes explicit.

KIR v4 preserves `:i64`, `:f32`, `:f64`, `:string`, `:keyword`, `:bool`, `:option-i64`, and the
first bounded `:map` profile as distinct value types. String
literals must be well-formed UTF-16, are capped at 4,096 UTF-8 bytes each and
65,536 bytes per module, and every runtime string crossing a function or host
boundary is revalidated against the 65,536-byte cap. The backend supports only
`string-concat`, `string=?`, and `string-byte-length`; it does not hash strings
into integers or expose JavaScript property access as language semantics.
Keywords preserve canonical Unicode source text, are capped at 512 UTF-8
bytes, and are never hashed into integers. Maps contain at most 128 unique
keyword keys and signed-i64 values; their host ABI is a canonical frozen array
of `[keyword, bigint]` entries. `map-assoc` returns a new frozen value and does
not mutate its input. Nested or mixed-value maps remain fail-closed until a
later explicitly typed profile owns them.

Booleans cross the host boundary only as JavaScript `true` or `false`; numeric
truthiness is not accepted. The first option profile is deliberately
`option<i64>` rather than an unbounded generic container. Its canonical host
ABI is the frozen tagged array `[false]` for none or `[true, bigint]` for some.
JavaScript `null`, `undefined`, integer sentinels, malformed tags, and payloads
outside signed i64 fail closed. `option-value` evaluates its fallback only for
none. The first algebraic-result profile is likewise deliberately closed:
`result<i64,i64>` uses the frozen tagged array `[true, bigint]` for ok and
`[false, bigint]` for err. Both payload positions are signed i64,
malformed/missing payloads fail closed, and `result-value` / `result-error`
evaluate their fallback only for the opposite variant. This is a monomorphic
ABI foundation, not yet a claim of generic ADTs.

Parametric results use the canonical recursive type descriptor
`[:result ok-type err-type]` and the explicit KIR operations
`result-ok-of`, `result-err-of`, `result-ok?-of`, `result-value-of`, and
`result-error-of`. Descriptors may nest to depth 8 and contain at most 64 type
nodes; runtime payload validation carries the same depth and node budgets.
Every constructor and projection carries its descriptor, so generated code
never guesses a payload type from JavaScript shape.

`result-match-of` is the checked KIR form for exhaustive result matching. It
contains exactly one ok binder/body and one err binder/body; both binders are
typed from the descriptor, both branch result types must agree, and generated
code validates the result before evaluating exactly one branch.

User-defined finite variants use
`[:variant :qualified/type [[:case payload-type] ...]]`, with 1--32 unique
cases inside the shared depth-8/node-64 type budget. Canonical host values are
frozen `[descriptor, ":case", payload]` arrays. The full descriptor is part of
the value identity and is structurally revalidated, preventing a same-named
case from another schema or module from being substituted. `variant-match`
must list every declared case exactly once and in declaration order; there is
no wildcard that can silently absorb later schema expansion.

Generic options use `[:option payload-type]`. Canonical host values are
`[descriptor, false]` for none and `[descriptor, true, payload]` for some, so
even payload-free none values retain exact type identity. Constructors,
projection, and exhaustive none/some matching carry the descriptor explicitly;
null, undefined, malformed tags, cross-option substitution, and eager fallback
evaluation remain outside the language ABI.

Fixed heterogeneous vectors use `[:vector [item-type ...]]` with at most 32
positions inside the shared depth-8/node-64 descriptor budget. Their canonical
host value is `[descriptor, item ...]`: descriptor identity, exact length, and
every position's declared type are revalidated at each export boundary.
Construction must supply every position exactly once; projection and persistent
replacement use an admission-time in-range integer index, so their result or
replacement type is statically determined. Equality first validates both full
values against the same descriptor and then compares every canonical nested
value structurally; JavaScript object identity is never observed. Dynamic indexes, sparse values,
append/drop operations, and host mutation are not admitted by this profile.

Typed sets use `[:set item-type]` and at most 32 values inside the shared
depth/node budget. Their canonical host value is `[descriptor, items]`, where
items are recursively validated, uniquely sorted by a language-owned total
order, and frozen. Constructors reject duplicates instead of silently losing
input. Membership, idempotent insertion, removal, count, and equality operate
on canonical values; updates never mutate their input. The total order covers
every currently admitted scalar and structured value type, so neither
JavaScript insertion order nor object identity participates in set semantics.

Nominal bounded records use
`[:record :qualified/type [[:field field-type] ...]]`, with 1--32 unique
keyword fields in declaration order under the shared depth/node budget. Their
canonical host value is `[descriptor, field-value ...]`; the complete nominal
schema, exact arity, and every field value are revalidated at each boundary.
Construction supplies every field exactly once in declaration order. Field
projection and persistent replacement require an admission-time declared
keyword literal, making their types static and excluding dynamic property,
prototype, sparse-object, and unknown-field behavior. Equality is structural
only after exact descriptor validation.

The first sequential collection profile is `vector<i64>`, bounded to 128
items. Its host ABI is a frozen JavaScript array whose elements are revalidated
as signed i64 at every exported boundary. `vector-get` has a lazy explicit
fallback; `vector-assoc` only replaces an existing index; `vector-conj` fails
at capacity. Both updates return new frozen arrays and never mutate the input.

Run tests with `clojure -M:test`.
