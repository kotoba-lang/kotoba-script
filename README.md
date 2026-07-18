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

KIR v4 preserves `:i64`, `:string`, `:keyword`, `:bool`, `:option-i64`, and the
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
none.

The first sequential collection profile is `vector<i64>`, bounded to 128
items. Its host ABI is a frozen JavaScript array whose elements are revalidated
as signed i64 at every exported boundary. `vector-get` has a lazy explicit
fallback; `vector-assoc` only replaces an existing index; `vector-conj` fails
at capacity. Both updates return new frozen arrays and never mutate the input.

Run tests with `clojure -M:test`.
