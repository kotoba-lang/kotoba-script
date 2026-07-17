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

KIR v4 preserves `:i64` and `:string` as distinct value types. String
literals must be well-formed UTF-16, are capped at 4,096 UTF-8 bytes each and
65,536 bytes per module, and every runtime string crossing a function or host
boundary is revalidated against the 65,536-byte cap. The backend supports only
`string-concat`, `string=?`, and `string-byte-length`; it does not hash strings
into integers or expose JavaScript property access as language semantics.

Run tests with `clojure -M:test`.
