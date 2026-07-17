# kotoba-script

Restricted JavaScript backend for Kotoba. This package accepts only checked
`kotoba.kir/v3` data produced by `kotoba-lang/compiler`; it does not parse
`.kotoba` source and does not own language semantics.

```text
.kotoba -> kotoba-lang/compiler -> checked KIR -> kotoba-script -> .mjs
```

Generated modules expose `kotobaArtifact` and `instantiateKotoba(grants)`.
They use no ambient browser/Node authority and execute capability effects only
through explicitly supplied grant functions.

Run tests with `clojure -M:test`.
