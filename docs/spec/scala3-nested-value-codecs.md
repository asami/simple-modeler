# Scala 3 nested value codecs

A generated domain value product can derive a Circe `Codec.AsObject` when every
field passes the existing scalar and runtime-type restrictions and each nested
generated product resolves to a domain value in the generation context. The
transitive graph must be acyclic. Required fields and optional, ordered vector,
and nonempty vector fields are supported; ordered values retain their order in
JSON arrays. Each participating product derives its own typed codec.

A missing declaration, a recursive reference, a nested entity,
nominal scalar, shared model value, external runtime object, or Record prevents
derivation. Existing exclusions for these shapes and scalar handling remain in
force. This contract does not introduce a fallback codec or flatten a domain
value. Entity-family derivation retains its existing generated-model exclusion.

The generator specification exercises admission and refusal using model context.
The Scala 3 fixture project compiles actual generated products and checks typed
encode/decode roundtrips as part of the root full test task.
