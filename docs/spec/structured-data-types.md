# Built-in structured data types

Status: authored and focused generator validation passed on October 5, 2026;
independent complete producer review pending.
Scope: the Phase 78 declaration producer for CNCF Phase 100 mapping rules R1,
R3, R4, R6 and R12. Authoring closes no Step, Phase or checklist item.

## Declaration and semantic identity

`MBuiltinStructuredDataType` is a Scala 2.12-compatible sealed semantic type
with exactly `Json` and `SemiStructuredData` companion case objects. It is
distinct from the existing complex user-defined `MStructuredDataType`.

`MBuiltinStructuredDataType.get(spelling)` trims and case-normalizes an explicit
declaration using the root locale. It recognizes these aliases:

| Semantic object | Accepted spellings | Logical identity | Runtime class | Generated schema |
| --- | --- | --- | --- | --- |
| `Json` | `json`, `xjson` | `json` | `io.circe.Json` | `org.goldenport.schema.XJson` |
| `SemiStructuredData` | `record`, `semistructureddata`, `semi-structured-data`, `xsemistructureddata` | `semiStructuredData` | `org.goldenport.record.Record` | `org.goldenport.schema.XSemiStructuredData` |

Each semantic object supplies Description, runtime package/class, package
affiliation and empty constraints. Unknown spellings return `None`.
`MAttributeType.create` selects these objects first and otherwise retains
`MDataType.create`. Declaration normalization never parses or guesses payloads.
A declared String remains String even when its eventual content resembles JSON.

## Transformation and generation

`ScalaModelTransformer` selects `TypeName.create(MBuiltinStructuredDataType)`.
The result is `TypeName.Structured(datatype)`, retaining the selected object;
its `name` and `fullName` describe the explicit runtime class. It is required
and platform-provided. `TypeName.record` denotes the semi-structured object.

Optional, repeated and nonempty multiplicity wraps this node in the existing
Option, Vector and NonEmptyVector containers. Update and Condition wrappers
retain the child, including repeated children. Import collection uses a Plain
runtime import adapter; component relative-name presentation uses the runtime
full name. Neither adapter replaces nodes in parameter or attribute trees.

Schema generation peels containers and selects the canonical Core schema
object by the typed structured node. The exact legacy Plain reference
`org.goldenport.record.Record` also emits `XSemiStructuredData`. A custom Plain
`Json` or `Record` in another package remains a custom declaration; a simple
class name never selects either canonical type. Selected structured declarations
emit neither `DataType.Named("json")` nor `DataType.Named("record")`.

Component update request schemas correlate each operation metadata field with
the same operation's actual typed input parameters using the existing normalized
contract name. This schema lookup recursively visits nested value parameter
sequences, retaining canonical Record leaves even when the separate request
additions routine excludes opaque Record parameters. Only `TypeName.Structured`
or the exact legacy Plain `org.goldenport.record.Record`, possibly wrapped in
containers, selects the typed schema generator. Recognition peels containers;
schema generation receives the original type. Nonstructured and unmatched
metadata fields retain the existing string datatype fallback. Explicit foreign
Plain Json/Record class declarations whose names match that metadata retain their
existing Named class projection, bypassing the name-only structured refusal; they
never select a canonical structured datatype. No public AST fields change.
Operation metadata still determines multiplicity, ordering, required, web hints,
constraints, confidentiality and update descriptors. Additional revision inputs
and explicit Record/DeclaredResult responses retain their existing projection.

Builder optionalization preserves the structured child. JSON admission uses
Core `ValueReader[io.circe.Json]`; Record retains the existing Record-specific
admission path and canonical reader where already selected. JSON receives no
invented `createC` or `fromRecord` method. The generator source imports no
Scala 3 Core implementation; emitted Scala 3 symbols reference canonical Core.

## Executable specification and pending acceptance

[StructuredDataGenerationSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/transformers/scala/StructuredDataGenerationSpec.scala)
authors Given/When/Then behavior for aliases and fallback, actual semantic
MDomainResource transformation, recursive family container identity, imports,
real Scala3EntityFamilyGenerator products, canonical schema/readers and custom
class-name collisions. These checks establish generator authoring coverage
only after parent-owned execution succeeds.

[EntityUpdateOperationContractProjectionSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/generator/scala/EntityUpdateOperationContractProjectionSpec.scala)
extends the retained scalar/revision/result scenario with actual component
transformation and generation for two update variants. It observes individual
canonical, legacy, optional and repeated request fields, metadata authority,
registered custom Json/Record values backed by XString and metadata-only fallback.

Historical producer evidence: the original four focused SimpleModeler suites
passed 17/17 before this operation-schema repair, and the prior local SNAPSHOT
refresh succeeded. Cozy's subsequent focused run passed 31/32 and failed
`MODELER-STRUCTURED-OPERATION-SCHEMA-001`: actual update request fields still
emitted Named structured identities. Those receipts remain historical and do
not validate the new repair, authored October 5, 2026. The SimpleModeler a4 test
syntax and a5 observer-path failures were corrected without changing expected
behavior; neither failed attempt counts as accepted validation.

Current focused evidence on October 5, 2026: invocation
`tkw-simplemodeler-structured-validation-20261005-a6` passed 20/20 tests in five
completed suites; `tkw-simplemodeler-local-refresh-20261005-a2` successfully
ran `publishLocal` for the fresh exact 1.1.26-SNAPSHOT;
`tkw-cozy-structured-validation-20261005-a2` passed 32/32 tests in two completed
suites. All three commands terminated with SBT and wrapper exits 0 and
`lock=released`. This evidence establishes focused generator validation only;
independent complete producer review and all full acceptance gates remain
pending. No Step, Phase or checklist item closes through these results.

Required pending work includes independent complete producer review,
generated compilation against selected Core/CNCF dependencies, field
codec metadata after its ABI freeze, external-format attribute projection,
application-class and array storage policy, provider and process-restart
integration, both Workbench proposal fixtures, and complete CNCF Phase 100,
Cozy Phase 78 and Workbench acceptance. No storage/provider behavior or runtime
roundtrip acceptance follows from generated source checks.

Generated support code may explicitly use the canonical Record JSON bridge
inside a private typed codec without changing a custom field's datatype. The
custom Json/Record isolation specification therefore checks actual schema and
storage-binding expressions, alongside the exact semantic Plain types and
Named class declarations, rather than banning the type's spelling everywhere.
