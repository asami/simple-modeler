# Built-in structured data types

HISTORICAL prerequisite validation snapshot — 2026-10-05:

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

## Executable specification and historical prerequisite acceptance — 2026-10-05

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


## Current reader and external projection delegation — 2026-10-18

This section governs the current Phase 115 producer contract. Earlier dated
validation evidence remains historical; its success does not establish this
implementation or generated runtime acceptance. The earlier Record-specific
admission description is superseded by the canonical readers specified here.

The generator selects declared types, cardinality, metadata field names and
ordered aliases. Core `ValueReader` owns generic collection reading and canonical
structured normalization. Scalar Record admission delegates through
`_record_get_as_c[Record]`; ordinary repeated Record admission selects the whole
`scala.Vector[org.goldenport.record.Record]`. The canonical Record reader applies
`XSemiStructuredData` meaning, including its nested object rules. Declared Json
always selects `ValueReader[io.circe.Json]`, preserving raw Json shape and null
values rather than interpreting it as a Record. Custom application classes
named Json or Record retain their full declared identity.

A declared `NonEmptyVector[A]` is read as that complete type, including Record
and Json leaves. Core owns strict nonempty admission: present empty or malformed
input fails; missing input alone retains the generated required-property or
existing default branch. This policy applies to Persistent, Create, Update and
Query without reducing their declared cardinality. The ordinary generic vector
compatibility helper alone explicitly selects
`ValueReader.vector(elementReader, allowSingle = true)`. Its contextual variant
passes the identical execution context through the contextual alias getter.
Canonical Record vectors and application-record vectors use strict collection
readers and do not inherit scalar promotion.

Application-record vectors compose Core `ValueReader.recordReader[A](decode)`
with strict `ValueReader.vector`. ClassTag-based typed admission and Record
callbacks belong to Core; the generator emits that thin helper only when a
selected application collection or ContentReferenceOccurrence requires it.
Conventional composite generated value readers likewise delegate to
`recordReader[DeclaredClass](createC)`. Nominal, single-scalar and powertype
constructors retain their declared-domain and database parsing branches.

Explicit nested legacy declarations retain their selected child types and
List/Seq wrapper mapping. Their emitted helpers forward callback-based element
readers into strict Core Vector or NonEmptyVector readers. Canonical Record
leaves select Core's Record reader, application leaves select an explicit
`recordReader` with the concrete class's `createC`, and other leaves use their
existing declared reader. Alias traversal remains generated metadata selection;
shape dispatch, element traversal, null policy and nonempty construction are
library responsibilities. Unsupported declarations remain rejected.

External Record projection delegates to
`org.simplemodeling.model.value.ModelValueEncoder.encodeC(value).TAKE`. Core
owns recursive external projection, ordered collection semantics and raw Json
identity; the model library adds explicit model groups, nominal unwrapping and
typed Condition/Update projection. Unsupported values and non-String Map keys
retain structured failures rather than arbitrary `toString` output. Existing
public `toRecord` and `toDataStore` signatures retain their throwing projection
surface at `TAKE`; no new public consequence-returning Record API is introduced.

Storage-specific generated `toDataStore` cases, permission/media SQL encodings
and the mapping ABI retain their existing owners. CNCF retains declared storage
mapping, persistence stages and optional Update clear semantics; external
projection does not introduce SQL clear or persisted-format policy.

Current adoption status — 2026-10-18: the selected Modeler delegation repair
passed 33/33 in three suites and its unchanged 1.1.26-SNAPSHOT local refresh
succeeded. Forced model regeneration compiled actual 11 products, inspected their
thin delegates and passed 54/54 model tests. CNCF's selected connected consumer
runs now cover ordinary CML regeneration, generated compilation/persistence and
actual `Query.createC`/Condition Vector application reconstruction. The runtime
`Phase115GeneratedCollectionPolicySpec` also covers an actual generated
ScalarCode115 DATATYPE; earlier CountryCode/PostalCode VALUE products do not
prove nominal scalar semantics. These selected focused results retain their
original scopes and do not retroactively close the historical comparison finding.
Independent complete protected focused RCP-CONSUMERS Step review passed.
Phase 115 adoption is CLOSED and all five RCP obligations are DONE. Both
connected Steps were independently reviewed and committed locally; the sole
original Phase full review, Modeler repair1 focused closure and combined
repair2–6 focused closure passed. The selected Modeler full gate passed
118 tests in 21 suites, zero failed/ignored/pending/aborted, with 3 existing
canceled cases. Its original 117/118 full result remains a failure; the existing
delimiter oracle repair changed no generator behavior. The
[Phase acceptance](../../../cloud-native-component-framework/docs/phase/phase-115.md#final-phase-acceptance--2026-10-18)
and [checklist](../../../cloud-native-component-framework/docs/phase/phase-115-checklist.md)
record the four selected full gates and retained history. Closure takes effect
through the local release commit containing `phase115-final-closure-20261018-01@1`.
Earlier dated pending/execution receipts retain their
historical subjects.

## Ordinary domain value operation inputs — 2026-10-19

An operation parameter whose actual model object is `MDomainValue` carries its
ordinary `ValueScalaModelTransformer` plain product in Scala `Parameter.value`.
This applies through the existing typed-object body recursion. The value's
effective attributes retain their declared order, semantic types and
multiplicities, including ordinary scalars, optional fields and repeated fields.
A required Json field carries one `io.circe.Json`; the document may itself be a
JSON array without changing the declared field multiplicity.

Recursive operation schema lookup uses this actual input carrier to correlate
metadata fields with their typed declarations and select `XJson` or
`XSemiStructuredData`. A custom domain value named `Json` or `Record` remains
its declared class and its String attributes remain String. Class spelling
does not select a structured semantic type. An opaque Record reference has
no invented attribute carrier. Existing entity-value Create, Update and other
family transformations retain precedence and their existing behavior.

Named command action projection selects the carried value's ordered leaf
parameters only when operation metadata explicitly declares `COMMAND_VALUE`
(trimmed and normalized with the root locale), the action wrapper has exactly
one parameter carrying an actual value, that value explicitly has plain purpose,
its class kind is not EntityValue, and its class name matches the selected
metadata input type's existing action class name. The carried parameter sequence
must be nonempty. The generated named command then uses these same declared
fields and the existing canonical field admission paths, without a nested
`record: <InputType>` or a self-call to `<InputType>.createC`.

Unmatched metadata names or kinds, Query Values, empty carriers, entity-value
families, opaque Record inputs and wrappers with additional explicit parameters
retain the existing action projection. Semantic purpose and actual model
identity govern this selection; class spelling never supplies datatype meaning.
Operation metadata retains its existing request order, multiplicity and field
metadata authority.

[ValueInputOperationProjectionSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/generator/scala/ValueInputOperationProjectionSpec.scala)
authors actual component transformation and named command generation, carrier
identity/order/cardinality properties and compatibility controls. It supplies
producer evidence only after parent-owned execution. Acceptance also requires
the Cozy parse/model/generation consumer, the original server's Scala 3
compilation and the owning Phase release validation. All remain pending for
this repair; no success is inferred from historical validation above.

## Named command structured leaf admission — 2026-10-19

Within the existing selected `COMMAND_VALUE` leaf projection, required Json and
Record fields use keyed `Consequence.successOrRecordNotFound` with the canonical
qualified runtime type. A named Record leaf contains only its document, excluding
the surrounding request and sibling fields. The opaque Record action/query path
retains its existing whole-request meaning outside that projection.

Optional structured leaves use `request.toRecord.getAsC[Leaf](key)`: omission is
`None`, present Json.Null is `Some(Json.Null)`, and an invalid present value fails.
Json.Null is not a valid optional Record document. Zero-or-more leaves select
`getAsC[scala.Vector[Leaf]]` and default only missing input to `Vector.empty`.
Nonempty leaves select `successOrRecordNotFound[cats.data.NonEmptyVector[Leaf]]`.
The existing Core readers own ordered element normalization, strict sequence
admission and empty/nonempty checks. No reader wraps a single document or the
whole request into a collection. A scalar Json array remains one Json document.

Recognition uses actual structured semantic identity or the exact canonical
`org.goldenport.record.Record` reference, with one declared Option, Vector or
NonEmptyVector container. Custom classes named Json or Record keep their own
constructor paths. Schema, field order, metadata and public AST contracts retain
their existing owners. The internal selection context applies only when the
existing command-value descriptor gate succeeds.

The Modeler executable specification checks selected reader generation and
compatibility controls. Actual generated Scala 3 constructor behavior is authored
by the downstream Editing Studio test-only StructuredAdmissionFixture CML and
StructuredCommandAdmissionSpec through normal pinned Cozy generation. That
consumer supplies runtime admission evidence only after parent-owned execution.
