# EntityStore mapping generation

Status: SGEN01–05 source and E1–E6 executable specifications authored on
October 5, 2026; current declared-store focused validation has passed:
SimpleModeler 26/26 in six suites, exact unchanged 1.1.26-SNAPSHOT local refresh,
and Cozy 32/32 in two suites on October 6, 2026 (attempt
`tkw-store-generation-cozy-validation-20261006-a4`, subject 7). Independent
complete fifteen-path protected prerequisite review remains pending. This SGEN07
documentation completes the frozen producer description; it establishes no
generated runtime, Step, project or Phase acceptance.

Owner: SimpleModeler declaration producer for Cozy Phase 78 and CNCF Phase 100;
Workbench Phase 1 / Step 2, DECLARED-STORE-GENERATION. Consumer authority remains
CNCF's DataStore logical type mapping, EntityStore field/row mapping,
persistence stages and native scalar mapping specifications. The rules below
describe projection into that frozen ABI and preserve its policy ownership.

## Collection adoption boundary — 2026-10-18

The user-selected shared RULE.md and CNCF
`docs/rules/collection-and-semi-structured-data.md` govern current collection
representation. Ordinary materialized multiplicity uses Vector, or
NonEmptyVector for an explicitly nonempty attribute. Existing List/Seq controls
and restoration descriptions below record the retained Phase 100 producer
behavior, not a new ordinary business-model generation contract. An old
declaration alone does not justify adding List restoration.

CNCF Phase 115 owns the coordinated declaration, producer, caller and executable
specification migration with SimpleModeler and Cozy. It must verify generated
signatures and actual reconstruction together; generated files are not patched.
Existing E6 assertions and nested legacy fixtures remain historical behavioral
evidence and do not establish that migration. Phase 100's selected multiplicity
fixture generates Vector. The separate static entity-object List Query finding
`CPB-PHASE100-GENERATED-OBJECT-QUERY-CONTAINER` remains recorded in the original
comparison invalidation; this disposition does not claim it fixed or close that
comparison. See CNCF `docs/phase/phase-115.md` and
`docs/journal/2026/10/2026-10-18-phase100-phase115-collection-boundary.md`.

## R1: Declaration authority and original capture

Explicit semantic attributes determine logical names, types, multiplicity and
storage names. `EntityStoreDeclaration(typeName, multiplicity)` captures the
original declaration during Scala model transformation and survives parameter
and attribute copies. Update constructor types may unwrap an optional value
inside `Update`; this does not erase the original optional storage declaration.
Persistent and Update mappings use the captured declaration when present;
Update without a capture peels its existing Update wrapper. Create uses its
generated constructor type so an auto-complemented required field that becomes
optional at Create retains the existing default behavior. The original required
declaration remains captured and the Persistent domain remains required.

Each ordinary field emits its semantic name as `logicalName` and its explicit
`dbColumnName`, or the semantic name when no alias exists, as `storageName`.
Payload syntax, SQL types, runtime values and method availability do not select
a declaration. A declared String remains String even if it resembles JSON.
Only typed structured identity or the exact legacy
`org.goldenport.record.Record` selects canonical structured binding; custom
classes named Json or Record retain their full application identity.

Persistent, Create and Update each emit their own complete ordinary-field
`EntityStoreRowMapping` and expose `Some(_store_row_mapping)` through
`storeRowMapping`. Query has no declared-store mapping or new persistence hook.
Read and query family generation retain their existing roles.

## R2: Explicit bindings and compatible domains

Each `EntityStoreField` contains both names, a canonical Core `ValueDomain`
and a compatible explicit `EntityStoreFieldBinding`. Selection is declaration
projection, with no codec registry, reflection-based inference or payload guess.

| Declared leaf | Domain datatype | Binding |
| --- | --- | --- |
| String | `XString` | `string` |
| Built-in Json | `XJson` | `json` |
| Built-in semi-structured data or exact legacy Record | `XSemiStructuredData` | `semiStructuredData` |
| Supported native text type, including Core Name | Existing canonical schema datatype, including `DataType.Named("name")` | `nativeScalar[A](datatype, Text, identity)` |
| Boolean | `XBoolean` | `nativeScalar[A](datatype, Boolean, identity)` |
| Byte/Short/Int, Long, Float, Double, BigInt, BigDecimal | `XInt`, `XLong`, `XFloat`, `XDouble`, `XInteger`, `XDecimal`, respectively | `nativeScalar[A](datatype, Number, identity)` |
| Declared application class | `DataType.Named(fullClassName)` | `application[A](identity)` |
| Supported ordered collection | Child datatype | Recursive `array(childBinding)` |

Native and application bindings emit identity `declared:<fullClassName>` and
explicit `summon[scala.reflect.ClassTag[A]]`, `summon[io.circe.Encoder[A]]` and
`summon[io.circe.Decoder[A]]` captures. Application binding does not replace a
canonical native domain; the native alternative preserves that supplied domain.
Scalar, Option, zero-or-more ordered collection and NonEmptyVector project
`Multiplicity.One`, `ZeroOne`, `ZeroMore` and `OneMore`. Option retains the
leaf binding; ordered collections retain recursive child bindings and the child
datatype. Empty collections still carry complete metadata.

E4's direct semantic fixture declares `org.example.proposal.value.Summary`.
Mandatory `summary`, optional `optionalSummary` and repeated `summaries` retain
that class and its captures, including the recursive application array child.
The composite value generator's admission accepts an already typed Summary and
the existing Record admission route; family readers retain typed scalar and
vector admission. Generated source references to captures are authoring evidence;
their availability must be proved by compiling generated Scala.

## R3: Complete logical records and raw patch hooks

`_logical_store_record` emits actual `Record(Vector(Field(...,
Field.Value.Single(value)), ...))` for every ordinary field. It preserves typed
String, Json, Record, application objects, None/Some, ordered collections,
empty values and raw Update markers. Ordinary values do not pass through
`Record.dataAuto`, external-format conversion, `_to_data_store_value`, old
physical `toRecord`/`toDataStore`, or `Update.toChangesRecord` in this hook.
Existing framework values alone retain their selected physical conversions.

Persistent, Create and Update override `toLogicalStoreRecord` to return
`Consequence.success(e._logical_store_record)`. Persistent also overrides
`admitLogicalStoreRecord` as identity admission and
`decodeAdmittedLogicalStoreRecord` through `createC(r)`, retaining generated
typed readers and domain admission. Physical decode occurs in the CNCF strict
stage before logical admission; final decoding does not parse physical values
again. Create keeps its existing constructor/default semantics.

Update returns the complete raw logical patch, including Noop, SetValue and
SetNull. CNCF's strict `toStoreChangesRecordC` stage validates raw shape and
complete metadata, calls actual `Update.toChangesRecord` once and performs
partial declared encoding. Noop omits, SetValue unwraps once, optional None
omits, nullable SetNull retains the marker under its storage name, required
SetNull fails, and bare patch null cannot clear. Generation introduces no new
Update dialect or duplicate normalization.

## R4: Framework ownership and temporary physical compatibility

Ordinary attributes receive explicit bindings. For a SimpleEntity parent, the
existing selected flattened framework properties form `physicalFields` and
retain their established conversions; the whitelist is not inferred from
incoming keys. Ordinary fields cannot collide by logical or storage name with
those framework keys. Duplicate framework keys and collisions fail generation.
The whitelist excludes the outer securityAttributes/auditAttributes objects and
ordinary payload fields. Persistent permission projection and Update's selected
owner/marker conversion retain the existing family-specific behavior.

Identity, collection, revision, permission, ContentBody, lifecycle,
authorization, OCC, SQL NULL and transaction policy remain with existing
framework and route owners. Whitelisting does not authorize managed updates.
All legacy physical methods, scalar String projection and collection
compatibility helpers remain temporarily available. They are not logical
sources for the strict declared route and are not the completed provider
integration. Consumer transition and their eventual removal remain CNCF R11
work requiring declared consumer coverage before provider activation.

## R5: Unsupported declarations and missing captured codecs

Projection rejects optional collections/containers, unordered Set, unsupported
containers such as Scala Array, unsupported primitive/declaration shapes and
bounded or disjoint semantic multiplicities with a syntax fault identifying the
field and declaration. The retained producer controls cover Vector, List, Seq
and NonEmptyVector ordered admission and recursive nested bindings. List/Seq
controls describe existing behavior under the collection adoption boundary
above; canonical ordinary generation is the coordinated Phase 115 migration.
This rule adds no resolver-precedence policy.

Every selected native/application leaf requires actual ClassTag, Encoder and
Decoder availability in generated Scala. Missing implicit captures must fail
generated compilation; no stringification, inferred codec or successful fallback
is supplied. CNCF recursively validates complete bindings before hooks and before
empty-array traversal, retaining its missing-mapping/missing-codec-binding and
causal failure contracts. Source inspection cannot prove these runtime gates.

## R6: Executable evidence and mandatory integration gates

[EntityStoreMappingGenerationSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/generator/scala/EntityStoreMappingGenerationSpec.scala)
authors actual semantic transformation and family/value generation:

| Example | Rules | Authored observation |
| --- | --- | --- |
| E1 | R1, R2 | Ordinary logical identities, explicit storage alias and family mapping selection |
| E2 | R1, R3 | Original optional Update declaration and Create auto-complement default |
| E3 | R2, R3 | Complete typed optional/ordered structured rows and logical hooks |
| E4 | R2, R3 | Native captures, declared Summary mandatory/optional/array bindings, typed composite admission and custom Json/Record collisions |
| E5 | R4 | Actual SimpleEntity framework whitelist and retained physical ownership |
| E6 | R5 | Unsupported declarations with field context and supported ordered controls |

Cozy's E7 actual CML route and seven selected products are described in its
`docs/spec/datastore-logical-type-generation.md`. E7 uses
`domain.value.ProposalSummary`, avoiding the existing predefined summary /
I18nSummary alias; E4 remains `org.example.proposal.value.Summary`.

Retained prerequisite evidence is separate: Core 94/94 with independent PASS,
commit `8c0d31e` and exact 0.4.3-SNAPSHOT refresh; earlier structured and
operation-schema producer SimpleModeler 20/20 in five suites, exact
1.1.26-SNAPSHOT refresh and Cozy 32/32 in two suites, with complete twelve-path
independent PASS at ledger revision 13; CNCF 85/85 in eight suites and complete
eighteen-path independent PASS at ledger revision 16. These results do not
validate the new declared-store generator changes.

Historical SimpleModeler 17/17 and prior local refresh, Cozy 31/32 with
`MODELER-STRUCTURED-OPERATION-SCHEMA-001`, and the earlier structured producer's
SimpleModeler a4 syntax/a5 observer-path failures remain retained history. Those
failed attempts are distinct from the successful declared-store a4 below.

Current declared-store lineage retains SimpleModeler
`tkw-store-generation-sm-validation-20261005-a1` syntax, a2 capture/range and a3
Seq failures, fixed before a4 success. Cozy
`tkw-store-generation-cozy-validation-20261005-a1` stopped on capacity rejection
before starting and ran no tests; `tkw-store-generation-cozy-validation-20261006-a1`
test syntax, a2 Name expectation and a3 scalar designation mismatches were
resolved before a4 success. Failed commands remain unaccepted.

Parent-owned current focused evidence is successful: invocation
`tkw-store-generation-sm-validation-20261005-a4`, subject 4, passed 26/26 in six
suites (EntityStoreMappingGenerationSpec plus the accepted five-suite
accumulator); `tkw-store-generation-sm-refresh-20261005-a1`, subject 4,
successfully refreshed exact unchanged
`org.simplemodeling:simplemodeler_2.12:1.1.26-SNAPSHOT` locally. Invocation
`tkw-store-generation-cozy-validation-20261006-a4`, subject 7, passed 32/32 in
two suites on October 6, 2026, including the seven-product E7 fixture and
unchanged explicit runtime descriptor/version options. All three commands
terminated with exit 0 and released locks. Cozy subjects 5–7 changed only test
notation/expectations, so SimpleModeler's 26/26 and exact refresh remain
applicable. Independent complete fifteen-path protected prerequisite review
remains pending; earlier-slice counts and PASS do not substitute for it.

Actual generated Scala compilation against selected Core/CNCF dependencies,
including every declared consumer and its captured codecs, is mandatory before
CNCF R11 provider/default activation. Provider write/read, physical roundtrips
for String/Json/Record/array/application classes, process restart, all native
mutation/readback routes, Collection/seed forwarding, SQL precision/defaults,
recovery and both TKL and Editing Studio Workbench proposal fixtures remain
required. Generator source checks and deterministic comparison do not prove
these behaviors. Full Goal, original bases, epoch 1, Step repair cycle 1,
Phase repair cycle 0, nonconverging count 0 and full Phase review count 0 remain
retained. Separate Workbench, Cozy Phase 78, CNCF Phase 100, Step/project and
Phase acceptance remain pending; no Phase 1.1 work is authorized here.

## Migration continuation examples — 2026-10-17

- **E7:** The retained generated repeated Record Update admission preserves
  its declared Vector/List/Seq container, uses the existing Record reader, keeps
  absent input as Noop, and honors the existing outer SetNull handling. Typed
  application-object collections retain that historical restoration after
  decode. This observation does not require adding restoration to another path
  or accept canonical Phase 115 collection generation.
- **E8:** The generated object collection reader admits elements already of its
  declared application class using an explicit ClassTag. Record elements retain
  the existing `createC` decoder; incompatible elements fail. This admission
  does not select a persistence codec from a runtime type.

These examples extend the existing R2/R3 admission contract. Generated-source
checks alone do not prove compiled consumer behavior or Phase acceptance.

## Declared product codec materialization — 2026-10-17

Actual CNCF Information regeneration exposed missing captured encoders and
decoders for declared product values (108 compilation errors); string-only
source assertions had not established their availability. Generated mapping
companions import standard Circe compile-time product derivation so explicit
full-type captures can materialize product codecs and their typed children.
Existing supplied type codecs remain normal implicit inputs. Classes that
cannot supply or derive an admitted codec still fail compilation.

A private, explicitly typed Record codec uses the canonical
`XSemiStructuredData.toJson/fromJson` bridge; generic product derivation must
never serialize Record's internal implementation fields. Malformed or wrong-root
Record input is a decoding failure. Unsupported Record encoding fails within
the existing application-codec failure boundary rather than returning a fallback
JSON value. This is JSON intermediate conversion, independent of SQL mapping.

**E9:** The actual generated persistence families retain full declared ClassTag,
Encoder and Decoder captures, standard product derivation, and that canonical
Record bridge. None of these supplies a missing logical/storage declaration or
selects a mapping from a runtime value. Actual generated compilation and
roundtrip proof remain required beyond source checks.

## E10 Standard EntityStore framework physical fields

Every generated persistent/create/update mapping declares the closed set of
StandardEntityStore management and ContentBody storage names independently of
incoming row contents. Use the existing CNCF management/security field policy
for its logical/storage spellings, permission/revision, and the explicitly
owned ContentBody metadata spellings. Remove keys already mapped as ordinary
logical/storage fields so each field has one owner. Existing simple-entity
physical properties remain included. Unknown application keys remain errors.
This metadata allows typed reads of rows complemented by EntityStore defaults;
it neither normalizes application values nor changes the policy owners.

## Compiled producer admission — 2026-10-18

Actual Phase 78 fixture compilation fixes the generated source type as well as
the storage binding. XDecimal emits Scala BigDecimal and XInteger emits BigInt;
these are built-in Scala names and require no fictitious SimpleModeling import.
Generated Query/Update for repeated Json reads each element through the declared
Json ValueReader and retains the existing declared-container behavior. Repeated
Record uses the existing Record collection reader in both Query and Update;
container unwrapping must never classify a whole repeated field as one scalar
Record. These observations retain the current producer contract under the
collection adoption boundary above; they do not justify new List restoration. Optional/raw
Update semantics and the captured storage mappings remain unchanged.

E3 and E4 cover these declarations. CNCF Phase 100's compiled CML fixture is the
behavioral integration gate; source checks alone cannot accept the producer.

The framework-managed EntityRevision captures its authoritative DATA_TYPE and
native Number codec, preserving the existing revision owner and native CAS
representation. Flattened framework optional scalars use Record.dataAuto's
existing absence/unwrapping semantics. Content references carry explicit array/product bindings from their framework
field declaration;
the provider does not select JSON conversion from runtime collection values.
Update Builder record input uses the existing Update.toChangesRecord before
element readers, so Set/Noop/SetNull retain the same patch meaning as typed
Builder input.

Existing media collections retain their framework physical-adapter ownership.
This Phase does not introduce a new complete SimpleObject/media codec graph
or claim reconstruction of flattened media fields absent from the existing
Builder reader. Content-reference arrays retain present empty vectors before
ordinary scalar option omission.

Operation request metadata consumes the corresponding semantic method input for
structured fields. If that declaration is absent, the old name-only Named(record)
producer branch is retired: generation rejects the incomplete contract instead
of emitting Named placeholders for built-in structured data. The canonical
structured spelling parser is used only to reject that missing-declaration path.

Semantic object inputs remain candidates before their child fields are flattened,
so a custom class named Json or Record keeps its declared class identity rather
than a lowercased built-in marker. Ordinary metadata-only scalar fields retain
their existing schema fallback. Metadata-only structured fields require a typed
declaration and are covered by the negative generation matrix.

## Framework media and recursive admission

The four existing framework physical media arrays (`images`, `audios`, `videos`,
`attachments`) use an explicit generated physical text adapter. It preserves the
legacy external-value projection as JSON array text, including nonempty arrays
and Update directives. SQL does not infer or encode arbitrary collections.
This does not introduce complete flattened-media read reconstruction or new
model-library codecs.

Nested ordered declarations compose generated collection admission recursively
from their declared container shapes and existing typed leaf readers. Record
leaves remain Records, Json leaves remain Json, and empty inner collections
remain present. Update and query readers use the same recursive conversion.
Programmatic producer fixtures are generated from semantic ASTs by the producer
specification and copied unchanged into the consumer's compiled test fixtures;
they are not hand-authored generated implementations.

The native fixture producer is `EntityStoreMappingGenerationSpec` example E12.
It emits the four `org/example/phase100/nested/entity/{,create/,update/,query/}Proposal.scala`
products under `target/phase100/nested-fixture`. The producer fixture emitter
canonicalizes only trailing line whitespace and the final newline. Copy those emitted products
unchanged to CNCF `src/test/scala` at the same relative paths. The consumer
`Phase100GeneratedStoreSpec` compiles them and exercises actual typed admission
and SQL persistence. Regeneration changes the semantic AST input or generator,
never the copied generated implementation by hand.
