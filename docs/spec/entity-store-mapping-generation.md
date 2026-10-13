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
field and declaration. Supported Vector, List, Seq and NonEmptyVector controls
retain ordered admission; supported nested ordered collections retain recursive
bindings. This rule adds no resolver-precedence policy.

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
