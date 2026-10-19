# ESA62 ordinary value operation input repair — 2026-10-19

Failure: `VAL-ESA62-OPERATION-JSON-GENERATION-001`.
Boundary: PHASE-6.2 / ESA62-02 / ESA62-02D; frozen repair revision 2.

An actual ordinary `MDomainValue` operation input previously lost its attribute
carrier at `ScalaModelTransformer._from_object`: the generic `MValue` branch
returned no value. Structured request metadata consequently had no typed input
evidence and generation correctly refused the name-only `json` declaration.
The existing plain value transformer now supplies the actual carrier, preserving
the predecessor's partial repair.

The component transformer also wraps operation inputs in an action. For explicit
`COMMAND_VALUE` metadata naming that ordinary input, projecting the wrapper
would emit a same-named nested `record` and constructor. The component generator
now projects the nonempty value's ordered leaf parameters only for the frozen
single-carrier, explicit plain purpose, non-EntityValue and matching-name gates.
Other kinds, mismatches, entity families, empty values and additional parameters
retain their existing projection.

SimpleModeler owns these semantic carrier and generated action/schema choices.
Cozy owns parsing actual declarations into the producer model; its partial work
is not a substitute for this producer repair. Core owns canonical structured
field reading. The new executable specification authors real component
generation and compatibility controls plus a shrinking schema property; it
does not claim payload, runtime or compilation acceptance.

Status: authored, unvalidated. The parent owns focused SimpleModeler execution,
local artifact refresh, Cozy parse/model/generation, original server generation
and Scala 3 compilation, independent review, full release validation and commits.
No Step or Phase is closed by this journal.

Follow-up failure: `VAL-ESA62-GENERATED-JSON-TYPE-REFERENCE-001`. Generated
constructor readers now use the existing semantic type printer for platform
types and their container children. This preserves `io.circe.Json` in readers
as in action declarations, while retaining the existing container mapping.
The generated-output specification now expects qualified readers for scalar,
optional, repeated and required repeated Json fields. Actual consumer generation
and Scala 3 compilation remain parent-owned validation.

## Connected Step structured admission repair — 2026-10-19

The frozen successor `esa62-structured-operation-admission-fix-20261019@1`
addresses `CPB-ESA62-COMMAND-JSON-CARDINALITY-001` and
`CPB-ESA62-COMMAND-RECORD-LEAF-001`. The newly exposed named leaves previously
inherited container-wrapping and whole-request Record constructor branches.
The existing command-value selection now carries an internal context through
private constructor emission. Only that context selects keyed canonical readers
for required, optional, Vector and NonEmptyVector structured leaves.

The updated generator specification authors exact Json and Record keyed reader
oracles, declared nonempty Record fields and existing schema/order/custom/entity/
query controls. Core owns the prerequisite canonical structured Vector carrier
validation and existing ValueReader semantics; Cozy owns actual CML generation.
The server owns the normal test-only generated Scala 3 consumer matrix, including
missing versus null, ordered collections and nested Record sibling isolation.

Status: authored, validation pending. Prior contribution entries remain dated
history. The parent owns focused execution, normal unchanged SNAPSHOT refresh,
actual compiled consumer evidence, independent focused closure review and all
acceptance or commit decisions. No producer Phase closure is claimed.

## Contribution acceptance — 2026-10-19

The preceding authoring/pending entries retain their historical scope. Final
Modeler validation 048 passed 23 tests/4 suites; normal local refresh 049
completed successfully. Actual generated Test compilation 047 failed on
NonEmptyVector display; the bounded correction emits its qualified canonical
type display. Downstream normal generated consumer 052 then compiled and passed
76 tests/4 suites after the separately retained 051 fixture-identity failure
was corrected. Earlier overlapping counts are not additive.

Independent focused result `esa62-structured-operation-focused-result-20261019@1`
is PASS, accepting the limited carrier/named-leaf generation contribution and
resolving both structured-operation blockers. No producer Phase is closed.
App Phase 6.2 stays OPEN/`in_progress`; Step2/3 local commits, sole comprehensive
Phase review, owning final full validation and distinct release remain due.
Original native 001 uninstall/data loss and native 006/host 001 cleanup false
remain. Only 52 October 9 capture files have proven capture recovery; later
audio/configuration/credentials and full data recovery remain unproven.
Existing local naming and publication/domain debt remains separate/nonblocking.
