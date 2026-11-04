# UserAccount generation against current CNCF

## Scope

The ordinary SimpleModeler Scala 3 producer must preserve UserAccount's existing
required `expiresAt: java.time.Instant` contract while targeting CNCF
`0.5.3-SNAPSHOT`. The producer development coordinate is `1.1.27-SNAPSHOT`.
This contract covers producer source and its selected generated-runtime checks.
Actual backend publication, Cozy selection, UserAccount CAR delivery and composed
Studio authentication acceptance remain consumer integration obligations.

## Required shared expiry

A required, non-derived semantic attribute named `expiresAt`, with declared type
`java.time.Instant` and canonical storage spelling `expiresAt` or `expires_at`,
refines the SimpleEntity framework resource lifetime. This classification is a
derived property of the existing typed declaration; model case-class constructor
signatures are unchanged. Optional expiry, a different type, a different framework
field or a noncanonical storage alias does not acquire this refinement.

The public persistent and creation field remains a required Instant. Update
retains `Update[Instant]`; Query retains `Condition[Instant]`. The framework
`resourceAttributes.expiresAt` is derived at UTC from that Instant, preserving
nanoseconds and the exact instant. The existing resource-group constructor input
is retained as a private seed under `resourceAttributesInput`; its supplied expiry
must agree with the public value. It does not supply an independent lifetime.
Generated builders retain their resourceAttributes vocabulary. Generated
`withResourceAttributes` updates the required Instant together with the resource
group, and `withExpiresAt` updates the derived group without retaining a stale
expiry seed. A group setter cannot clear the required expiry; Update group Noop
retains the current directive and SetNull fails. Temporal consistency uses typed
Instant comparison with nanosecond precision. Resource-group aliases compare
all three optional timestamps by instant and activation status by its declared
code, accepting equivalent timezone representations without global CanEqual.
The generated
resource-group constructor argument is named resourceAttributesInput for refined
entities; this does not change the producer's ScalaModel binary constructors.

There is one required declared storage binding, with logical name `expiresAt`
and canonical default storage name `expires_at`. An explicit supported canonical
storage declaration remains authoritative. Framework flattened expiry is omitted
from both the logical framework row and legacy physical/view projections when
the refinement owns it. Required domain metadata and SQL NULL policy remain
unchanged. Other collisions continue to fail generation.

Creation and persistent load require an expiry. Readers validate every supplied
canonical flat alias and resource-group representation before accepting them.
Missing or malformed expiry, a different instant across aliases/groups, or a
required clear directive fails. Equal instants expressed at different offsets
remain equal. A historical row using canonical `expiresAt`/`expires_at` remains
readable; physical encode/decode and a new admission of that row preserve the
instant. Query input remains optional as a filter but validates supplied expiry;
Update Noop preserves absence while a selected update projects to the same
framework lifetime and storage field.

## State-machine linkage

The existing account graph contains exactly these edges, in declaration order:

| Source | Event | Target |
| --- | --- | --- |
| provisional | promoteToFormalRegistration | formal |
| provisional | suspend | suspended |
| registered | suspend | suspended |
| formal | suspend | suspended |
| suspended | restore | registered |

Generated plans use Vector `transitionActions`, together with existing ordered
exit/entry vectors. A missing transition action emits an empty vector; a selected
action emits one element. Actions implement the current
`ExecUowM[ActionExecution]` program ABI. A legacy unbound script fails through a
typed notImplemented program; it cannot masquerade as successful execution.
There is no new script interpreter or bypass of native binding/authorization.

Canonical transition bindings, operation invocation identity, ordering, guards,
terminal topology, normalized history metadata and the existing guard resolver
remain authoritative. An event name alone cannot authorize an Operation rule.

## Executable specification and evidence boundary

`EntityStoreMappingGenerationSpec` E17 compiles actual generated Scala 3 entity
families using the selected public Dotty compiler entrypoint and selected CNCF
runtime artifacts, isolated from the Scala 2 producer. The semantic fixture uses
Cozy's CML `instant` projection, an explicit `java.time.Instant` object reference;
it does not use the legacy datatype-name registry. The compiler loader uses the
union of selected compiler and runtime artifacts so Dotty can load its Scala
3/2.13 runtime dependencies while its platform parent excludes producer Scala
2.12 classes. Its property checks vary
future seconds and nanoseconds and execute public typed creation, required/malformed/
conflict admission, storage encode/decode, historical-row re-admission, Query
and Update behavior, including public expiry/group mutators, equivalent timezone
aliases and required group-clear rejection. The same generated creation/persistence APIs create and save
through StandardEntityStore's public revision-bearing save API and SQLite.
Each provider context registers the collection bootstrapped from the fixture's
ordinary generated component descriptor with namespace-qualified identity
`org.example.proposal.Expiry`, including SimpleEntity's Embedded
revision metadata. Native create therefore initializes managed revision, and
revision-bearing save must advance it to two; a bare unregistered context does
not substitute for that runtime model binding. Then a closed provider is reopened and
a new store loads the exact expiry. No storage/security/revision guard is disabled. Because each property sample
executes real provider operations, samples do not shrink. The property retains
at least twenty successful samples over the same future-second/nanosecond ranges
and reports its reproducible initial seed, exact failing input, named fact or
original probe exception with its cause. A final Boolean assertion must not hide
ScalaCheck's retained failure. SQL factory, bootstrap, create, load and save failures
also retain their stage and structured Conclusion display/causes. Probe exceptions
report a bounded cause chain of up to thirty-two levels, including initializer
and reflection causes, with explicit cycle/limit markers and source locations;
initializer errors must not collapse to a null-only summary. Runtime/compiler inputs are newline-separated existing
artifact paths, selected through `textus.sm.cncf.runtime-classpath-file` and
`textus.sm.scala3.compiler-classpath-file`. When
`textus.sm.generated-runtime.required=true`, absent inputs are a test failure.
Fixtures are emitted under ignored target output; generated source is never
patched, and no nested SBT process is started.

`ComponentStateMachineHistoryGenerationSpec` compiles the ordinary component
adapter and five-edge fixture, then executes the native structural planner with
explicit operation invocation identities. It checks selected binding linkage,
invalid targets and missing invocation rejection. A separate generated action
fixture executes its program through the public interpreter interface and proves
that an unbound script fails. Existing history/normalization specifications remain
part of the selected focused suite.

These are authored executable obligations, not a record of successful checks.
Native compile/focused validation, independent integrated review and scoped local
commit remain necessary. Source fixture success does not establish actual Cozy
consumption, full UserAccount authentication lifecycle, CAR delivery or Studio
HTTP acceptance.
