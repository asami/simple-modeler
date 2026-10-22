# Service-qualified operation generation

Status: normative generation contract

Executable specification:
[ServiceQualifiedOperationGenerationSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/generator/scala/ServiceQualifiedOperationGenerationSpec.scala).

## Ownership and compatibility

`MComponent.OperationDefinition`, its `SComponent.OperationDefinition` mirror,
and `SMethod` carry `serviceName: Option[String] = None` as their final field.
The default preserves existing source construction and field meanings. Ownership
is modeled separately from the canonical bare operation name, public selector,
input DTO identity and output DTO identity.

`ComponentScalaModelTransformer` copies metadata ownership and stamps each method
with the actual owning `MService` name. During generation `ComponentPart` also
stamps the actual `SService` owner, including directly authored models whose
methods have no explicit owner. Owner context is carried as immutable values.

Cozy owns extraction of the actual service name from its parsed service model.
SimpleModeler owns preserving that identity and using it to select each
generated service's schemas, actions, factories and public API contracts.

## Metadata selection

Operation lookup first applies the consumer's existing normalized operation-name
matching. Among those candidates it selects a definition with the exact
normalized service owner. If no such candidate exists, only one ownerless legacy
definition may be selected. A differently owned definition is never a fallback.
An ownerless method may select only a unique ownerless definition.

Multiple selected definitions, whether qualified or ownerless, fail generation
through the native model syntax-error mechanism. The diagnostic identifies the
ambiguous operation and requested service owner. An exact owner takes precedence
over ownerless legacy metadata. No match preserves existing method-derived
generation.

The selected definition supplies request fields and requiredness, input-value
kind and action name, response type, access metadata, echo-record behavior and
sync-by-design behavior. These consumers must not independently scan all
component operations for a matching bare name. Existing actual authorization
semantics are preserved.

## Generated contracts

Two services may both declare `readOriginal`. BookMaterial's
`BookMaterialReadOriginalQuery(resourceId, originalId)` and KnowledgeMaterial's
`OriginalSelectionQuery(materialRef, resourceId)` retain their distinct ordered
required fields, action/factory identities and response types. Neither service's
block may acquire the other's input schema or result type. Service order and
metadata order do not establish ownership.

`ComponentApiSourceGenerator` supplies the actual provided service name to its
operations before emitting signatures and proxies. `ComponentApiContractMetadata`
selects at most one definition per actual service operation by the same owner
and legacy rules. Only the selected request/response value graph enters that
API's public type closure. Existing artifact ABI canonicalization and hash
calculation remain unchanged; selected public types still determine the ABI.

## Executable coverage and downstream boundary

The executable specification exercises actual model transformation, normal
Scala 3 component generation, directly authored SMethod compatibility, unique
ownerless fallback, absent/foreign metadata, ambiguity, command/query controls,
and normal API source plus public type closure generation. A deterministic
property varies independent safe owner/schema names and reversed service and
metadata orders, requiring at least thirty successful samples with no discards.

This contract does not add `serviceName` to CNCF `CmlOperationDefinition`, whose
runtime carrier has no such field. Generated runtime metadata and its bare-name
consumers require separate parent assessment before integrated acceptance.
Producer tests, refreshed Cozy generation, native Studio generation/compilation,
independent review and whole Phase acceptance are distinct gates. Source edits
or generated text establish none of those results.
