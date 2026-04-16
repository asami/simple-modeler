package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SComponent and SMComponent.
 * 
 * @since   Jan.  5, 2009
 *  version Aug.  7, 2009
 *  version Jul. 24, 2020
 *  version Feb.  9, 2026
 * @version Apr. 17, 2026
 * @author  ASAMI, Tomoharu
 */
trait MComponent extends MObject {
  def entities: Vector[MEntity]
}

object MComponent {
  sealed trait TransitionTrigger
  object TransitionTrigger {
    case object Save extends TransitionTrigger
    case object Update extends TransitionTrigger
  }

  sealed trait RuleGuard
  object RuleGuard {
    final case class Ref(name: String) extends RuleGuard
    final case class Expression(expr: String) extends RuleGuard
  }

  final case class RuleAction(
    script: String
  )

  final case class RulePlan(
    exit: Vector[RuleAction] = Vector.empty,
    transition: Option[RuleAction] = None,
    entry: Vector[RuleAction] = Vector.empty
  )

  final case class StateMachineTransitionRule(
    collectionName: String,
    trigger: TransitionTrigger,
    eventName: String,
    priority: Int = 0,
    declarationOrder: Int = 0,
    guard: Option[RuleGuard] = None,
    plan: RulePlan = RulePlan()
  )

  final case class StateMachineDefinition(
    name: String,
    states: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty
  )

  final case class EventReceptionDefinition(
    name: String,
    category: String = "NonActionEvent",
    kind: Option[String] = None,
    selectors: Map[String, String] = Map.empty,
    actionName: Option[String] = None,
    priority: Int = 0
  )

  final case class EventRoutingDefinition(
    name: String,
    when: Option[String] = None,
    topic: Option[String] = None,
    service: Option[String] = None,
    partition: Option[String] = None
  )

  final case class EventSubscriptionDefinition(
    name: String,
    eventName: String,
    route: String = "Unicast",
    entityName: Option[String] = None,
    target: Option[String] = None,
    targets: Vector[String] = Vector.empty,
    selector: Option[String] = None,
    actionName: String,
    declaredTargetUpperBound: Int = 1,
    activation: Option[String] = None
  )

  final case class AggregateDefinition(
    name: String,
    entityName: String,
    members: Vector[AggregateMemberDefinition] = Vector.empty,
    creates: Vector[AggregateCreateDefinition] = Vector.empty,
    commands: Vector[AggregateCommandDefinition] = Vector.empty,
    state: Vector[AggregateStateDefinition] = Vector.empty,
    invariants: Vector[AggregateInvariantDefinition] = Vector.empty
  )

  final case class AggregateMemberDefinition(
    name: String,
    entityName: String,
    kind: Option[String] = None,
    boundary: Option[String] = None,
    join: Option[String] = None,
    joinFieldName: Option[String] = None,
    multiplicity: Option[String] = None
  )

  final case class AggregateCommandDefinition(
    name: String,
    input: Map[String, String] = Map.empty,
    validations: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty,
    newState: Option[String] = None,
    implementation: Option[String] = None
  )

  final case class AggregateCreateDefinition(
    name: String,
    input: Map[String, String] = Map.empty,
    validations: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty,
    initialState: Option[String] = None,
    implementation: Option[String] = None
  )

  final case class AggregateStateDefinition(
    name: String,
    datatype: Option[String] = None,
    multiplicity: Option[String] = None
  )

  final case class AggregateInvariantDefinition(
    name: String,
    expression: Option[String] = None
  )

  final case class ViewQueryDefinition(
    name: String,
    expression: Option[String] = None
  )

  final case class ViewDefinition(
    name: String,
    entityName: String,
    viewNames: Vector[String] = Vector.empty,
    queries: Vector[ViewQueryDefinition] = Vector.empty,
    sourceEvents: Vector[String] = Vector.empty,
    rebuildable: Option[Boolean] = None
  )

  final case class ComponentCoordinate(
    group: String,
    artifact: String,
    version: String
  ) {
    def asString: String = s"${group}:${artifact}:${version}"
  }

  final case class ComponentDefinition(
    name: String,
    coordinates: Vector[ComponentCoordinate] = Vector.empty,
    componentlets: Vector[String] = Vector.empty,
    extensionPoints: Vector[String] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    domainVisions: Vector[VisionDefinition] = Vector.empty,
    domainCapabilities: Vector[CapabilityDefinition] = Vector.empty,
    domainQualities: Vector[QualityDefinition] = Vector.empty,
    domainConstraints: Vector[ConstraintDefinition] = Vector.empty,
    domainUseCases: Vector[UseCaseDefinition] = Vector.empty,
    useCases: Vector[UseCaseDefinition] = Vector.empty
  )

  final case class VisionDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  )

  final case class ContextDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  final case class SystemContextDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  final case class ContextMapDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  final case class CapabilityDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    actor: Option[String] = None,
    primaryActor: Option[String] = None,
    secondaryActor: Option[String] = None,
    supportingActor: Option[String] = None,
    stakeholder: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  )

  final case class QualityDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  )

  final case class ConstraintDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  )

  final case class UseCaseDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    actor: Option[String] = None,
    primaryActor: Option[String] = None,
    secondaryActor: Option[String] = None,
    supportingActor: Option[String] = None,
    stakeholder: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None,
    scenarios: Vector[UseCaseScenario] = Vector.empty
  )

  final case class UseCaseScenario(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    steps: Vector[String] = Vector.empty,
    alternates: Vector[String] = Vector.empty,
    exceptions: Vector[String] = Vector.empty
  )

  final case class SubsystemDefinition(
    name: String,
    components: Vector[ComponentCoordinate] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    config: Map[String, String] = Map.empty,
    domainVisions: Vector[VisionDefinition] = Vector.empty,
    domainContexts: Vector[ContextDefinition] = Vector.empty,
    domainSystemContexts: Vector[SystemContextDefinition] = Vector.empty,
    domainContextMaps: Vector[ContextMapDefinition] = Vector.empty,
    domainCapabilities: Vector[CapabilityDefinition] = Vector.empty,
    domainQualities: Vector[QualityDefinition] = Vector.empty,
    domainConstraints: Vector[ConstraintDefinition] = Vector.empty,
    domainUseCases: Vector[UseCaseDefinition] = Vector.empty
  )

  final case class OperationDefinition(
    name: String,
    kind: String,
    summary: Option[String] = None,
    execution: Option[String] = None,
    implementation: Option[String] = None,
    entityName: Option[String] = None,
    entityNames: Vector[String] = Vector.empty,
    inputType: String,
    inputSummary: Option[String] = None,
    inputDescription: Option[String] = None,
    outputType: String,
    outputSummary: Option[String] = None,
    outputDescription: Option[String] = None,
    inputValueKind: String,
    access: Option[OperationAccess] = None,
    parameters: Vector[OperationField] = Vector.empty
  )

  final case class OperationAccess(
    policy: String,
    resource: Option[String] = None,
    target: Option[String] = None,
    mode: Option[String] = None,
    relation: Option[String] = None,
    operationModel: Option[String] = None,
    entityUsage: Option[String] = None,
    entityOperationKind: Option[String] = None,
    entityApplicationDomain: Option[String] = None,
    condition: Option[String] = None
  )

  final case class OperationField(
    name: String,
    datatype: String,
    multiplicity: String = "1"
  )

  final case class EntityRuntimeDescriptor(
    entityName: String,
    packageName: String,
    usageKind: Option[String] = None,
    operationKind: Option[String] = None,
    applicationDomain: Option[String] = None,
    viewNames: Vector[String] = Vector.empty
  )

  case class Core(
    entities: Vector[MEntity],
    entityRuntimeDescriptors: Vector[EntityRuntimeDescriptor] = Vector.empty,
    stateMachineTransitionRules: Vector[StateMachineTransitionRule] = Vector.empty,
    stateMachineDefinitions: Vector[StateMachineDefinition] = Vector.empty,
    eventReceptionDefinitions: Vector[EventReceptionDefinition] = Vector.empty,
    eventRoutingDefinitions: Vector[EventRoutingDefinition] = Vector.empty,
    eventSubscriptionDefinitions: Vector[EventSubscriptionDefinition] = Vector.empty,
    aggregateDefinitions: Vector[AggregateDefinition] = Vector.empty,
    viewDefinitions: Vector[ViewDefinition] = Vector.empty,
    operationDefinitions: Vector[OperationDefinition] = Vector.empty,
    componentDefinitions: Vector[ComponentDefinition] = Vector.empty,
    subsystemDefinitions: Vector[SubsystemDefinition] = Vector.empty
  )
  object Core {
    trait Holder {
      def componentCore: Core

      def entities = componentCore.entities
      def entityRuntimeDescriptors = componentCore.entityRuntimeDescriptors
      def stateMachineTransitionRules = componentCore.stateMachineTransitionRules
      def stateMachineDefinitions = componentCore.stateMachineDefinitions
      def eventReceptionDefinitions = componentCore.eventReceptionDefinitions
      def eventRoutingDefinitions = componentCore.eventRoutingDefinitions
      def eventSubscriptionDefinitions = componentCore.eventSubscriptionDefinitions
      def aggregateDefinitions = componentCore.aggregateDefinitions
      def viewDefinitions = componentCore.viewDefinitions
      def operationDefinitions = componentCore.operationDefinitions
      def componentDefinitions = componentCore.componentDefinitions
      def subsystemDefinitions = componentCore.subsystemDefinitions
    }
  }
}
