package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SComponent and SMComponent.
 * 
 * @since   Jan.  5, 2009
 *  version Aug.  7, 2009
 *  version Jul. 24, 2020
 *  version Feb.  9, 2026
 *  version May.  8, 2026
 *  version Jul. 25, 2026
 *  version Aug. 14, 2026
 * @version Sep. 18, 2026
 * @author  ASAMI, Tomoharu
 */
trait MComponent extends MObject {
  def entities: Vector[MEntity]
}

object MComponent extends MStateMachinePredicateProgram {
  sealed trait StateMachineGuardProgram
  object StateMachineGuardProgram {
    final case class Predicate(program: PredicateProgram) extends StateMachineGuardProgram
    final case class Named(identity: StateMachineGuardIdentity) extends StateMachineGuardProgram

    def validate(program: StateMachineGuardProgram): Either[PredicateProgramFailure, Unit] =
      program match {
        case Predicate(value) => PredicateProgram.validate(value)
        case Named(identity) =>
          if (!identity.name.matches("^[A-Za-z_][A-Za-z0-9_\\.]*$"))
            Left(PredicateProgramFailure.InvalidBindingName)
          else if (identity.name.getBytes("UTF-8").length <= PredicateProgram.MAXIMUM_TEXT_BYTES)
            Right(())
          else
            Left(PredicateProgramFailure.TextLimitExceeded(identity.name.getBytes("UTF-8").length))
      }
  }

  /**
   * Pure runtime-facing contract for an admitted named guard.  Normalization
   * records the binding identity only and never invokes this resolver.
   */
  trait StateMachineGuardBindingResolver {
    def evaluate(
      binding: StateMachineGuardIdentity,
      context: StateMachineTriggerContext
    ): StateMachineGuardBindingResult
  }

  sealed trait StateMachineGuardBindingResult
  object StateMachineGuardBindingResult {
    case object Matched extends StateMachineGuardBindingResult
    case object NotMatched extends StateMachineGuardBindingResult
    final case class Failed(code: String) extends StateMachineGuardBindingResult
  }

  sealed trait StateMachineTransitionTarget
  object StateMachineTransitionTarget {
    final case class State(
      identity: StateMachineStateIdentity
    ) extends StateMachineTransitionTarget
    final case class ShallowHistory(
      composite: StateMachineStateIdentity,
      fallbackLeaf: Option[StateMachineStateIdentity]
    ) extends StateMachineTransitionTarget
    case object Final extends StateMachineTransitionTarget
  }

  final case class StateMachineTransitionPriority(value: Int) {
    require(value >= 0, "StateMachine transition priority must be non-negative.")
  }
  object StateMachineTransitionPriority {
    val default = StateMachineTransitionPriority(0)
  }

  final case class StateMachineCompositeTopology(
    identity: StateMachineStateIdentity,
    directLeaves: Vector[StateMachineStateIdentity]
  )

  final case class StateMachineTopology(
    composites: Vector[StateMachineCompositeTopology] = Vector.empty,
    terminalTransitions: Vector[StateMachineTransitionIdentity] = Vector.empty
  )

  final case class StateMachineHistoryWrite(
    composite: StateMachineStateIdentity,
    leaf: StateMachineStateIdentity
  )

  final case class NormalizedStateMachineAction(
    identity: StateMachineActionIdentity,
    reference: String
  ) {
    require(reference.matches("^[A-Za-z_][A-Za-z0-9_\\.]*$"), "Normalized StateMachine action requires a named local binding reference.")
    require(reference.getBytes("UTF-8").length <= PredicateProgram.MAXIMUM_TEXT_BYTES, "Normalized StateMachine action reference exceeds the UTF-8 byte limit.")
  }

  final case class NormalizedStateMachineActionPlan(
    exit: Vector[NormalizedStateMachineAction] = Vector.empty,
    transition: Vector[NormalizedStateMachineAction] = Vector.empty,
    entry: Vector[NormalizedStateMachineAction] = Vector.empty
  )

  final case class NormalizedStateMachineTransition(
    identity: StateMachineTransitionIdentity,
    source: Option[StateMachineStateIdentity],
    target: StateMachineTransitionTarget,
    trigger: StateMachineTriggerIdentity,
    sourceLocation: StateMachineSourceLocation,
    priority: StateMachineTransitionPriority = StateMachineTransitionPriority.default,
    guard: StateMachineGuardProgram = StateMachineGuardProgram.Predicate(PredicateProgram()),
    actions: NormalizedStateMachineActionPlan = NormalizedStateMachineActionPlan(),
    historyWrites: Vector[StateMachineHistoryWrite] = Vector.empty
  ) {
    require(source.forall(_.machine == identity.machine), "StateMachine transition source must belong to its machine.")
    require(trigger.machine == identity.machine, "StateMachine transition trigger must belong to its machine.")
    require(_target_machines(target).forall(_ == identity.machine), "StateMachine transition target must belong to its machine.")
    require(_guard_is_admitted(guard), "StateMachine transition guard must be an admitted program or a binding for this transition.")
    require(_action_identities(actions).forall(_.transition == identity), "StateMachine action must belong to its transition.")
    require(_actions_have_phase(actions.exit, StateMachineActionPhase.Exit), "StateMachine exit action must have exit phase.")
    require(_actions_have_phase(actions.transition, StateMachineActionPhase.Transition), "StateMachine transition action must have transition phase.")
    require(_actions_have_phase(actions.entry, StateMachineActionPhase.Entry), "StateMachine entry action must have entry phase.")
    require(_actions_have_unique_declaration_order(actions.exit), "StateMachine exit action declaration order must be unique.")
    require(_actions_have_unique_declaration_order(actions.transition), "StateMachine transition action declaration order must be unique.")
    require(_actions_have_unique_declaration_order(actions.entry), "StateMachine entry action declaration order must be unique.")
    require(historyWrites.forall(_.composite.machine == identity.machine), "StateMachine history write composite must belong to its machine.")
    require(historyWrites.forall(_.leaf.machine == identity.machine), "StateMachine history write leaf must belong to its machine.")

    private def _target_machines(target: StateMachineTransitionTarget): Vector[StateMachineIdentity] =
      target match {
        case StateMachineTransitionTarget.State(value) => Vector(value.machine)
        case StateMachineTransitionTarget.ShallowHistory(composite, fallbackleaf) =>
          Vector(composite.machine) ++ fallbackleaf.map(_.machine)
        case StateMachineTransitionTarget.Final => Vector.empty
      }

    private def _action_identities(plan: NormalizedStateMachineActionPlan): Vector[StateMachineActionIdentity] =
      plan.exit.map(_.identity) ++ plan.transition.map(_.identity) ++ plan.entry.map(_.identity)

    private def _actions_have_phase(
      values: Vector[NormalizedStateMachineAction],
      phase: StateMachineActionPhase
    ): Boolean =
      values.forall(_.identity.phase == phase)

    private def _actions_have_unique_declaration_order(
      values: Vector[NormalizedStateMachineAction]
    ): Boolean =
      values.map(_.identity.declarationOrder).distinct.size == values.size

    private def _guard_is_admitted(value: StateMachineGuardProgram): Boolean =
      value match {
        case StateMachineGuardProgram.Predicate(program) =>
          PredicateProgram.validate(program).isRight
        case StateMachineGuardProgram.Named(binding) =>
          binding.transition == identity && StateMachineGuardProgram.validate(value).isRight
      }
  }

  final case class NormalizedStateMachine(
    identity: StateMachineIdentity,
    version: Int = 1,
    initialState: Option[StateMachineStateIdentity] = None,
    states: Vector[StateMachineStateIdentity] = Vector.empty,
    transitions: Vector[NormalizedStateMachineTransition] = Vector.empty,
    historyFieldName: Option[String] = None,
    topology: StateMachineTopology = StateMachineTopology()
  ) {
    require(version > 0, "Normalized StateMachine version must be positive.")
    require(initialState.nonEmpty, "Normalized StateMachine requires an explicit initial state.")
    require(initialState.forall(_.machine == identity), "Normalized initial state must belong to its machine.")
    require(states.forall(_.machine == identity), "Normalized state must belong to its machine.")
    require(states.distinct.size == states.size, "Normalized StateMachine state identity is duplicated.")
    require(initialState.forall(states.contains), "Normalized initial state must be declared by its machine.")
    require(transitions.forall(_.identity.machine == identity), "Normalized transition must belong to its machine.")
    require(transitions.map(_.identity).distinct.size == transitions.size, "Normalized StateMachine transition identity is duplicated.")
    require(transitions.map(_.identity.declarationOrder).distinct.size == transitions.size, "Normalized StateMachine declaration order is duplicated.")
    require(transitions.forall(_.source.forall(states.contains)), "Normalized transition source state must be declared by its machine.")
    require(transitions.forall(_target_state(_).forall(states.contains)), "Normalized transition target state must be declared by its machine.")
    require(topology.composites.forall(_.identity.machine == identity), "Normalized composite topology must belong to its machine.")
    require(topology.composites.map(_.identity).distinct.size == topology.composites.size, "Normalized composite topology identity is duplicated.")
    require(topology.composites.forall(composite => states.contains(composite.identity)), "Normalized composite topology state must be declared by its machine.")
    require(topology.composites.forall(_.directLeaves.forall(states.contains)), "Normalized composite topology leaves must be declared by its machine.")
    require(topology.composites.forall(composite => composite.directLeaves.forall(_is_direct_leaf(composite.identity, _))), "Normalized composite topology leaf must be an immediate path child of its composite.")
    require(topology.composites.forall(composite => composite.directLeaves.forall(leaf => !topology.composites.map(_.identity).contains(leaf))), "Normalized composite topology leaf must not itself be a declared composite.")
    require(topology.composites.forall(composite => composite.directLeaves.distinct.size == composite.directLeaves.size), "Normalized composite topology leaf identity is duplicated.")
    require(transitions.forall(_history_target_is_admitted), "Normalized shallow-history target must name an admitted composite and direct fallback leaf.")
    require(transitions.forall(_history_writes_are_admitted), "Normalized history write must name an admitted composite and direct leaf.")
    require(topology.terminalTransitions.forall(_.machine == identity), "Normalized terminal transition must belong to its machine.")
    require(topology.terminalTransitions.distinct.size == topology.terminalTransitions.size, "Normalized terminal transition identity is duplicated.")
    require(topology.terminalTransitions.forall(transitions.map(_.identity).contains), "Normalized terminal transition must be declared by its machine.")
    require(topology.terminalTransitions.forall(_is_final_transition), "Normalized terminal transition must target final state.")

    private def _target_state(
      transition: NormalizedStateMachineTransition
    ): Option[StateMachineStateIdentity] =
      transition.target match {
        case StateMachineTransitionTarget.State(value) => Some(value)
        case _ => None
      }

    private def _history_target_is_admitted(
      transition: NormalizedStateMachineTransition
    ): Boolean =
      transition.target match {
        case StateMachineTransitionTarget.ShallowHistory(composite, fallbackleaf) =>
          _topology_for(composite).exists { topology =>
            fallbackleaf.forall(topology.directLeaves.contains)
          }
        case _ => true
      }

    private def _history_writes_are_admitted(
      transition: NormalizedStateMachineTransition
    ): Boolean =
      transition.historyWrites.forall { write =>
        _topology_for(write.composite).exists(_.directLeaves.contains(write.leaf))
      }

    private def _topology_for(
      composite: StateMachineStateIdentity
    ): Option[StateMachineCompositeTopology] =
      topology.composites.find(_.identity == composite)

    private def _is_direct_leaf(
      composite: StateMachineStateIdentity,
      leaf: StateMachineStateIdentity
    ): Boolean =
      leaf.path.size == composite.path.size + 1 && leaf.path.startsWith(composite.path)

    private def _is_final_transition(
      transitionidentity: StateMachineTransitionIdentity
    ): Boolean =
      transitions.find(_.identity == transitionidentity).exists { transition =>
        transition.target == StateMachineTransitionTarget.Final
      }
  }

  final case class StateMachineNormalizationDiagnostic(
    code: String,
    message: String,
    machine: StateMachineIdentity,
    sourceLocation: StateMachineSourceLocation,
    transition: Option[StateMachineTransitionIdentity] = None
  )

  sealed trait StateMachineNormalization
  object StateMachineNormalization {
    final case class Accepted(
      value: NormalizedStateMachine
    ) extends StateMachineNormalization
    final case class Rejected(
      diagnostics: Vector[StateMachineNormalizationDiagnostic]
    ) extends StateMachineNormalization {
      require(diagnostics.nonEmpty, "StateMachine normalization rejection requires at least one diagnostic.")
    }
  }

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

  final case class StateMachineHistoryRecordWrite(
    compositeName: String,
    leafName: String
  )

  final case class StateMachineHistoryComposite(
    name: String,
    directLeaves: Vector[String] = Vector.empty,
    fallbackLeaf: Option[String] = None
  )

  /*
   * Generated-code input for one exact normalized CML transition.  The
   * component generator supplies the component and EntityCollection identity;
   * this model deliberately retains the transition identity rather than
   * asking a runtime to recover it from an operation, a state field, or a
   * proposed record.
   */
  final case class StateMachineTransitionBinding(
    entityName: String,
    machine: StateMachineIdentity,
    version: Int,
    transition: StateMachineTransitionIdentity,
    source: StateMachineStateIdentity,
    target: StateMachineTransitionTarget,
    trigger: StateMachineTriggerIdentity
  ) {
    require(entityName.trim.nonEmpty, "StateMachine transition binding entity name must be nonempty.")
    require(version > 0, "StateMachine transition binding version must be positive.")
    require(transition.machine == machine, "StateMachine transition binding transition must belong to its machine.")
    require(source.machine == machine, "StateMachine transition binding source must belong to its machine.")
    require(trigger.machine == machine, "StateMachine transition binding trigger must belong to its machine.")
  }

  final case class StateMachineTransitionRule(
    collectionName: String,
    trigger: TransitionTrigger,
    eventName: String,
    machineName: Option[String] = None,
    stateFieldName: Option[String] = None,
    fromState: Option[String] = None,
    fromStateValue: Option[Int] = None,
    toState: Option[String] = None,
    toStateValue: Option[Int] = None,
    priority: Int = 0,
    declarationOrder: Int = 0,
    guard: Option[RuleGuard] = None,
    plan: RulePlan = RulePlan(),
    historyCompositeName: Option[String] = None,
    historyFieldName: Option[String] = None,
    historyDirectLeaves: Vector[String] = Vector.empty,
    historyDirectLeafValues: Map[String, Int] = Map.empty,
    historyFallbackLeaf: Option[String] = None,
    expectedHistoryRecordWrites: Vector[StateMachineHistoryRecordWrite] = Vector.empty,
    binding: Option[StateMachineTransitionBinding] = None
  )

  final case class StateMachineDefinition(
    name: String,
    states: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty,
    historyFieldName: Option[String] = None,
    historyComposites: Vector[StateMachineHistoryComposite] = Vector.empty,
    normalization: Option[StateMachineNormalization] = None
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
    viewFields: Map[String, Vector[String]] = Map.empty,
    queries: Vector[ViewQueryDefinition] = Vector.empty,
    sourceEvents: Vector[String] = Vector.empty,
    rebuildable: Option[Boolean] = None
  )

  final case class RelationshipDefinition(
    name: String,
    kind: String,
    sourceEntityName: String,
    targetEntityName: String,
    targetModelKind: String = "entity",
    sourceRole: Option[String] = None,
    targetRole: Option[String] = None,
    multiplicity: Option[String] = None,
    storageMode: String = "association-record",
    parentIdField: Option[String] = None,
    valueField: Option[String] = None,
    sortOrderField: Option[String] = None,
    associationDomain: Option[String] = None,
    targetKind: Option[String] = None,
    lifecyclePolicy: Option[String] = None
  )

  final case class OperationAssociationBinding(
    domain: String,
    targetKind: String,
    createsAssociation: Boolean = false,
    detachesAssociation: Boolean = false,
    roles: Vector[String] = Vector.empty,
    parameters: Vector[String] = Vector.empty,
    sourceEntityIdMode: String = "none",
    sourceEntityIdParameters: Vector[String] = Vector.empty,
    sourceEntityIdResultFields: Vector[String] = Vector("entity_id", "entityId", "id"),
    targetIdParameters: Vector[String] = Vector.empty,
    sortOrderParameters: Vector[String] = Vector.empty
  )

  final case class OperationChildEntityBinding(
    name: String,
    entityName: String,
    inputParameter: String,
    parentIdField: String,
    relationshipName: Option[String] = None,
    sourceEntityIdMode: String = "none",
    sourceEntityIdParameters: Vector[String] = Vector.empty,
    sourceEntityIdResultFields: Vector[String] = Vector("entity_id", "entityId", "id"),
    childIdField: Option[String] = Some("id"),
    sortOrderField: Option[String] = None,
    createsEntity: Boolean = false,
    failurePolicy: String = "compensate-parent-on-create"
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
    actors: Vector[ActorDefinition] = Vector.empty,
    coordinates: Vector[ComponentCoordinate] = Vector.empty,
    componentlets: Vector[String] = Vector.empty,
    extensionPoints: Vector[String] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    domainVisions: Vector[VisionDefinition] = Vector.empty,
    domainCapabilities: Vector[CapabilityDefinition] = Vector.empty,
    domainQualities: Vector[QualityDefinition] = Vector.empty,
    domainConstraints: Vector[ConstraintDefinition] = Vector.empty,
    domainUseCases: Vector[UseCaseDefinition] = Vector.empty,
    useCases: Vector[UseCaseDefinition] = Vector.empty,
    services: Vector[ComponentServiceDefinition] = Vector.empty
  )

  final case class ActorDefinition(
    name: String,
    kind: Option[String] = None,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  final case class ActorReference(
    name: String,
    role: String,
    targetKind: String
  )

  final case class ComponentServiceDefinition(
    name: String,
    spiStandard: Option[String] = None,
    spiDirection: String = "provides",
    spiSocket: Boolean = false,
    spiMultiplicity: Option[String] = None,
    spiRequired: Boolean = false,
    spiApiName: Option[String] = None,
    spiComponentApi: Option[String] = None
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
    id: Option[String] = None,
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
    trigger: Option[String] = None,
    priority: Option[String] = None,
    status: Option[String] = None,
    actorReferences: Vector[ActorReference] = Vector.empty,
    scenarios: Vector[UseCaseScenario] = Vector.empty
  )

  final case class UseCaseScenario(
    name: String,
    kind: String = "main",
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
    commandKind: Option[String] = None,
    commandExecutionProperties: Map[String, String] = Map.empty,
    commandExecutionPolicy: Option[String] = None,
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
    visibility: Option[String] = None,
    access: Option[OperationAccess] = None,
    parameters: Vector[OperationField] = Vector.empty,
    resultFields: Vector[OperationField] = Vector.empty,
    operationAuthorization: Option[OperationAuthorization] = None,
    childEntityBindings: Vector[OperationChildEntityBinding] = Vector.empty,
    associationBinding: Option[OperationAssociationBinding] = None,
    evaluation: Option[OperationEvaluation] = None
  )

  final case class OperationAuthorization(
    operationModes: Vector[String] = Vector.empty,
    allowAnonymous: Option[Boolean] = None,
    anonymousOperationModes: Vector[String] = Vector.empty
  )

  final case class OperationEvaluation(
    corpus: Option[CorpusOperationEvaluation] = None,
    experiment: Option[ExperimentOperationEvaluation] = None
  )

  final case class CorpusOperationEvaluation(
    capture: String,
    profile: String,
    admission: String = "optional",
    outcomes: Vector[String] = Vector.empty,
    sampling: Option[String] = None,
    redaction: Option[String] = None
  )

  final case class ExperimentOperationEvaluation(
    eligible: Boolean,
    purpose: String,
    admission: String = "optional",
    variantProfile: Option[String] = None
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

  final case class OperationUpdateField(
    sourceMultiplicity: String,
    nullAllowed: Boolean
  )

  final case class OperationField(
    name: String,
    datatype: String,
    multiplicity: String = "1",
    label: Option[String] = None,
    controlType: Option[String] = None,
    placeholder: Option[String] = None,
    help: Option[String] = None,
    required: Option[Boolean] = None,
    confidentiality: Option[String] = None,
    constraints: List[MConstraint] = Nil,
    typeConstraints: List[MConstraint] = Nil,
    update: Option[OperationUpdateField] = None
  )

  final case class EntityRuntimeDescriptor(
    entityName: String,
    packageName: String,
    entityKind: Option[String] = None,
    usageKind: Option[String] = None,
    operationKind: Option[String] = None,
    applicationDomain: Option[String] = None,
    viewNames: Vector[String] = Vector.empty,
    revisionModelKind: Option[String] = None,
    revisionRepresentation: Option[String] = None
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
    relationshipDefinitions: Vector[RelationshipDefinition] = Vector.empty,
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
      def relationshipDefinitions = componentCore.relationshipDefinitions
      def operationDefinitions = componentCore.operationDefinitions
      def componentDefinitions = componentCore.componentDefinitions
      def subsystemDefinitions = componentCore.subsystemDefinitions
    }
  }
}
