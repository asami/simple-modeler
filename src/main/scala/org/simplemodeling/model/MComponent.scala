package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SComponent and SMComponent.
 * 
 * @since   Jan.  5, 2009
    version Aug.  7, 2009
 *  version Jul. 24, 2020
 *  version Feb.  9, 2026
 * @version Mar. 25, 2026
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
    entityName: String
  )

  final case class ViewDefinition(
    name: String,
    entityName: String,
    viewNames: Vector[String] = Vector.empty
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
    extensionBindings: Map[String, String] = Map.empty
  )

  final case class SubsystemDefinition(
    name: String,
    components: Vector[ComponentCoordinate] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    config: Map[String, String] = Map.empty
  )

  final case class OperationDefinition(
    name: String,
    kind: String,
    inputType: String,
    outputType: String,
    inputValueKind: String,
    parameters: Vector[OperationField] = Vector.empty
  )

  final case class OperationField(
    name: String,
    datatype: String,
    multiplicity: String = "1"
  )

  case class Core(
    entities: Vector[MEntity],
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
