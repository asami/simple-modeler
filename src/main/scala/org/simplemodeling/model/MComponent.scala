package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SComponent and SMComponent.
 * 
 * @since   Jan.  5, 2009
    version Aug.  7, 2009
 *  version Jul. 24, 2020
 * @version Feb.  9, 2026
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

  case class Core(
    entities: Vector[MEntity],
    stateMachineTransitionRules: Vector[StateMachineTransitionRule] = Vector.empty
  )
  object Core {
    trait Holder {
      def componentCore: Core

      def entities = componentCore.entities
      def stateMachineTransitionRules = componentCore.stateMachineTransitionRules
    }
  }
}
