package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 24, 2026
 *  version Mar. 25, 2026
 *  version May. 22, 2026
 * @version Aug. 30, 2026
 * @author  ASAMI, Tomoharu
 */
class StateMachineScalaModelTransformer() extends ScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_accept_object(p: MObject): Boolean = p.isInstanceOf[MStateMachine]

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MStateMachine, purpose) if is_accept_purpose(purpose) =>
        transform_state_machine(m, purpose)
      case _ =>
        Consequence.noReachDefect("StateMachineScalaModelTransformer#apply")
    }

  protected def transform_state_machine(
    p: MStateMachine,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] = Consequence {
    val core = to_scala_core_with_subpackage(p).withPurpose(purpose)
    val states = p.states.toVector.map { state =>
      SStateMachine.State(state.name, state.value)
    }
    val transitions = p.transitions.toVector.map { transition =>
      SStateMachine.Transition(
        transition.preState.name,
        transition.event.map(_.name),
        transition.postState.name
      )
    }
    Vector(SStateMachine(core, SStateMachine.StateMachineCore(states, transitions)))
  }
}
