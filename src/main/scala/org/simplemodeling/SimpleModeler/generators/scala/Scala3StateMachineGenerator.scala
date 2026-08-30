package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.SourceArtifacts
import org.simplemodeling.SimpleModeler.generator.scala.model.SStateMachine

/*
 * @since   Aug. 30, 2026
 * @version Aug. 30, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3StateMachineGenerator() {
  def generate(p: SStateMachine): Consequence[SourceArtifacts] = Consequence {
    SourceArtifacts.create(_path(p), _source(p))
  }

  private def _path(p: SStateMachine): String =
    s"${p.packageName.toPathName}/${p.className.name}.scala"

  private def _source(p: SStateMachine): String = {
    val states = _states(p.states)
    val transitions = _transitions(p.transitions)
    s"""package ${p.packageName.name}
       |
       |object ${p.className.name} {
       |  final case class State(
       |    name: String,
       |    value: Either[String, Int]
       |  )
       |
       |  final case class Transition(
       |    from: String,
       |    event: Option[String],
       |    to: String
       |  )
       |
       |  val states: Vector[State] = $states
       |
       |  val transitions: Vector[Transition] = $transitions
       |
       |  def permits(from: String, event: String, to: String): Boolean =
       |    transitions.exists { transition =>
       |      transition.from == from &&
       |        transition.event.contains(event) &&
       |        transition.to == to
       |    }
       |}
       |""".stripMargin
  }

  private def _states(p: Vector[SStateMachine.State]): String =
    _vector(p.map(_state))

  private def _state(p: SStateMachine.State): String =
    s"State(${_string_literal(p.name)}, ${_either(p.value)})"

  private def _transitions(p: Vector[SStateMachine.Transition]): String =
    _vector(p.map(_transition))

  private def _transition(p: SStateMachine.Transition): String =
    s"Transition(${_string_literal(p.from)}, ${_option_string_literal(p.event)}, ${_string_literal(p.to)})"

  private def _either(p: Either[String, Int]): String = p match {
    case Left(value) => s"Left(${_string_literal(value)})"
    case Right(value) => s"Right($value)"
  }

  private def _option_string_literal(p: Option[String]): String =
    p.map(x => s"Some(${_string_literal(x)})").getOrElse("None")

  private def _string_literal(p: String): String =
    "\"" + _escape_string(Option(p).getOrElse("")) + "\""

  private def _escape_string(p: String): String =
    p.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    }

  private def _vector(p: Vector[String]): String =
    if (p.isEmpty)
      "Vector.empty"
    else
      p.mkString("Vector(\n    ", ",\n    ", "\n  )")
}
