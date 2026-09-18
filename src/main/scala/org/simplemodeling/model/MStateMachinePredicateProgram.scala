package org.simplemodeling.model

/*
 * @since   Sep. 18, 2026
 * @version Sep. 18, 2026
 * @author  ASAMI, Tomoharu
 */
/**
 * Closed identity and predicate vocabulary inherited by [[MComponent]].
 *
 * Keeping this grammar-sized responsibility separate preserves the stable
 * `MComponent.*` member paths while keeping the legacy model container
 * reviewable as its own responsibility.
 */
trait MStateMachinePredicateProgram {
  final case class StateMachineIdentity(
    qualifiedName: String
  ) {
    require(qualifiedName.trim.nonEmpty, "StateMachine identity requires a qualified name.")
  }

  final case class StateMachineStateIdentity(
    machine: StateMachineIdentity,
    path: Vector[String]
  ) {
    require(path.nonEmpty && path.forall(_.trim.nonEmpty), "StateMachine state identity requires a non-empty state path.")
  }

  final case class StateMachineTriggerIdentity(
    machine: StateMachineIdentity,
    eventName: String
  ) {
    require(eventName.trim.nonEmpty, "StateMachine trigger identity requires an event name.")
  }

  final case class StateMachineTriggerContextIdentity(
    trigger: StateMachineTriggerIdentity
  )

  final case class StateMachineTransitionIdentity(
    machine: StateMachineIdentity,
    declarationOrder: Int
  ) {
    require(declarationOrder >= 0, "StateMachine transition declaration order must be non-negative.")
  }

  final case class StateMachineGuardIdentity(
    transition: StateMachineTransitionIdentity,
    name: String
  ) {
    require(name.trim.nonEmpty, "StateMachine guard identity requires a binding name.")
  }

  sealed trait StateMachineActionPhase
  object StateMachineActionPhase {
    case object Exit extends StateMachineActionPhase
    case object Transition extends StateMachineActionPhase
    case object Entry extends StateMachineActionPhase
  }

  final case class StateMachineActionIdentity(
    transition: StateMachineTransitionIdentity,
    phase: StateMachineActionPhase,
    declarationOrder: Int
  ) {
    require(declarationOrder >= 0, "StateMachine action declaration order must be non-negative.")
  }

  final case class StateMachineSourceLocation(
    declarationPath: Vector[String]
  ) {
    require(declarationPath.nonEmpty, "StateMachine source location requires a declaration path.")
  }

  sealed trait StateMachinePredicateField
  object StateMachinePredicateField {
    case object EventName extends StateMachinePredicateField
    case object TargetIdentifier extends StateMachinePredicateField
    case object CurrentState extends StateMachinePredicateField
    case object CandidateState extends StateMachinePredicateField
  }

  sealed trait StateMachinePredicateValue
  object StateMachinePredicateValue {
    final case class Text(value: String) extends StateMachinePredicateValue
    final case class Bool(value: Boolean) extends StateMachinePredicateValue
  }

  /** Closed predicate language for a normalized StateMachine declaration. */
  sealed trait StateMachinePredicate
  object StateMachinePredicate {
    case object Always extends StateMachinePredicate
    final case class Literal(value: StateMachinePredicateValue.Bool) extends StateMachinePredicate
    final case class Equals(
      field: StateMachinePredicateField,
      value: StateMachinePredicateValue
    ) extends StateMachinePredicate
    final case class NotEquals(
      field: StateMachinePredicateField,
      value: StateMachinePredicateValue
    ) extends StateMachinePredicate
    final case class IsPresent(field: StateMachinePredicateField) extends StateMachinePredicate
    final case class All(terms: Vector[StateMachinePredicate]) extends StateMachinePredicate
    final case class Any(terms: Vector[StateMachinePredicate]) extends StateMachinePredicate
    final case class Not(term: StateMachinePredicate) extends StateMachinePredicate
  }

  final case class StateMachineTriggerContext(
    identity: StateMachineTriggerContextIdentity,
    eventName: String,
    targetIdentifier: Option[String] = None,
    currentState: Option[StateMachineStateIdentity] = None,
    candidateState: Option[StateMachineStateIdentity] = None
  ) {
    require(identity.trigger.eventName == eventName, "StateMachine trigger context event must match its trigger identity.")
    require(currentState.forall(_.machine == identity.trigger.machine), "StateMachine current state must belong to the context machine.")
    require(candidateState.forall(_.machine == identity.trigger.machine), "StateMachine candidate state must belong to the context machine.")
  }

  /** A closed, pure predicate program. Raw source text has no representation here. */
  final case class PredicateProgram(
    version: Int = PredicateProgram.VERSION_1,
    expression: StateMachinePredicate = StateMachinePredicate.Always
  )

  sealed trait PredicateProgramFailure {
    def code: String
    def message: String
  }
  object PredicateProgramFailure {
    final case class UnsupportedVersion(actual: Int) extends PredicateProgramFailure {
      val code = "unsupported-predicate-version"
      val message = s"Predicate program version $actual is not admitted."
    }
    final case class DepthLimitExceeded(actual: Int) extends PredicateProgramFailure {
      val code = "predicate-depth-limit"
      val message = s"Predicate program depth $actual exceeds the maximum."
    }
    final case class NodeLimitExceeded(actual: Int) extends PredicateProgramFailure {
      val code = "predicate-node-limit"
      val message = s"Predicate program node count $actual exceeds the maximum."
    }
    final case class TextLimitExceeded(actual: Int) extends PredicateProgramFailure {
      val code = "predicate-text-limit"
      val message = s"Predicate program text length $actual exceeds the maximum."
    }
    case object InvalidBindingName extends PredicateProgramFailure {
      val code = "invalid-guard-binding"
      val message = "Guard binding name is not admitted by the normalized contract."
    }
    final case class MissingContextField(field: StateMachinePredicateField) extends PredicateProgramFailure {
      val code = "predicate-missing-context-field"
      val message = s"Predicate program requires unavailable trigger-context field $field."
    }
    final case class TypeMismatch(
      field: StateMachinePredicateField,
      expected: StateMachinePredicateValue,
      actual: StateMachinePredicateValue
    ) extends PredicateProgramFailure {
      val code = "predicate-type-mismatch"
      val message = s"Predicate program value type does not match trigger-context field $field."
    }
  }

  object PredicateProgram {
    val VERSION_1 = 1
    val MAXIMUM_DEPTH = 8
    val MAXIMUM_NODES = 64
    val MAXIMUM_TEXT_BYTES = 256

    def validate(program: PredicateProgram): Either[PredicateProgramFailure, Unit] = {
      import PredicateProgramFailure._
      if (program.version != VERSION_1)
        Left(UnsupportedVersion(program.version))
      else if (_depth(program.expression) > MAXIMUM_DEPTH)
        Left(DepthLimitExceeded(_depth(program.expression)))
      else if (_nodes(program.expression) > MAXIMUM_NODES)
        Left(NodeLimitExceeded(_nodes(program.expression)))
      else {
        _texts(program.expression).find(value => _utf8_size(value) > MAXIMUM_TEXT_BYTES) match {
          case Some(value) => Left(TextLimitExceeded(_utf8_size(value)))
          case None => Right(())
        }
      }
    }

    def evaluate(
      program: PredicateProgram,
      context: StateMachineTriggerContext
    ): Either[PredicateProgramFailure, Boolean] =
      validate(program) match {
        case Left(error) => Left(error)
        case Right(_) => _evaluate(program.expression, context)
      }

    private def _evaluate(
      expression: StateMachinePredicate,
      context: StateMachineTriggerContext
    ): Either[PredicateProgramFailure, Boolean] = {
      import PredicateProgramFailure._
      import StateMachinePredicate._
      expression match {
        case Always => Right(true)
        case Literal(StateMachinePredicateValue.Bool(value)) => Right(value)
        case Equals(field, expected) =>
          _value(field, context) match {
            case None => Left(MissingContextField(field))
            case Some(actual) if _same_type(expected, actual) => Right(actual == expected)
            case Some(actual) => Left(TypeMismatch(field, expected, actual))
          }
        case NotEquals(field, expected) =>
          _evaluate(Equals(field, expected), context) match {
            case Right(value) => Right(!value)
            case Left(error) => Left(error)
          }
        case IsPresent(field) => Right(_value(field, context).nonEmpty)
        case All(terms) => _all(terms, context)
        case Any(terms) => _any(terms, context)
        case Not(term) =>
          _evaluate(term, context) match {
            case Right(value) => Right(!value)
            case Left(error) => Left(error)
          }
      }
    }

    private def _all(
      terms: Vector[StateMachinePredicate],
      context: StateMachineTriggerContext
    ): Either[PredicateProgramFailure, Boolean] = {
      var index = 0
      var alltrue = true
      while (index < terms.size) {
        _evaluate(terms(index), context) match {
          case Right(value) => alltrue = alltrue && value
          case Left(error) => return Left(error)
        }
        index += 1
      }
      Right(alltrue)
    }

    private def _any(
      terms: Vector[StateMachinePredicate],
      context: StateMachineTriggerContext
    ): Either[PredicateProgramFailure, Boolean] = {
      var index = 0
      var anytrue = false
      while (index < terms.size) {
        _evaluate(terms(index), context) match {
          case Right(value) => anytrue = anytrue || value
          case Left(error) => return Left(error)
        }
        index += 1
      }
      Right(anytrue)
    }

    private def _value(
      field: StateMachinePredicateField,
      context: StateMachineTriggerContext
    ): Option[StateMachinePredicateValue] = {
      import StateMachinePredicateField._
      import StateMachinePredicateValue._
      field match {
        case EventName => Some(Text(context.eventName))
        case TargetIdentifier => context.targetIdentifier.map(Text.apply)
        case CurrentState => context.currentState.map(x => Text(x.path.mkString(".")))
        case CandidateState => context.candidateState.map(x => Text(x.path.mkString(".")))
      }
    }

    private def _same_type(
      expected: StateMachinePredicateValue,
      actual: StateMachinePredicateValue
    ): Boolean =
      (expected, actual) match {
        case (_: StateMachinePredicateValue.Text, _: StateMachinePredicateValue.Text) => true
        case (_: StateMachinePredicateValue.Bool, _: StateMachinePredicateValue.Bool) => true
        case _ => false
      }

    private def _depth(expression: StateMachinePredicate): Int = {
      import StateMachinePredicate._
      expression match {
        case Literal(_) => 1
        case All(terms) => 1 + _child_depth(terms)
        case Any(terms) => 1 + _child_depth(terms)
        case Not(term) => 1 + _depth(term)
        case _ => 1
      }
    }

    private def _child_depth(terms: Vector[StateMachinePredicate]): Int =
      if (terms.isEmpty) 0 else terms.map(_depth).max

    private def _nodes(expression: StateMachinePredicate): Int = {
      import StateMachinePredicate._
      expression match {
        case Literal(_) => 1
        case All(terms) => 1 + terms.map(_nodes).sum
        case Any(terms) => 1 + terms.map(_nodes).sum
        case Not(term) => 1 + _nodes(term)
        case _ => 1
      }
    }

    private def _texts(expression: StateMachinePredicate): Vector[String] = {
      import StateMachinePredicate._
      import StateMachinePredicateValue.Text
      expression match {
        case Literal(_) => Vector.empty
        case Equals(_, Text(value)) => Vector(value)
        case NotEquals(_, Text(value)) => Vector(value)
        case All(terms) => terms.flatMap(_texts)
        case Any(terms) => terms.flatMap(_texts)
        case Not(term) => _texts(term)
        case _ => Vector.empty
      }
    }

    private def _utf8_size(value: String): Int =
      value.getBytes("UTF-8").length
  }
}
