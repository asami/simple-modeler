package org.simplemodeling.model

import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.model.MComponent.{PredicateProgram, PredicateProgramFailure, StateMachineIdentity, StateMachinePredicate, StateMachinePredicateField, StateMachinePredicateValue, StateMachineTriggerContext, StateMachineTriggerContextIdentity, StateMachineTriggerIdentity}

/*
 * @since   Sep. 18, 2026
 * @version Sep. 18, 2026
 * @author  ASAMI, Tomoharu
 */
final class PredicateProgramSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _machine = StateMachineIdentity("example.lifecycle")
  private val _trigger = StateMachineTriggerIdentity(_machine, "publish")
  private val _context_identity = StateMachineTriggerContextIdentity(_trigger)

  private def _transition(
    identity: MComponent.StateMachineTransitionIdentity,
    source: Option[MComponent.StateMachineStateIdentity] = None,
    target: MComponent.StateMachineTransitionTarget = MComponent.StateMachineTransitionTarget.Final,
    actions: MComponent.NormalizedStateMachineActionPlan = MComponent.NormalizedStateMachineActionPlan(),
    historywrites: Vector[MComponent.StateMachineHistoryWrite] = Vector.empty
  ): MComponent.NormalizedStateMachineTransition =
    MComponent.NormalizedStateMachineTransition(
      identity = identity,
      source = source,
      target = target,
      trigger = _trigger,
      sourceLocation = MComponent.StateMachineSourceLocation(Vector("statemachine", "payment", "transition", identity.declarationOrder.toString)),
      actions = actions,
      historyWrites = historywrites
    )

  "PredicateProgram" should {
    "evaluate an admitted typed predicate against its explicit trigger context" in {
      Given("a version-one equality predicate and matching trigger context")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Equals(
          StateMachinePredicateField.EventName,
          StateMachinePredicateValue.Text("publish")
        )
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the pure predicate program is evaluated")
      val result = PredicateProgram.evaluate(program, context)

      Then("the declared scalar value matches without ambient lookup")
      result shouldBe Right(true)
    }

    "evaluate a closed boolean literal without an ambient lookup" in {
      Given("a version-one false literal")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Literal(StateMachinePredicateValue.Bool(false))
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the pure predicate program is evaluated")
      val result = PredicateProgram.evaluate(program, context)

      Then("the literal is evaluated directly")
      result shouldBe Right(false)
    }

    "evaluate a nonmatching event name as true and its negated equality as false" in {
      Given("a publish trigger context and two predicates for the archive event")
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")
      val notequalsprogram = PredicateProgram(
        expression = StateMachinePredicate.NotEquals(
          StateMachinePredicateField.EventName,
          StateMachinePredicateValue.Text("archive")
        )
      )
      val notprogram = PredicateProgram(
        expression = StateMachinePredicate.Not(
          StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("publish")
          )
        )
      )

      When("the predicates are evaluated against the explicit trigger context")
      val notequalsresult = PredicateProgram.evaluate(notequalsprogram, context)
      val notresult = PredicateProgram.evaluate(notprogram, context)

      Then("the nonmatching event is true and the negated matching equality is false")
      notequalsresult shouldBe Right(true)
      notresult shouldBe Right(false)
    }

    "report whether an optional target identifier is present" in {
      Given("publish contexts with an absent target and with target person-1")
      val absentcontext = StateMachineTriggerContext(_context_identity, eventName = "publish")
      val presentcontext = StateMachineTriggerContext(
        _context_identity,
        eventName = "publish",
        targetIdentifier = Some("person-1")
      )
      val program = PredicateProgram(
        expression = StateMachinePredicate.IsPresent(StateMachinePredicateField.TargetIdentifier)
      )

      When("the presence predicate is evaluated for each context")
      val absentresult = PredicateProgram.evaluate(program, absentcontext)
      val presentresult = PredicateProgram.evaluate(program, presentcontext)

      Then("absence and presence are represented as false and true")
      absentresult shouldBe Right(false)
      presentresult shouldBe Right(true)
    }

    "evaluate every admitted term in a successful All expression" in {
      Given("a publish context with target person-1 and an All expression of true terms")
      val context = StateMachineTriggerContext(
        _context_identity,
        eventName = "publish",
        targetIdentifier = Some("person-1")
      )
      val program = PredicateProgram(
        expression = StateMachinePredicate.All(Vector(
          StateMachinePredicate.Always,
          StateMachinePredicate.Literal(StateMachinePredicateValue.Bool(true)),
          StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("publish")
          ),
          StateMachinePredicate.NotEquals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("archive")
          ),
          StateMachinePredicate.IsPresent(StateMachinePredicateField.TargetIdentifier),
          StateMachinePredicate.Not(
            StateMachinePredicate.Equals(
              StateMachinePredicateField.EventName,
              StateMachinePredicateValue.Text("archive")
            )
          )
        ))
      )

      When("the All expression is evaluated")
      val result = PredicateProgram.evaluate(program, context)

      Then("all admitted terms produce a successful true result")
      result shouldBe Right(true)
    }

    "evaluate a successful Any expression when one term matches" in {
      Given("a publish context and an Any expression with one matching event term")
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Any(Vector(
          StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("archive")
          ),
          StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("publish")
          )
        ))
      )

      When("the Any expression is evaluated")
      val result = PredicateProgram.evaluate(program, context)

      Then("the matching term makes the aggregate result true")
      result shouldBe Right(true)
    }

    "project current and candidate state paths into dotted predicate values" in {
      Given("a publish context with same-machine Draft and Review.Approved states")
      val currentstate = MComponent.StateMachineStateIdentity(_machine, Vector("Draft"))
      val candidatestate = MComponent.StateMachineStateIdentity(_machine, Vector("Review", "Approved"))
      val context = StateMachineTriggerContext(
        _context_identity,
        eventName = "publish",
        currentState = Some(currentstate),
        candidateState = Some(candidatestate)
      )
      val currentprogram = PredicateProgram(
        expression = StateMachinePredicate.Equals(
          StateMachinePredicateField.CurrentState,
          StateMachinePredicateValue.Text("Draft")
        )
      )
      val candidateprogram = PredicateProgram(
        expression = StateMachinePredicate.Equals(
          StateMachinePredicateField.CandidateState,
          StateMachinePredicateValue.Text("Review.Approved")
        )
      )

      When("the state predicates are evaluated against the explicit trigger context")
      val currentresult = PredicateProgram.evaluate(currentprogram, context)
      val candidateresult = PredicateProgram.evaluate(candidateprogram, context)

      Then("the state identities match their dotted path values")
      currentresult shouldBe Right(true)
      candidateresult shouldBe Right(true)
    }

    "reject a predicate beyond the declared structural depth" in {
      Given("a nested predicate whose depth exceeds the version-one bound")
      val expression = (1 to PredicateProgram.MAXIMUM_DEPTH).foldLeft[StateMachinePredicate](
        StateMachinePredicate.Always
      ) { (term, _) =>
        StateMachinePredicate.Not(term)
      }
      val program = PredicateProgram(expression = expression)

      When("the program is validated before evaluation")
      val result = PredicateProgram.validate(program)

      Then("the depth breach has a typed deterministic failure")
      result shouldBe Left(PredicateProgramFailure.DepthLimitExceeded(PredicateProgram.MAXIMUM_DEPTH + 1))
    }

    "reject an unsupported predicate-program version" in {
      Given("a predicate program with an unadmitted version")
      val program = PredicateProgram(version = PredicateProgram.VERSION_1 + 1)

      When("the program is validated")
      val result = PredicateProgram.validate(program)

      Then("the version breach has a typed deterministic failure")
      result shouldBe Left(PredicateProgramFailure.UnsupportedVersion(PredicateProgram.VERSION_1 + 1))
    }

    "reject a predicate beyond the declared node limit" in {
      Given("one aggregate predicate with more than the version-one node bound")
      val program = PredicateProgram(
        expression = StateMachinePredicate.All(
          Vector.fill(PredicateProgram.MAXIMUM_NODES)(StateMachinePredicate.Always)
        )
      )

      When("the program is validated")
      val result = PredicateProgram.validate(program)

      Then("the node breach has a typed deterministic failure")
      result shouldBe Left(PredicateProgramFailure.NodeLimitExceeded(PredicateProgram.MAXIMUM_NODES + 1))
    }

    "report an unavailable declared context field as a typed failure" in {
      Given("a predicate that requires the optional target identifier")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Equals(
          StateMachinePredicateField.TargetIdentifier,
          StateMachinePredicateValue.Text("person-1")
        )
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the predicate is evaluated without that field")
      val result = PredicateProgram.evaluate(program, context)

      Then("the result is a typed missing-field failure, not an ambient lookup")
      result shouldBe Left(PredicateProgramFailure.MissingContextField(StateMachinePredicateField.TargetIdentifier))
    }

    "reject a mismatched typed predicate value" in {
      Given("a text-valued trigger field compared to a boolean literal")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Equals(
          StateMachinePredicateField.EventName,
          StateMachinePredicateValue.Bool(true)
        )
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the pure predicate program is evaluated")
      val result = PredicateProgram.evaluate(program, context)

      Then("the value mismatch is a typed failure")
      result shouldBe Left(
        PredicateProgramFailure.TypeMismatch(
          StateMachinePredicateField.EventName,
          StateMachinePredicateValue.Bool(true),
          StateMachinePredicateValue.Text("publish")
        )
      )
    }

    "report a missing later All term after evaluating an earlier false literal" in {
      Given("an All predicate with a false literal before a missing target-identifier comparison")
      val program = PredicateProgram(
        expression = StateMachinePredicate.All(Vector(
          StateMachinePredicate.Literal(StateMachinePredicateValue.Bool(false)),
          StateMachinePredicate.Equals(
            StateMachinePredicateField.TargetIdentifier,
            StateMachinePredicateValue.Text("person-1")
          )
        ))
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the composite predicate is evaluated in declaration order")
      val result = PredicateProgram.evaluate(program, context)

      Then("the later missing field is reported instead of a short-circuited false result")
      result shouldBe Left(PredicateProgramFailure.MissingContextField(StateMachinePredicateField.TargetIdentifier))
    }

    "report a mismatched later Any term after evaluating an earlier true literal" in {
      Given("an Any predicate with a true literal before an event-name type mismatch")
      val program = PredicateProgram(
        expression = StateMachinePredicate.Any(Vector(
          StateMachinePredicate.Literal(StateMachinePredicateValue.Bool(true)),
          StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Bool(true)
          )
        ))
      )
      val context = StateMachineTriggerContext(_context_identity, eventName = "publish")

      When("the composite predicate is evaluated in declaration order")
      val result = PredicateProgram.evaluate(program, context)

      Then("the later type mismatch is reported instead of a short-circuited true result")
      result shouldBe Left(
        PredicateProgramFailure.TypeMismatch(
          StateMachinePredicateField.EventName,
          StateMachinePredicateValue.Bool(true),
          StateMachinePredicateValue.Text("publish")
        )
      )
    }

    "enforce the UTF-8 literal limit as a property" in {
      Given("generated ASCII literals beyond the version-one byte limit")
      val sizes = Gen.chooseNum(PredicateProgram.MAXIMUM_TEXT_BYTES + 1, PredicateProgram.MAXIMUM_TEXT_BYTES + 64)

      When("every generated literal is validated")
      val property = Prop.forAll(sizes) { size =>
        val program = PredicateProgram(
          expression = StateMachinePredicate.Equals(
            StateMachinePredicateField.EventName,
            StateMachinePredicateValue.Text("x" * size)
          )
        )
        PredicateProgram.validate(program) == Left(PredicateProgramFailure.TextLimitExceeded(size))
      }
      val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(100), property)

      Then("the program never admits an oversized literal")
      result.passed shouldBe true
    }
  }

  "StateMachine normalization types" should {
    "keep a named guard outside the closed predicate language" in {
      Given("a transition identity and a named guard binding")
      val transition = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val guard = MComponent.StateMachineGuardProgram.Named(
        MComponent.StateMachineGuardIdentity(transition, "canPublish")
      )

      When("the normalized guard contract validates the named binding")
      val result = MComponent.StateMachineGuardProgram.validate(guard)

      Then("the binding remains outside the closed predicate language")
      result shouldBe Right(())
    }

    "reject a negative transition priority at construction" in {
      Given("a negative transition priority")

      When("the normalized priority is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.StateMachineTransitionPriority(-1)
      }

      Then("the invalid priority is rejected before normalization")
      result.getMessage should include("priority")
    }

    "retain a phase-correct action plan" in {
      Given("exit, transition, and entry actions with their matching phase and declaration order")
      val identity = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val actions = MComponent.NormalizedStateMachineActionPlan(
        exit = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Exit, 0),
          "actions.leaveDraft"
        )),
        transition = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Transition, 0),
          "actions.publish"
        )),
        entry = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Entry, 0),
          "actions.enterPublished"
        ))
      )

      When("the normalized transition is constructed")
      val result = _transition(identity, actions = actions)

      Then("the phase-correct action plan is retained")
      result.actions shouldBe actions
    }

    "reject a raw action expression reference" in {
      Given("a normalized action that carries raw action text instead of a named local reference")
      val identity = MComponent.StateMachineTransitionIdentity(_machine, 0)

      When("the normalized action is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Transition, 0),
          "actions.publish()"
        )
      }

      Then("the raw action text is rejected at the closed-model boundary")
      result.getMessage should include("named local binding reference")
    }

    "reject an action reference beyond the UTF-8 byte limit" in {
      Given("a named action reference whose ASCII bytes exceed the version-one bound")
      val identity = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val reference = "a" * (PredicateProgram.MAXIMUM_TEXT_BYTES + 1)

      When("the normalized action is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Transition, 0),
          reference
        )
      }

      Then("the oversized named reference is rejected before it becomes an action binding")
      result.getMessage should include("UTF-8 byte limit")
    }

    "reject an action assigned to the wrong plan phase" in {
      Given("an exit action whose identity is assigned the entry phase")
      val identity = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val actions = MComponent.NormalizedStateMachineActionPlan(
        exit = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Entry, 0),
          "leaveDraft"
        ))
      )

      When("the normalized transition is constructed")
      val result = intercept[IllegalArgumentException] {
        _transition(identity, actions = actions)
      }

      Then("the plan phase mismatch is rejected at construction")
      result.getMessage should include("exit action")
    }

    "reject duplicate declaration order within one action-plan phase" in {
      Given("two transition actions with the same declaration order")
      val identity = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val actions = MComponent.NormalizedStateMachineActionPlan(
        transition = Vector(
          MComponent.NormalizedStateMachineAction(
            MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Transition, 0),
            "publish"
          ),
          MComponent.NormalizedStateMachineAction(
            MComponent.StateMachineActionIdentity(identity, MComponent.StateMachineActionPhase.Transition, 0),
            "notify"
          )
        )
      )

      When("the normalized transition is constructed")
      val result = intercept[IllegalArgumentException] {
        _transition(identity, actions = actions)
      }

      Then("the duplicate phase declaration order is rejected at construction")
      result.getMessage should include("declaration order")
    }

    "reject an empty StateMachine normalization diagnostic result" in {
      Given("a rejected normalization with no diagnostics")

      When("the rejected normalization result is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.StateMachineNormalization.Rejected(Vector.empty)
      }

      Then("an empty rejection result cannot escape the normalization boundary")
      result.getMessage should include("diagnostic")
    }

    "reject a normalized StateMachine without an explicit initial state" in {
      Given("a normalized StateMachine with no declared initial-state identity")

      When("the normalized StateMachine is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(identity = _machine)
      }

      Then("the closed model rejects the absent required initial state")
      result.getMessage should include("explicit initial state")
    }

    "reject a shallow-history fallback leaf from another StateMachine" in {
      Given("a transition with a local composite and a foreign fallback leaf")
      val othermachine = StateMachineIdentity("example.other")
      val transition = MComponent.StateMachineTransitionIdentity(_machine, 0)
      val composite = MComponent.StateMachineStateIdentity(_machine, Vector("processing"))
      val foreignfallback = MComponent.StateMachineStateIdentity(othermachine, Vector("default"))
      val trigger = StateMachineTriggerIdentity(_machine, "publish")
      val location = MComponent.StateMachineSourceLocation(Vector("statemachine", "payment", "transition", "0"))

      When("the normalized transition is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachineTransition(
          identity = transition,
          source = None,
          target = MComponent.StateMachineTransitionTarget.ShallowHistory(composite, Some(foreignfallback)),
          trigger = trigger,
          sourceLocation = location
        )
      }

      Then("the foreign fallback metadata is rejected at the semantic boundary")
      result.getMessage should include("target")
    }

    "reject an undeclared initial or source state within its declared StateMachine" in {
      Given("one declared state and same-machine initial and source identities that were not declared")
      val declaredstate = MComponent.StateMachineStateIdentity(_machine, Vector("draft"))
      val undeclaredstate = MComponent.StateMachineStateIdentity(_machine, Vector("unlisted"))
      val transition = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 0),
        source = Some(undeclaredstate)
      )

      When("the normalized StateMachine is constructed")
      val initialresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(undeclaredstate),
          states = Vector(declaredstate)
        )
      }
      val sourceresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(declaredstate),
          states = Vector(declaredstate),
          transitions = Vector(transition)
        )
      }

      Then("both references fail closed rather than relying on same-machine membership")
      initialresult.getMessage should include("initial state")
      sourceresult.getMessage should include("source state")
    }

    "reject an unlisted shallow-history composite or fallback leaf" in {
      Given("declared same-machine states without the required composite ownership relation")
      val composite = MComponent.StateMachineStateIdentity(_machine, Vector("processing"))
      val fallback = MComponent.StateMachineStateIdentity(_machine, Vector("processing", "default"))
      val otherleaf = MComponent.StateMachineStateIdentity(_machine, Vector("other"))
      val unlistedtarget = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 0),
        target = MComponent.StateMachineTransitionTarget.ShallowHistory(composite, Some(fallback))
      )
      val unrelatedfallback = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 1),
        target = MComponent.StateMachineTransitionTarget.ShallowHistory(composite, Some(otherleaf))
      )
      val topology = MComponent.StateMachineTopology(
        composites = Vector(MComponent.StateMachineCompositeTopology(composite, Vector(fallback)))
      )

      When("the normalized StateMachine is constructed with unlinked history targets")
      val compositeresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(composite),
          states = Vector(composite, fallback),
          transitions = Vector(unlistedtarget)
        )
      }
      val fallbackresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(composite),
          states = Vector(composite, fallback, otherleaf),
          transitions = Vector(unrelatedfallback),
          topology = topology
        )
      }

      Then("composite membership and direct fallback ownership are both required")
      compositeresult.getMessage should include("shallow-history target")
      fallbackresult.getMessage should include("shallow-history target")
    }

    "reject an unrelated state claimed as a composite direct leaf" in {
      Given("a history target and write whose declared direct leaf is not an immediate path child")
      val composite = MComponent.StateMachineStateIdentity(_machine, Vector("processing"))
      val unrelatedleaf = MComponent.StateMachineStateIdentity(_machine, Vector("other"))
      val transition = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 0),
        target = MComponent.StateMachineTransitionTarget.ShallowHistory(composite, Some(unrelatedleaf)),
        historywrites = Vector(MComponent.StateMachineHistoryWrite(composite, unrelatedleaf))
      )
      val topology = MComponent.StateMachineTopology(
        composites = Vector(MComponent.StateMachineCompositeTopology(composite, Vector(unrelatedleaf)))
      )

      When("the normalized StateMachine is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(composite),
          states = Vector(composite, unrelatedleaf),
          transitions = Vector(transition),
          topology = topology
        )
      }

      Then("the direct-parent topology relation rejects the fallback and history-write carrier")
      result.getMessage should include("immediate path child")
    }

    "reject a declared composite nested as another composite direct leaf" in {
      Given("a composite whose immediate direct leaf is also declared as a composite")
      val parent = MComponent.StateMachineStateIdentity(_machine, Vector("processing"))
      val nested = MComponent.StateMachineStateIdentity(_machine, Vector("processing", "validating"))
      val leaf = MComponent.StateMachineStateIdentity(_machine, Vector("processing", "validating", "pending"))
      val topology = MComponent.StateMachineTopology(
        composites = Vector(
          MComponent.StateMachineCompositeTopology(parent, Vector(nested)),
          MComponent.StateMachineCompositeTopology(nested, Vector(leaf))
        )
      )

      When("the nested composite topology is constructed")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(parent),
          states = Vector(parent, nested, leaf),
          topology = topology
        )
      }

      Then("the one-level direct-leaf topology rejects a composite beneath a composite")
      result.getMessage should include("declared composite")
    }

    "reject a history write that is not linked to an admitted direct leaf" in {
      Given("a declared composite whose history write names another declared leaf")
      val composite = MComponent.StateMachineStateIdentity(_machine, Vector("processing"))
      val directleaf = MComponent.StateMachineStateIdentity(_machine, Vector("processing", "default"))
      val otherleaf = MComponent.StateMachineStateIdentity(_machine, Vector("other"))
      val transition = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 0),
        historywrites = Vector(MComponent.StateMachineHistoryWrite(composite, otherleaf))
      )
      val topology = MComponent.StateMachineTopology(
        composites = Vector(MComponent.StateMachineCompositeTopology(composite, Vector(directleaf)))
      )

      When("the normalized StateMachine retains that unlinked history write")
      val result = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(composite),
          states = Vector(composite, directleaf, otherleaf),
          transitions = Vector(transition),
          topology = topology
        )
      }

      Then("history writes are constrained by their composite direct-leaf topology")
      result.getMessage should include("history write")
    }

    "reject undeclared or non-final terminal transition identities" in {
      Given("a declared final transition and one ordinary transition")
      val finaltransition = _transition(MComponent.StateMachineTransitionIdentity(_machine, 0))
      val state = MComponent.StateMachineStateIdentity(_machine, Vector("draft"))
      val ordinarytransition = _transition(
        identity = MComponent.StateMachineTransitionIdentity(_machine, 1),
        target = MComponent.StateMachineTransitionTarget.State(state)
      )

      When("the topology names a missing or non-final terminal transition")
      val missingresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(state),
          states = Vector(state),
          transitions = Vector(finaltransition),
          topology = MComponent.StateMachineTopology(
            terminalTransitions = Vector(MComponent.StateMachineTransitionIdentity(_machine, 2))
          )
        )
      }
      val nonfinalresult = intercept[IllegalArgumentException] {
        MComponent.NormalizedStateMachine(
          identity = _machine,
          initialState = Some(state),
          states = Vector(state),
          transitions = Vector(ordinarytransition),
          topology = MComponent.StateMachineTopology(
            terminalTransitions = Vector(ordinarytransition.identity)
          )
        )
      }

      Then("terminal topology can name only declared final transitions")
      missingresult.getMessage should include("terminal transition")
      nonfinalresult.getMessage should include("target final")
    }
  }
}
