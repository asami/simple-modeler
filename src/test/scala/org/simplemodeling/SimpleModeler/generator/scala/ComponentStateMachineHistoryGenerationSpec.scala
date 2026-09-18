package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainComponent
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since Aug. 14, 2026
 * @version Aug. 14, 2026
 * @author ASAMI, Tomoharu
 */
final class ComponentStateMachineHistoryGenerationSpec
  extends AnyWordSpec
  with Matchers
  with GivenWhenThen {

  "Component state-machine Scala generation" should {
    "emit defaulted named shallow-history metadata" in {
      Given("an SComponent carrying a Review history transition and definition")
      val component = _component

      When("the Scala component adapter is generated")
      val source = new Scala3ComponentGenerator(ScalaModel.Context.default)
        .generate(component)
        .take
        .slots
        .map(_.content)
        .mkString("\n")

      Then("the generated runtime calls retain every history contract field")
      source should include("historyCompositeName = Some(\"Review\")")
      source should include("historyFieldName = Some(\"lifecycleHistory\")")
      source should include("historyDirectLeaves = Vector(\"Pending\", \"Approved\")")
      source should include("historyFallbackLeaf = Some(\"Pending\")")
      source should include("HistoryRecordWrite(compositeName = \"Review\", leafName = \"Approved\")")
      source should include("CmlHistoryCompositeDefinition(name = \"Review\"")
    }

    "lower an accepted normalized MComponent definition without changing its canonical identity" in {
      Given("an MComponent state machine with accepted terminal, history, predicate, guard, and action metadata")
      val component = _mcomponent(Some(MComponent.StateMachineNormalization.Accepted(_normalized)))

      When("the component is transformed and its adapter source is generated")
      val source = _generated_source(component)

      Then("the complete canonical normalized ABI is emitted")
      source should include("normalized = Some(org.goldenport.cncf.statemachine.CmlNormalizedStateMachine(")
      source should include("CollectionTransitionRuleProvider with CmlStateMachineDefinitionProvider")
      source should include("initialState = org.goldenport.cncf.statemachine.CmlStateMachineStateIdentity(")
      source should include("CmlStateMachineStateKind.Composite")
      source should include("CmlStateMachineStatePath(Vector(\"Review\", \"Pending\"))")
      source should include("CmlStateMachineTransitionTarget.Final")
      source should include("terminalTransitions = Vector(org.goldenport.cncf.statemachine.CmlStateMachineTransitionIdentity(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), 1))")
      source should include("CmlStateMachineShallowHistoryTarget(")
      source should include("CmlStateMachineHistoryWrite(")
      source should include("\"lifecycleHistory\"")
      source should include("CmlStateMachineTriggerContextFieldIdentity(")
      source should include("\"eventName\"")
      source should include("\"targetIdentifier\"")
      source should include("\"currentState\"")
      source should include("\"candidateState\"")
      source should include("CmlStateMachineScalarType.StringValue")
      source should include("CmlStateMachinePredicate.Present(")
      source should include("CmlStateMachineGuardIdentity(org.goldenport.cncf.statemachine.CmlStateMachineTransitionIdentity(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), 0), \"predicate-0\")")
      source should include("CmlStateMachineGuardIdentity(org.goldenport.cncf.statemachine.CmlStateMachineTransitionIdentity(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), 1), \"canApprove\")")
      source should include("CmlStateMachineActionPhase.Exit, 0), \"clearDraft\"")
      source should include("CmlStateMachineActionPhase.Transition, 0), \"stampSubmitted\"")
      source should include("CmlStateMachineActionPhase.Entry, 0), \"notifyReview\"")
      source should include("CmlStateMachineSourceLocation(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), Vector(\"states\", \"Draft\", \"on\", \"submit\"))")
    }

    "emit all final transition identities in declaration order when topology lists them differently" in {
      Given("an accepted normalized MComponent with two final transitions and reversed topology terminal identities")
      val component = _mcomponent(Some(MComponent.StateMachineNormalization.Accepted(_normalized_with_reversed_terminal_topology)))

      When("the component is transformed and its adapter source is generated")
      val source = _generated_source(component)

      Then("the generated normalized ABI lists exactly the final transitions in declaration order")
      source should include("terminalTransitions = Vector(org.goldenport.cncf.statemachine.CmlStateMachineTransitionIdentity(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), 1), org.goldenport.cncf.statemachine.CmlStateMachineTransitionIdentity(org.goldenport.cncf.statemachine.CmlStateMachineIdentity(\"lifecycle\"), 2))")
    }

    "reject absent and rejected MComponent normalizations during transformation" in {
      Given("MComponent state-machine definitions without an accepted normalization")
      val rejected = _mcomponent(Some(MComponent.StateMachineNormalization.Rejected(Vector(
        MComponent.StateMachineNormalizationDiagnostic(
          code = "invalid-state-machine",
          message = "normalization failed",
          machine = _normalized.identity,
          sourceLocation = MComponent.StateMachineSourceLocation(Vector("states"))
        )
      ))))
      val absent = _mcomponent(None)

      When("the component transformer receives each definition")
      val rejectedresult = _transform(rejected)
      val absentresult = _transform(absent)

      Then("neither definition silently becomes the legacy/raw carrier")
      rejectedresult.isSuccess shouldBe false
      rejectedresult.conclusion.message should include("StateMachine normalization rejected for lifecycle")
      absentresult.isSuccess shouldBe false
      absentresult.conclusion.message should include("StateMachine normalization missing for lifecycle")
    }
  }

  private def _generated_source(component: MDomainComponent): String = {
    val scalacontext = ScalaModel.Context.fromObjects(Vector(component))
    val transformed = ScalaModel.Context.withContext(scalacontext) {
      _transform(component)
        .take
        .collectFirst { case m: SComponent => m }
        .getOrElse(fail("generated component model missing"))
    }
    new Scala3ComponentGenerator(scalacontext)
      .generate(transformed)
      .take
      .slots
      .map(_.content)
      .mkString("\n")
  }

  private def _transform(component: MDomainComponent) =
    ScalaModel.Context.withContext(ScalaModel.Context.fromObjects(Vector(component))) {
      new ComponentScalaModelTransformer().apply((component, Purpose.Plain))
    }

  private def _mcomponent(
    normalization: Option[MComponent.StateMachineNormalization]
  ): MDomainComponent =
    MDomainComponent(
      description = Description.name("Example"),
      objectCore = MObject.Core(MPackageRef("example")),
      componentCore = MComponent.Core(
        entities = Vector.empty,
        stateMachineDefinitions = Vector(
          MComponent.StateMachineDefinition(
            name = "lifecycle",
            states = Vector("Draft", "Review", "Pending", "Approved", "Published"),
            events = Vector("submit", "approve"),
            historyFieldName = Some("lifecycleHistory"),
            historyComposites = Vector(MComponent.StateMachineHistoryComposite("Review", Vector("Pending", "Approved"), Some("Pending"))),
            normalization = normalization
          )
        )
      )
    )

  private def _normalized: MComponent.NormalizedStateMachine = {
    val machine = MComponent.StateMachineIdentity("lifecycle")
    val draft = _state(machine, "Draft")
    val review = _state(machine, "Review")
    val pending = _state(machine, "Review", "Pending")
    val approved = _state(machine, "Review", "Approved")
    val published = _state(machine, "Published")
    val submitidentity = MComponent.StateMachineTransitionIdentity(machine, 0)
    val approveidentity = MComponent.StateMachineTransitionIdentity(machine, 1)
    val submit = MComponent.NormalizedStateMachineTransition(
      identity = submitidentity,
      source = Some(draft),
      target = MComponent.StateMachineTransitionTarget.ShallowHistory(review, Some(pending)),
      trigger = MComponent.StateMachineTriggerIdentity(machine, "submit"),
      sourceLocation = MComponent.StateMachineSourceLocation(Vector("states", "Draft", "on", "submit")),
      priority = MComponent.StateMachineTransitionPriority(3),
      guard = MComponent.StateMachineGuardProgram.Predicate(
        MComponent.PredicateProgram(
          expression = MComponent.StateMachinePredicate.IsPresent(MComponent.StateMachinePredicateField.CurrentState)
        )
      ),
      actions = MComponent.NormalizedStateMachineActionPlan(
        exit = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(submitidentity, MComponent.StateMachineActionPhase.Exit, 0),
          "clearDraft"
        )),
        transition = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(submitidentity, MComponent.StateMachineActionPhase.Transition, 0),
          "stampSubmitted"
        )),
        entry = Vector(MComponent.NormalizedStateMachineAction(
          MComponent.StateMachineActionIdentity(submitidentity, MComponent.StateMachineActionPhase.Entry, 0),
          "notifyReview"
        ))
      )
    )
    val approve = MComponent.NormalizedStateMachineTransition(
      identity = approveidentity,
      source = Some(pending),
      target = MComponent.StateMachineTransitionTarget.Final,
      trigger = MComponent.StateMachineTriggerIdentity(machine, "approve"),
      sourceLocation = MComponent.StateMachineSourceLocation(Vector("states", "Review", "Pending", "on", "approve")),
      priority = MComponent.StateMachineTransitionPriority(1),
      guard = MComponent.StateMachineGuardProgram.Named(
        MComponent.StateMachineGuardIdentity(approveidentity, "canApprove")
      ),
      historyWrites = Vector(MComponent.StateMachineHistoryWrite(review, approved))
    )
    MComponent.NormalizedStateMachine(
      identity = machine,
      initialState = Some(draft),
      states = Vector(draft, review, pending, approved, published),
      transitions = Vector(submit, approve),
      historyFieldName = Some("lifecycleHistory"),
      topology = MComponent.StateMachineTopology(
        composites = Vector(MComponent.StateMachineCompositeTopology(review, Vector(pending, approved))),
        terminalTransitions = Vector(approveidentity)
      )
    )
  }

  private def _normalized_with_reversed_terminal_topology: MComponent.NormalizedStateMachine = {
    val normalized = _normalized
    val machine = normalized.identity
    val finishidentity = MComponent.StateMachineTransitionIdentity(machine, 2)
    val finish = MComponent.NormalizedStateMachineTransition(
      identity = finishidentity,
      source = Some(_state(machine, "Published")),
      target = MComponent.StateMachineTransitionTarget.Final,
      trigger = MComponent.StateMachineTriggerIdentity(machine, "finish"),
      sourceLocation = MComponent.StateMachineSourceLocation(Vector("states", "Published", "on", "finish"))
    )
    normalized.copy(
      transitions = normalized.transitions :+ finish,
      topology = normalized.topology.copy(
        terminalTransitions = Vector(finishidentity, MComponent.StateMachineTransitionIdentity(machine, 1))
      )
    )
  }

  private def _state(
    machine: MComponent.StateMachineIdentity,
    path: String*
  ): MComponent.StateMachineStateIdentity =
    MComponent.StateMachineStateIdentity(machine, path.toVector)

  private def _component: SComponent =
    SComponent(
      core = ClassCore(
        packageName = PackageName("example"),
        declaration = ClassDeclaration.Control,
        className = ClassName("ExampleComponent")
      ),
      componentCore = SComponent.ComponentCore(
        componentName = "example",
        services = Nil,
        stateMachineTransitionRules = Vector(
          SComponent.StateMachineTransitionRule(
            collectionName = "person",
            trigger = SComponent.TransitionTrigger.Update,
            eventName = "resume",
            machineName = Some("lifecycle"),
            stateFieldName = Some("status"),
            fromState = Some("Suspended"),
            toState = Some("Pending"),
            historyCompositeName = Some("Review"),
            historyFieldName = Some("lifecycleHistory"),
            historyDirectLeaves = Vector("Pending", "Approved"),
            historyFallbackLeaf = Some("Pending"),
            expectedHistoryRecordWrites = Vector(SComponent.StateMachineHistoryRecordWrite("Review", "Approved"))
          )
        ),
        stateMachineDefinitions = Vector(
          SComponent.StateMachineDefinition(
            name = "lifecycle",
            states = Vector("Draft", "Pending", "Approved", "Suspended"),
            events = Vector("resume"),
            historyFieldName = Some("lifecycleHistory"),
            historyComposites = Vector(SComponent.StateMachineHistoryComposite("Review", Vector("Pending", "Approved"), Some("Pending")))
          )
        )
      )
    )
}
