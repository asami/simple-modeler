package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator

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
  }

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
