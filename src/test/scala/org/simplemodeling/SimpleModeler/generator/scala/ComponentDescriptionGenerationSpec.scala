package org.simplemodeling.SimpleModeler.generator.scala

import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator

/*
 * @since Oct.  7, 2026
 * @version Oct.  7, 2026
 */
final class ComponentDescriptionGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Generated Component descriptive metadata" should {
    "carry service and operation descriptions in runtime builders without changing the operation contract" in {
      Given("one described service and an independently described query with no input and a String result")
      val component = _component(Some("Inspect workflow state. Does not advance it."),
        Some("Return the current request. Caller answers only issued work."))
      When("the normal Scala3 component generator emits the adapter")
      val source = _generate(component)
      Then("descriptions and derived summaries are runtime metadata rather than comments only")
      source should include(".summary(\"Inspect workflow state.\").description(\"Inspect workflow state. Does not advance it.\")")
      source should include(".summary(\"Return the current request.\").description(\"Return the current request. Caller answers only issued work.\")")
      And("the canonical request, response and operation identity are preserved")
      source should include("OperationDefinition.Specification.Builder(\"inspect\")")
      source should include("content = org.goldenport.value.BaseContent.Builder(\"inspect\")")
      source should include("request = RequestDefinition.empty,")
      source should include("ResponseDefinition(result = List(org.goldenport.schema.DataType.Named(\"String\")))")
    }

    "escape authored text as one Scala string literal across quotes, slashes and line boundaries" in {
      Given("authored descriptions with quotes, backslashes, newlines, tabs and Japanese text")
      val descriptions = Gen.nonEmptyListOf(Gen.oneOf('a', 'あ', '\"', '\\', '\n', '\r', '\t'))
        .map(chars => "説明 " + chars.mkString + " 終")
      When("the normal generator processes the description property")
      val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(40), Prop.forAll(descriptions) { text =>
        val source = _generate(_component(Some(text), Some(text)))
        val literal = _literal(text)
        source.split(java.util.regex.Pattern.quote(s".description($literal)"), -1).length == 3
      })
      Then("both owning builders contain the entire authored description in a valid escaped literal")
      result.passed shouldBe true
    }

    "leave omitted and whitespace-only descriptions empty without borrowing other owners' metadata" in {
      Given("service and operation descriptions independently omitted or blank")
      val components = for {
        service <- Vector(None, Some(" \n\t "))
        operation <- Vector(None, Some(" \r\n "))
      } yield _component(service, operation)
      When("each normal adapter is generated")
      val sources = components.map(_generate)
      Then("the builders retain names and do not invent descriptive attributes")
      sources.foreach { source =>
        source should not include ".description("
        source should not include ".summary("
        source should include("content = org.goldenport.value.BaseContent.Builder(\"inspect\")")
        source should include("request = RequestDefinition.empty,")
      }
    }
  }

  private def _generate(component: SComponent): String =
    new Scala3ComponentGenerator(ScalaModel.Context.Default(
      componentNamespace = Some("org.example"), componentLocalId = Some("Inspection")))
      .generate(component).take.slots.map(_.content).mkString("\n")

  private def _component(service: Option[String], operation: Option[String]): SComponent = {
    val method = SMethod(MethodName("inspect"), SMethod.Descriptor.query,
      ParameterSequence.empty, TypeName.Primitive.string, description = operation)
    val definition = SService("example", "Inspection", MethodCompartment(Vector(method)),
      Vector.empty, description = service)
    SComponent(ClassCore(packageName = PackageName("example"), declaration = ClassDeclaration.Control,
      className = ClassName("InspectionComponent")),
      SComponent.ComponentCore(componentName = "inspection", services = List(definition)))
  }

  private def _literal(text: String): String = "\"" + text.flatMap {
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c => c.toString
  } + "\""
}
