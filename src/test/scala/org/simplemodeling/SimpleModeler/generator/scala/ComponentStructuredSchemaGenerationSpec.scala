package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.model.MBuiltinStructuredDataType
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator

/*
 * @since   Oct. 18, 2026
 * @version Oct. 18, 2026
 */
final class ComponentStructuredSchemaGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Operation structured field metadata" should {
    "refuse name-only fallback when the semantic input declaration is absent" in {
      Given("operation metadata naming built-in structured data without a corresponding typed method input")
      val spellings = Vector("record", "json", "semistructureddata", "xjson", "xsemistructureddata")

      When("the actual component generator attempts to emit each incomplete contract")
      val failures = spellings.map(spelling => intercept[Exception] {
        _generate(_component(spelling, None))
      })

      Then("none emits a Named placeholder or guesses a structured codec from the spelling")
      failures.foreach(_.getMessage should include("Operation structured datatype requires semantic input"))
    }

    "carry the corresponding semantic input into the actual operation request schema" in {
      Given("JSON and semi-structured operation fields with explicit semantic method inputs")
      val cases = Vector(
        ("json", TypeName.Structured(MBuiltinStructuredDataType.Json), "XJson"),
        ("record", TypeName.record, "XSemiStructuredData"))

      When("the actual component generator emits each complete contract")
      val sources = cases.map { case (spelling, typename, expected) =>
        _generate(_component(spelling, Some(typename))) -> expected
      }

      Then("the request schema selects the canonical type object from that declaration")
      sources.foreach { case (source, expected) =>
        source should include("datatype = org.goldenport.schema." + expected)
        source should not include "DataType.Named(\"record\")"
        source should not include "DataType.Named(\"json\")"
      }
    }
  }

  "Nonstructured operation metadata" should {
    "retain its existing named datatype instead of selecting the typed class spelling" in {
      Given("lowercase operation powertype metadata with a corresponding typed FetchMethod input")
      val component = _component("fetchmethod", Some(TypeName.vector(TypeName.Plain.create("example", "FetchMethod"))))

      When("the actual component generator emits its operation request schema")
      val source = _generate(component)

      Then("only structured semantic inputs override metadata and the old nonstructured name remains exact")
      source should include("datatype = org.goldenport.schema.DataType.Named(\"fetchmethod\")")
      source should not include "datatype = org.goldenport.schema.DataType.Named(\"FetchMethod\")"
    }
  }

  private def _generate(component: SComponent): String =
    new Scala3ComponentGenerator(ScalaModel.Context.Default(
      componentNamespace = Some("org.example"), componentLocalId = Some("Inspection")))
      .generate(component).take.slots.map(_.content).mkString("\n")

  private def _component(spelling: String, typename: Option[TypeName]): SComponent = {
    val parameters = ParameterSequence(typename.toVector.map(value => Parameter(ParameterName("payload"), value)))
    val method = SMethod(MethodName("inspect"), SMethod.Descriptor.query, parameters, TypeName.Primitive.string)
    val service = SService("example", "Inspection", MethodCompartment(Vector(method)), Vector.empty)
    val operation = SComponent.OperationDefinition(name = "inspect", kind = "query",
      inputType = "request", outputType = "String", inputValueKind = "record",
      parameters = Vector(SComponent.OperationField("payload", spelling)))
    SComponent(ClassCore(packageName = PackageName("example"), declaration = ClassDeclaration.Control,
      className = ClassName("InspectionComponent")),
      SComponent.ComponentCore(componentName = "inspection", services = List(service),
        operationDefinitions = Vector(operation)))
  }
}
