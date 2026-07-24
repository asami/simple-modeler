package org.simplemodeling.SimpleModeler.generator.scala

import org.goldenport.record.v2.{XEntityId, XInt}
import org.goldenport.values.Designation
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainComponent, MDomainResource}
import org.simplemodeling.SimpleModeler.generator.scala.model.{SComponent, ScalaModel}
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since   Jul. 25, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
final class EntityUpdateOperationContractProjectionSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  private val _operation_marker =
    "(?m)^    object [A-Za-z0-9]+Operation extends OperationDefinition".r

  "Entity update operation projection" should {
    "compose flattened update fields with additional parameters and the explicit result" in {
      Given("an Entity update value, an additional revision parameter, and a Record result")
      val entity = _entity
      val operation = MOperation.command(
        "updatePerson",
        List(
          MParameter("entity", MEntityValue.update(entity)),
          MParameter("cncfRevision", MDataType.create("long"))
        ),
        MResult(MObjectRef.record)
      )
      val component = _component(entity, operation)

      When("the component operation adapter is generated")
      ScalaModelTransformer.clearObjectRegistry()
      ScalaModelTransformer.registerObject(entity)
      val transformed = new ComponentScalaModelTransformer()
        .apply((component, Purpose.Plain))
        .take
        .collectFirst { case m: SComponent => m }
        .getOrElse(fail("generated component model missing"))
      val source = new Scala3ComponentGenerator(ScalaModel.Context.default)
        .generate(transformed)
        .take
        .slots
        .map(_.content)
        .mkString("\n")

      Then("the request schema contains both flattened fields and the revision")
      val operationblock = _operation_block(source, "UpdatePerson")
      operationblock should include("BaseContent.simple(\"id\")")
      operationblock should include("BaseContent.simple(\"name\")")
      operationblock should include("BaseContent.simple(\"age\")")
      operationblock should include("BaseContent.simple(\"cncfRevision\")")

      And("the command and decoder require the same revision parameter")
      operationblock should include("cncfRevision: Long")
      operationblock should include("successOrRecordNotFound[Long](\"cncfRevision\"")

      And("the explicit Record response is preserved")
      operationblock should include(
        "ResponseDefinition(result = List(org.goldenport.schema.DataType.Named(\"Record\")))"
      )

      And("an unrelated model-defined response remains authoritative")
      val inspectionblock = _operation_block(source, "InspectPerson")
      inspectionblock should include(
        "ResponseDefinition(result = List(org.goldenport.schema.DataType.Named(\"DeclaredResult\")))"
      )
      inspectionblock should not include
        "ResponseDefinition(result = List(org.goldenport.schema.DataType.Named(\"Record\")))"
    }
  }

  private def _operation_block(content: String, operation: String): String = {
    val marker = s"object ${operation}Operation extends OperationDefinition"
    val starts = _operation_marker.findAllMatchIn(content).map(_.start).toVector
    val start = starts.find(x => content.startsWith(s"    $marker", x)).getOrElse(-1)
    withClue(s"generated operation marker not found: $marker") {
      start should be >= 0
    }
    val end = starts.find(_ > start).getOrElse(content.length)
    content.substring(start, end)
  }

  private def _component(
    entity: MDomainResource,
    operation: MOperation
  ): MDomainComponent = {
    val pkg = MPackage("example", MPackageRef.default, Vector.empty)
    val inspection = MOperation.command(
      "inspectPerson",
      List(MParameter("name", MDataType.string)),
      MResult(MObjectRef.record)
    )
    val service = MService(pkg, "Entity", Seq(operation, inspection))
    MDomainComponent(
      Description.name("Example"),
      MObject.Core(MPackageRef("example"), services = List(service)),
      MComponent.Core(
        entities = Vector(entity),
        operationDefinitions = Vector(
          MComponent.OperationDefinition(
            name = "updatePerson",
            kind = "COMMAND",
            inputType = "Person",
            outputType = "unit",
            inputValueKind = "command",
            parameters = Vector(
              MComponent.OperationField("id", "entityid"),
              MComponent.OperationField("name", "string", multiplicity = "?"),
              MComponent.OperationField("age", "int", multiplicity = "?")
            )
          ),
          MComponent.OperationDefinition(
            name = "inspectPerson",
            kind = "COMMAND",
            inputType = "string",
            outputType = "DeclaredResult",
            inputValueKind = "command",
            parameters = Vector(
              MComponent.OperationField("name", "string")
            )
          )
        )
      )
    )
  }

  private def _entity: MDomainResource =
    MDomainResource(
      description = Description.name("Person"),
      affiliation = MPackageRef("example"),
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(
          Designation("id"),
          MDataType(Designation("EntityId"), XEntityId, MPackageRef.default),
          MOne,
          Nil,
          None
        ),
        MAttribute(
          Designation("name"),
          MDataType.string,
          MZeroOne,
          Nil,
          None
        ),
        MAttribute(
          Designation("age"),
          MDataType(Designation("Age"), XInt, MPackageRef.default),
          MZeroOne,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )
}
