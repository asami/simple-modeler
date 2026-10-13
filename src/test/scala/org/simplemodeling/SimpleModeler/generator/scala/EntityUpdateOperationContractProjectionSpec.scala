package org.simplemodeling.SimpleModeler.generator.scala

import org.goldenport.record.v2.{XEntityId, XInt, XString}
import org.goldenport.values.Designation
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainComponent, MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model.{SComponent, ScalaModel, TypeName}
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since   Jul. 25, 2026
 * @version Oct.  5, 2026
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
      val scalacontext = ScalaModel.Context.fromObjects(Vector(entity, component))
      val transformed = ScalaModel.Context.withContext(scalacontext) {
        new ComponentScalaModelTransformer()
          .apply((component, Purpose.Plain))
          .take
          .collectFirst { case m: SComponent => m }
          .getOrElse(fail("generated component model missing"))
      }
      val source = new Scala3ComponentGenerator(scalacontext)
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

    "structured request fields" which {
      "select nested typed inputs while retaining both update contracts and their metadata" in {
        Given("canonical Json and Record fields, legacy Record, optional and repeated inputs, and an extra revision")
        val entity = _request_entity(List(
          _attribute("originalProposal", MDataType.string, MOne),
          _attribute("payload", MBuiltinStructuredDataType.Json, MOne),
          _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MOne),
          _attribute("legacyEvidence", MObjectAttributeType(MObjectRef.record), MOne),
          _attribute("optionalPayload", MBuiltinStructuredDataType.Json, MZeroOne),
          _attribute("optionalEvidence", MBuiltinStructuredDataType.SemiStructuredData, MZeroOne),
          _attribute("payloads", MBuiltinStructuredDataType.Json, MZeroMore),
          _attribute("evidences", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore)
        ))
        val fields = Vector(
          MComponent.OperationField("originalProposal", "string"),
          MComponent.OperationField("payload", "json", multiplicity = "?",
            controlType = Some("textarea"), placeholder = Some("payload hint"),
            help = Some("declared payload"), required = Some(false),
            confidentiality = Some("private"), constraints = List(LiteralConstraint("max-length", 240)),
            update = Some(MComponent.OperationUpdateField("1", false))),
          MComponent.OperationField("evidence", "semistructureddata", multiplicity = "?"),
          MComponent.OperationField("legacyEvidence", "record", multiplicity = "?"),
          MComponent.OperationField("optionalPayload", "json", multiplicity = "?"),
          MComponent.OperationField("optionalEvidence", "record", multiplicity = "?"),
          MComponent.OperationField("payloads", "json", multiplicity = "*"),
          MComponent.OperationField("evidences", "semistructureddata", multiplicity = "*")
        )
        val component = _request_component(entity, fields)

        When("the actual component is transformed and its Scala operation adapters are generated")
        val (transformed, source) = _generate_request_component(entity, component)

        Then("each update variant selects canonical datatypes per field and preserves String and the extra revision")
        Vector("UpdateProposal", "UpdateProposalRecord").foreach { name =>
          val block = _operation_block(source, name)
          Vector("payload", "optionalPayload", "payloads").foreach { field =>
            _request_field(block, field) should include("datatype = org.goldenport.schema.XJson,")
          }
          Vector("evidence", "legacyEvidence", "optionalEvidence", "evidences").foreach { field =>
            _request_field(block, field) should include("datatype = org.goldenport.schema.XSemiStructuredData,")
          }
          _request_field(block, "originalProposal") should include("datatype = org.goldenport.schema.XString,")
          _request_field(block, "cncfRevision") should include("datatype = org.goldenport.schema.XLong,")
          block should include("cncfRevision: Long")
          block should include("successOrRecordNotFound[Long](\"cncfRevision\"")
          _request_field(block, "optionalPayload") should include("multiplicity = org.goldenport.schema.Multiplicity.ZeroOne")
          _request_field(block, "optionalEvidence") should include("multiplicity = org.goldenport.schema.Multiplicity.ZeroOne")
          _request_field(block, "payloads") should include("multiplicity = org.goldenport.schema.Multiplicity.ZeroMore")
          _request_field(block, "evidences") should include("multiplicity = org.goldenport.schema.Multiplicity.ZeroMore")
          val payload = _request_field(block, "payload")
          payload should include("multiplicity = org.goldenport.schema.Multiplicity.ZeroOne")
          payload should include("controlType = Some(\"textarea\")")
          payload should include("placeholder = Some(\"payload hint\")")
          payload should include("help = Some(\"declared payload\")")
          payload should include("required = Some(false)")
          payload should include("maxLength = Some(240)")
          payload should include("DataConfidentiality.getOrPublic(Some(\"private\"))")
        }

        And("operation metadata remains authoritative and the Record variant keeps its explicit response")
        transformed.services.flatMap(_.methods).foreach { method =>
          val types = method.parameters.parameters.find(_.name.name == "action").flatMap(_.value).get
            .parameterSequence.parameters.find(_.name.name == "entity").flatMap(_.value).get
            .parameterSequence.parameters.map(x => x.name.name -> x.typeName).toMap
          _type_leaf(types("payload")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.Json)
          _type_leaf(types("evidence")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.SemiStructuredData)
          _type_leaf(types("legacyEvidence")) shouldBe TypeName.Plain.create("org.goldenport.record", "Record")
          _type_leaf(types("optionalEvidence")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.SemiStructuredData)
          _type_leaf(types("evidences")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.SemiStructuredData)
        }
        transformed.operationDefinitions.foreach { operation =>
          operation.parameters.map(_.name) shouldBe fields.map(_.name)
          operation.parameters.map(_.multiplicity) shouldBe fields.map(_.multiplicity)
          val payload = operation.parameters.find(_.name == "payload").get
          payload.datatype shouldBe "json"
          payload.controlType shouldBe Some("textarea")
          payload.confidentiality shouldBe Some("private")
          payload.required shouldBe Some(false)
          payload.update shouldBe Some(SComponent.OperationUpdateField("1", false))
        }
        source should include("CmlOperationUpdateField(sourceMultiplicity = \"1\", nullAllowed = false)")
        _operation_block(source, "UpdateProposalRecord") should include(
          "ResponseDefinition(result = List(org.goldenport.schema.DataType.Named(\"Record\")))"
        )
      }

      "retain custom Json and Record declarations and unmatched metadata fallback" in {
        Given("registered custom Json and Record values backed by XString and metadata-only fields")
        val values = Vector("Json", "Record").map { name =>
          MDomainValue(
            description = Description.name(name), affiliation = MPackageRef("org.example.custom"),
            stereotypes = Nil, base = None, traits = Nil, powertypes = Nil,
            attributes = List(_attribute("value", MDataType.string, MOne)), operations = Nil
          )
        }
        val entity = _request_entity(List(
          _attribute("customJson", MDataType(Designation("Json"), XString, MPackageRef("org.example.custom")), MZeroOne),
          _attribute("customRecord", MDataType(Designation("Record"), XString, MPackageRef("org.example.custom")), MZeroMore),
          _attribute("originalProposal", MDataType.string, MOne)
        ))
        val fields = Vector(
          MComponent.OperationField("customJson", "Json", multiplicity = "?"),
          MComponent.OperationField("customRecord", "Record", multiplicity = "*"),
          MComponent.OperationField("originalProposal", "string"),
          MComponent.OperationField("metadataJson", "json"),
          MComponent.OperationField("metadataRecord", "record"),
          MComponent.OperationField("metadataRevision", "long")
        )
        val component = _request_component(entity, fields)

        When("the actual registered model is transformed and generated")
        val (transformed, source) = _generate_request_component(entity, component, values)

        Then("custom names and absent typed inputs preserve their existing field-specific string fallback")
        transformed.services.flatMap(_.methods).foreach { method =>
          val types = method.parameters.parameters.find(_.name.name == "action").flatMap(_.value).get
            .parameterSequence.parameters.find(_.name.name == "entity").flatMap(_.value).get
            .parameterSequence.parameters.map(x => x.name.name -> x.typeName).toMap
          _type_leaf(types("customJson")) shouldBe TypeName.Plain.create("org.example.custom", "Json")
          _type_leaf(types("customRecord")) shouldBe TypeName.Plain.create("org.example.custom", "Record")
        }
        Vector("UpdateProposal", "UpdateProposalRecord").foreach { name =>
          val block = _operation_block(source, name)
          Vector("customJson", "metadataJson").foreach { field =>
            _request_field(block, field) should include("datatype = org.goldenport.schema.DataType.Named(\"json\")")
          }
          Vector("customRecord", "metadataRecord").foreach { field =>
            _request_field(block, field) should include("datatype = org.goldenport.schema.DataType.Named(\"record\")")
          }
          fields.foreach { field =>
            val expression = _request_field(block, field.name)
            expression should not include "org.goldenport.schema.XJson"
            expression should not include "org.goldenport.schema.XSemiStructuredData"
          }
          _request_field(block, "originalProposal") should include("datatype = org.goldenport.schema.XString,")
          _request_field(block, "metadataRevision") should include("datatype = org.goldenport.schema.XLong,")
        }
      }
    }
  }

  private def _type_leaf(typename: TypeName): TypeName = {
    typename match {
      case TypeName.Container(_, child) => {
        _type_leaf(child)
      }
      case leaf => {
        leaf
      }
    }
  }

  private def _request_field(block: String, field: String): String = {
    val request = block.linesIterator.find(_.contains("request = RequestDefinition(")).getOrElse(fail("request schema missing"))
    request.split("ParameterDefinition\\(content = ").find(
      _.startsWith(s"""org.goldenport.value.BaseContent.simple("$field")""")
    ).getOrElse(fail(s"request field missing: $field"))
  }

  private def _generate_request_component(
    entity: MDomainResource,
    component: MDomainComponent,
    values: Vector[MDomainValue] = Vector.empty
  ): (SComponent, String) = {
    val context = ScalaModel.Context.fromObjects(values ++ Vector(entity, component))
    val transformed = ScalaModel.Context.withContext(context) {
      new ComponentScalaModelTransformer().apply((component, Purpose.Plain)).take
        .collectFirst { case m: SComponent => m }.getOrElse(fail("generated component model missing"))
    }
    val source = new Scala3ComponentGenerator(context).generate(transformed).take.slots.map(_.content).mkString("\n")
    (transformed, source)
  }

  private def _request_component(entity: MDomainResource, fields: Vector[MComponent.OperationField]): MDomainComponent = {
    val pkg = MPackage("example", MPackageRef.default, Vector.empty)
    val operations = Vector("updateProposal", "updateProposalRecord").map { name =>
      MOperation.command(name, List(
        MParameter("entity", MEntityValue.update(entity)),
        MParameter("cncfRevision", MDataType.create("long"))
      ), if (name == "updateProposalRecord") { MResult(MObjectRef.record) } else { MResult.unit })
    }
    MDomainComponent(
      Description.name("Example"),
      MObject.Core(MPackageRef("example"), services = List(MService(pkg, "Entity", operations))),
      MComponent.Core(entities = Vector(entity), operationDefinitions = operations.map { operation =>
        MComponent.OperationDefinition(name = operation.name, kind = "COMMAND", inputType = "Proposal",
          outputType = if (operation.name == "updateProposalRecord") { "Record" } else { "unit" },
          inputValueKind = "command", parameters = fields)
      })
    )
  }

  private def _attribute(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute = {
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)
  }

  private def _request_entity(attributes: List[MAttribute]): MDomainResource = {
    _entity.copy(description = Description.name("Proposal"), attributes = _entity.attributes.take(1) ++ attributes)
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

  private def _entity: MDomainResource = {
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
}
