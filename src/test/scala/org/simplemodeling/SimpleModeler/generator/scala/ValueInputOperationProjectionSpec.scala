package org.simplemodeling.SimpleModeler.generator.scala

import org.goldenport.values.Designation
import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainComponent, MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since   Oct. 19, 2026
 * @version Oct. 19, 2026
 */
final class ValueInputOperationProjectionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Ordinary domain value operation inputs" should {
    "project real named commands" which {
      "share ordered semantic fields between the input carrier, request schema and command action" in {
        Given("an actual ProposalInput value with String, scalar Json, Record and optional or repeated structured fields")
        val attributes = List(
          _attribute("originalProposal", MDataType.string, MOne),
          _attribute("payload", MBuiltinStructuredDataType.Json, MOne),
          _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MOne),
          _attribute("optionalPayload", MBuiltinStructuredDataType.Json, MZeroOne),
          _attribute("optionalEvidence", MBuiltinStructuredDataType.SemiStructuredData, MZeroOne),
          _attribute("payloads", MBuiltinStructuredDataType.Json, MZeroMore),
          _attribute("evidences", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore),
          _attribute("requiredPayloads", MBuiltinStructuredDataType.Json, MOneMore),
          _attribute("requiredEvidences", MBuiltinStructuredDataType.SemiStructuredData, MOneMore))
        val input = _value("ProposalInput", attributes)
        val fields = _fields(attributes).map {
          case field if field.name == "payload" => field.copy(
            controlType = Some("textarea"), help = Some("one JSON document, including an array"),
            required = Some(true), confidentiality = Some("private"))
          case field => field
        }
        val component = _component(_command(input), "ProposalInput", "COMMAND_VALUE", fields)

        When("the normal component transformer and actual Scala3ComponentGenerator process the semantic input")
        val (transformed, source) = _generate(component, Vector(input))
        val carrier = _carrier(transformed)
        val action = _action_declaration(source, "ProposalInput")
        val actionfields = _action_fields(action).toMap
        val request = _request_line(source)

        Then("Parameter.value retains the actual normal value identity, field order, types and declared cardinalities")
        carrier.className.name shouldBe "ProposalInput"
        carrier.directive.purpose shouldBe Some(Purpose.Plain)
        carrier.directive.classKind should not be Some(ClassKind.EntityValue)
        carrier.parameterSequence.parameters.map(_.name.name) shouldBe attributes.map(_.name).toVector
        carrier.parameterSequence.parameters.map(_.typeName) shouldBe Vector(
          TypeName.Primitive.string, TypeName.Structured(MBuiltinStructuredDataType.Json), TypeName.record,
          TypeName.option(TypeName.Structured(MBuiltinStructuredDataType.Json)), TypeName.option(TypeName.record),
          _vector_type(TypeName.Structured(MBuiltinStructuredDataType.Json)), _vector_type(TypeName.record),
          TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"),
            TypeName.Structured(MBuiltinStructuredDataType.Json)),
          TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"), TypeName.record))

        And("the actual generated command has the same ordered leaf fields and no self-named nested input")
        _action_fields(action).map(_._1) shouldBe Vector("request") ++ attributes.map(_.name)
        actionfields("payload") shouldBe "io.circe.Json"
        actionfields("evidence") shouldBe "org.goldenport.record.Record"
        actionfields("optionalPayload") shouldBe "Option[io.circe.Json]"
        actionfields("optionalEvidence") shouldBe "Option[org.goldenport.record.Record]"
        actionfields("payloads") shouldBe "Vector[io.circe.Json]"
        actionfields("evidences") shouldBe "Vector[org.goldenport.record.Record]"
        actionfields("requiredPayloads") shouldBe "cats.data.NonEmptyVector[io.circe.Json]"
        actionfields("requiredEvidences") shouldBe "cats.data.NonEmptyVector[org.goldenport.record.Record]"
        action should not include "record:"
        source should not include "ProposalInput.createC(request.toRecord)"
        source should include("payload <- Consequence.successOrRecordNotFound[io.circe.Json](\"payload\", request.toRecord)")
        source should include("optionalPayload <- request.toRecord.getAsC[io.circe.Json](\"optionalPayload\")")
        source should include("payloads <- request.toRecord.getAsC[scala.Vector[io.circe.Json]](\"payloads\").map(_.getOrElse(Vector.empty))")
        source should include("requiredPayloads <- Consequence.successOrRecordNotFound[cats.data.NonEmptyVector[io.circe.Json]](\"requiredPayloads\", request.toRecord)")
        source should include("evidence <- Consequence.successOrRecordNotFound[org.goldenport.record.Record](\"evidence\", request.toRecord)")
        source should include("optionalEvidence <- request.toRecord.getAsC[org.goldenport.record.Record](\"optionalEvidence\")")
        source should include("evidences <- request.toRecord.getAsC[scala.Vector[org.goldenport.record.Record]](\"evidences\").map(_.getOrElse(Vector.empty))")
        source should include("requiredEvidences <- Consequence.successOrRecordNotFound[cats.data.NonEmptyVector[org.goldenport.record.Record]](\"requiredEvidences\", request.toRecord)")
        source should include("} yield ProposalInput(request, originalProposal, payload, evidence, optionalPayload, optionalEvidence, payloads, evidences, requiredPayloads, requiredEvidences)")

        And("request metadata selects canonical schema objects without changing the scalar Json document cardinality")
        _request_names(request) shouldBe attributes.map(_.name).toVector
        Vector("payload", "optionalPayload", "payloads", "requiredPayloads").foreach { name =>
          _request_field(request, name) should include("datatype = org.goldenport.schema.XJson,")
        }
        Vector("evidence", "optionalEvidence", "evidences", "requiredEvidences").foreach { name =>
          _request_field(request, name) should include("datatype = org.goldenport.schema.XSemiStructuredData,")
        }
        _request_field(request, "originalProposal") should include("datatype = org.goldenport.schema.XString,")
        _request_field(request, "payload") should include("multiplicity = org.goldenport.schema.Multiplicity.One)")
        _request_field(request, "payload") should include("controlType = Some(\"textarea\")")
        _request_field(request, "payload") should include("help = Some(\"one JSON document, including an array\")")
        _request_field(request, "payload") should include("required = Some(true)")
        _request_field(request, "payload") should include("DataConfidentiality.getOrPublic(Some(\"private\"))")
        _request_field(request, "optionalPayload") should include("Multiplicity.ZeroOne")
        _request_field(request, "payloads") should include("Multiplicity.ZeroMore")
        _request_field(request, "requiredPayloads") should include("Multiplicity.OneMore")
        _request_field(request, "requiredEvidences") should include("Multiplicity.OneMore")
        source should not include "DataType.Named(\"json\")"
        source should not include "DataType.Named(\"record\")"
      }

      "normalize explicit command kind metadata with the root locale" in {
        Given("a normal value selected by mixed-case COMMAND_VALUE metadata with surrounding whitespace")
        val input = _value("SelectedInput", List(_attribute("message", MDataType.string, MOne)))
        val component = _component(_command(input), "SelectedInput", "  cOmMaNd_VaLuE  ", _fields(input.attributes))

        When("the real command is transformed and generated")
        val (_, source) = _generate(component, Vector(input))

        Then("the selected name and ordinary String leaf appear in the actual action")
        _action_fields(_action_declaration(source, "SelectedInput")) shouldBe
          Vector("request" -> "Request", "message" -> "String")
        source should not include "SelectedInput.createC(request.toRecord)"
      }

      "keep custom Json and Record value names independent of structured datatype identity" in {
        Given("actual custom values named Json and Record whose declared attribute is String")
        val inputs = Vector("Json", "Record").map(name =>
          _value(name, List(_attribute("text", MDataType.string, MOne))))

        When("normal component transformation and named command generation use each actual custom value")
        val generated = inputs.map { input =>
          _generate(_component(_command(input), input.name, "COMMAND_VALUE", _fields(input.attributes)), Vector(input))
        }

        Then("the custom carrier identities and String fields never select canonical structured schemas")
        generated.zip(inputs).foreach { case ((transformed, source), input) =>
          _carrier(transformed).className.name shouldBe input.name
          _carrier(transformed).parameterSequence.parameters.map(_.typeName) shouldBe Vector(TypeName.Primitive.string)
          _action_fields(_action_declaration(source, input.name)) shouldBe Vector("request" -> "Request", "text" -> "String")
          _request_field(_request_line(source), "text") should include("datatype = org.goldenport.schema.XString,")
          source should not include "datatype = org.goldenport.schema.XJson"
          source should not include "datatype = org.goldenport.schema.XSemiStructuredData"
        }
      }
    }

    "preserve compatibility gates" which {
      "retain the wrapper for unmatched metadata names and non-command-value kinds" in {
        Given("one normal value with a mismatched selected name, QUERY_VALUE, RECORD or legacy command metadata")
        val input = _value("SelectedInput", List(_attribute("message", MDataType.string, MOne)))
        val selections = Vector("OtherInput" -> "COMMAND_VALUE", "SelectedInput" -> "QUERY_VALUE",
          "SelectedInput" -> "RECORD", "SelectedInput" -> "command")

        When("the actual component generator processes these compatibility contracts")
        val sources = selections.map { case (name, kind) =>
          _generate(_component(_command(input), name, kind, _fields(input.attributes)), Vector(input))._2
        }

        Then("each actual action retains its original nested input rather than flattening the message")
        sources.zip(Vector("OtherInput", "SelectedInput", "SubmitCommand", "SubmitCommand")).foreach {
          case (source, name) =>
            _action_fields(_action_declaration(source, name)) shouldBe
              Vector("request" -> "Request", "record" -> "SelectedInput")
            source should include("example.SelectedInput.createC(request.toRecord)")
        }
      }

      "retain extra explicit parameters beside a normal value input" in {
        Given("COMMAND_VALUE metadata and a normal value accompanied by a revision parameter")
        val input = _value("SelectedInput", List(_attribute("message", MDataType.string, MOne)))
        val operation = MOperation.command("submit", List(MParameter("record", input),
          MParameter("revision", MDataType.create("long"))), MResult.unit)
        val component = _component(operation, "SelectedInput", "COMMAND_VALUE", _fields(input.attributes))

        When("the real component action is generated")
        val (_, source) = _generate(component, Vector(input))

        Then("the wrapper preserves both explicit inputs in order")
        _action_fields(_action_declaration(source, "SelectedInput")) shouldBe
          Vector("request" -> "Request", "record" -> "SelectedInput", "revision" -> "Long")
        _request_field(_request_line(source), "revision") should include("datatype = org.goldenport.schema.XLong,")
      }

      "keep an empty normal value on the existing wrapper projection" in {
        Given("an actual empty value selected by matching COMMAND_VALUE metadata")
        val input = _value("EmptyInput", Nil)
        val component = _component(_command(input), "EmptyInput", "COMMAND_VALUE", Vector.empty)

        When("the actual component is transformed and generated")
        val (transformed, source) = _generate(component, Vector(input))

        Then("the empty semantic carrier is retained without inventing leaf parameters")
        _carrier(transformed).parameterSequence.parameters shouldBe empty
        _action_fields(_action_declaration(source, "EmptyInput")) shouldBe
          Vector("request" -> "Request", "record" -> "EmptyInput")
      }

      "preserve the legacy string-only Health Record contract" in {
        Given("a Health query accepting only opaque Record and declaring an ordinary String status field")
        val operation = MOperation.query("health", List(MParameter.record), MResult(MObjectRef.record))
        val component = _component(operation, "HealthRecord", "RECORD",
          Vector(MComponent.OperationField("status", "string")))

        When("normal component transformation and generation process Health")
        val (transformed, source) = _generate(component, Vector.empty)

        Then("Record has no invented carrier and the request retains its String metadata")
        _wrapper(transformed).parameterSequence.parameters.head.value shouldBe None
        _action_fields(_action_declaration(source, "HealthQuery")) shouldBe
          Vector("request" -> "Request", "record" -> "Record")
        _request_field(_request_line(source), "status") should include("datatype = org.goldenport.schema.XString,")
        source should include("Consequence.success(request.toRecord)")
        source should not include "datatype = org.goldenport.schema.XJson"
        source should not include "datatype = org.goldenport.schema.XSemiStructuredData"
      }

      "retain the existing entity update family even under matching COMMAND_VALUE metadata" in {
        Given("an actual entity Update carrier with either legacy command or matching COMMAND_VALUE metadata")
        val entity = MDomainResource(description = Description.name("Person"), affiliation = MPackageRef("example"),
          base = None, traits = Nil, powertypes = Nil,
          attributes = List(_attribute("nickname", MDataType.string, MZeroOne)),
          associations = Nil, operations = Nil, stateMachines = Nil)
        val operation = MOperation.command("submit", List(MParameter("entity", MEntityValue.update(entity))), MResult.unit)
        val components = Vector("command", "COMMAND_VALUE").map(kind =>
          _component(operation, "Person", kind, Vector(MComponent.OperationField("nickname", "string", multiplicity = "?")),
            Vector(entity)))

        When("both actual component contracts are transformed and generated")
        val generated = components.map(_generate(_, Vector(entity)))

        Then("entity purpose, kind, Update child and wrapper action projection retain their prior semantics")
        generated.zip(Vector("SubmitCommand", "Person")).foreach { case ((transformed, source), name) =>
          val carrier = _carrier(transformed)
          carrier.directive.purpose shouldBe Some(Purpose.Update)
          carrier.directive.classKind shouldBe Some(ClassKind.EntityValue)
          carrier.parameterSequence.parameters.find(_.name.name == "nickname").get.typeName shouldBe
            TypeName.Container(TypeName.Plain.create("org.simplemodeling.model.directive", "Update"), TypeName.Primitive.string)
          _action_fields(_action_declaration(source, name)).map(_._1) shouldBe Vector("request", "entity")
          _action_declaration(source, name) should include("entity: _root_.example.entity.update.Person")
          _request_field(_request_line(source), "nickname") should include("datatype = org.goldenport.schema.XString,")
        }
      }
    }

    "retain declared schema identity" which {
      "carry ordered mixed attributes and multiplicities across at least one hundred shrinking samples" in {
        Given("independent safe field names with ordered String, Json and Record attributes and sampled multiplicities")
        val samples = Gen.choose(3, 8).flatMap(size =>
          Gen.listOfN(size, Gen.zip(Gen.choose(0, 1000000), Gen.choose(0, 3))))
        val datatypes = Vector[MAttributeType](MDataType.string, MBuiltinStructuredDataType.Json,
          MBuiltinStructuredDataType.SemiStructuredData)
        val types = Vector[TypeName](TypeName.Primitive.string, TypeName.Structured(MBuiltinStructuredDataType.Json), TypeName.record)
        val multiplicities = Vector[MMultiplicity](MOne, MZeroOne, MZeroMore, MOneMore)

        When("ScalaCheck transforms actual component inputs with list, tuple and numeric shrinking and no discarded preconditions")
        val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(100), Prop.forAll(samples) { sample =>
          Given("one shrinking sample's independent field names, mixed semantic declarations and cardinalities")
          val attributes = sample.zipWithIndex.map { case ((seed, cardinality), index) =>
            _attribute("field" + index + "x" + java.lang.Integer.toUnsignedString(seed, 36),
              datatypes(index % datatypes.size), multiplicities(java.lang.Math.floorMod(cardinality, multiplicities.size)))
          }
          val input = _value("PropertyInput", attributes)
          val component = _component(_command(input), "PropertyInput", "COMMAND_VALUE", _fields(attributes))
          val expected = sample.zipWithIndex.map { case ((_, cardinality), index) =>
            val leaf = types(index % types.size)
            java.lang.Math.floorMod(cardinality, multiplicities.size) match {
              case 0 => leaf
              case 1 => TypeName.option(leaf)
              case 2 => _vector_type(leaf)
              case _ => TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"), leaf)
            }
          }.toVector

          When("the normal component transformer carries the sample's actual value into its operation")
          val transformed = _transform(component, Vector(input))._1
          val carrier = _carrier(transformed)

          Then("the actual schema carrier retains identity, distinct ordered names and exact structured children and containers")
          carrier.className.name shouldBe "PropertyInput"
          carrier.directive.purpose shouldBe Some(Purpose.Plain)
          carrier.parameterSequence.parameters.map(_.name.name) shouldBe attributes.map(_.name).toVector
          carrier.parameterSequence.parameters.map(_.name.name).distinct.size shouldBe attributes.size
          carrier.parameterSequence.parameters.map(_.typeName) shouldBe expected
          Prop.passed
        })

        Then("at least one hundred actual samples succeed and none is discarded")
        result.passed shouldBe true
        result.succeeded should be >= 100
        result.discarded shouldBe 0
      }

      "preserve typed-object body recursion into a normal value" in {
        Given("an actual typed Vector body containing a normal value with a required Json leaf")
        val input = _value("BodyInput", List(_attribute("payload", MBuiltinStructuredDataType.Json, MOne)))
        val operation = MOperation.command("submit", List(MParameter.vector("records", input)), MResult.unit)
        val component = _component(operation, "BodyInput", "RECORD", _fields(input.attributes))

        When("the normal component transformer follows the existing typed-object body recursion")
        val transformed = _transform(component, Vector(input))._1

        Then("the outer container and actual inner schema carrier both retain their declared identity")
        _wrapper(transformed).parameterSequence.parameters.head.typeName shouldBe
          TypeName.Container(TypeName.Plain.create("scala.lang", "Vector"), TypeName.Plain.create("example", "BodyInput"))
        _carrier(transformed).className.name shouldBe "BodyInput"
        _carrier(transformed).parameterSequence.parameters.map(_.typeName) shouldBe
          Vector(TypeName.Structured(MBuiltinStructuredDataType.Json))
      }
    }
  }

  private def _attribute(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute =
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)

  private def _value(name: String, attributes: List[MAttribute]): MDomainValue =
    MDomainValue(Description.name(name), MPackageRef("example"), Nil, None, Nil, Nil, attributes, Nil)

  private def _command(input: MDomainValue): MOperation =
    MOperation.command("submit", List(MParameter("record", input)), MResult.unit)

  private def _fields(attributes: List[MAttribute]): Vector[MComponent.OperationField] =
    attributes.map { attribute =>
      val datatype = attribute.attributeType match {
        case MBuiltinStructuredDataType.Json => "json"
        case MBuiltinStructuredDataType.SemiStructuredData => "record"
        case _ => "string"
      }
      val multiplicity = attribute.multiplicity match {
        case MOne => "1"
        case MZeroOne => "?"
        case MZeroMore => "*"
        case MOneMore => "+"
      }
      MComponent.OperationField(attribute.name, datatype, multiplicity = multiplicity)
    }.toVector

  private def _component(
    operation: MOperation,
    inputname: String,
    inputkind: String,
    fields: Vector[MComponent.OperationField],
    entities: Vector[MEntity] = Vector.empty
  ): MDomainComponent = {
    val pkg = MPackage("example", MPackageRef.default, Vector.empty)
    val kind = operation.descriptor.kind match {
      case MOperation.Kind.Command => "COMMAND"
      case MOperation.Kind.Query => "QUERY"
    }
    MDomainComponent(Description.name("Example"),
      MObject.Core(MPackageRef("example"), services = List(MService(pkg, "Submission", Seq(operation)))),
      MComponent.Core(entities = entities, operationDefinitions = Vector(
        MComponent.OperationDefinition(name = operation.name, kind = kind, inputType = inputname,
          outputType = "unit", inputValueKind = inputkind, parameters = fields))))
  }

  private def _transform(component: MDomainComponent, declarations: Vector[MObject]): (SComponent, ScalaModel.Context) = {
    val context = ScalaModel.Context.fromObjects(declarations :+ component)
    val transformed = ScalaModel.Context.withContext(context) {
      new ComponentScalaModelTransformer().apply((component, Purpose.Plain)).take
        .collectFirst { case model: SComponent => model }.getOrElse(fail("actual component model missing"))
    }
    (transformed, context)
  }

  private def _generate(component: MDomainComponent, declarations: Vector[MObject]): (SComponent, String) = {
    val (transformed, context) = _transform(component, declarations)
    val source = new Scala3ComponentGenerator(context).generate(transformed).take.slots.map(_.content).mkString("\n")
    (transformed, source)
  }

  private def _wrapper(component: SComponent): SClassBase =
    component.services.head.methods.head.parameters.parameters.head.value.getOrElse(fail("action wrapper missing"))

  private def _carrier(component: SComponent): SClassBase =
    _wrapper(component).parameterSequence.parameters.head.value.getOrElse(fail("actual input value carrier missing"))

  private def _action_declaration(source: String, name: String): String = {
    val marker = s"final case class $name private ("
    val start = source.indexOf(marker)
    if (start < 0) fail(s"actual named command declaration missing: $name")
    val end = source.indexOf(") extends ", start)
    if (end < 0) fail(s"actual named command end missing: $name")
    source.substring(start + marker.length, end)
  }

  private def _action_fields(action: String): Vector[(String, String)] =
    action.linesIterator.map(_.trim.stripSuffix(",")).filter(_.contains(": ")).map { line =>
      val parts = line.split(": ", 2)
      parts(0) -> parts(1)
    }.toVector

  private def _request_line(source: String): String =
    source.linesIterator.find(_.contains("request = RequestDefinition(")).getOrElse(fail("actual request schema missing"))

  private def _request_field(request: String, name: String): String =
    request.split("ParameterDefinition\\(content = ")
      .find(_.startsWith(s"""org.goldenport.value.BaseContent.simple("$name")"""))
      .getOrElse(fail(s"actual request field missing: $name"))

  private def _request_names(request: String): Vector[String] =
    "BaseContent\\.simple\\(\"([^\"]+)\"\\)".r.findAllMatchIn(request).map(_.group(1)).toVector

  private def _vector_type(leaf: TypeName): TypeName =
    TypeName.Container(TypeName.Plain.create("scala.collection.immutable", "Vector"), leaf)
}
