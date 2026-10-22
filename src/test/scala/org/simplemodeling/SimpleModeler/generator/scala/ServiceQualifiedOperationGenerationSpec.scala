package org.simplemodeling.SimpleModeler.generator.scala

import org.goldenport.values.Designation
import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.rng.Seed
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainComponent, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.componentapi.ComponentApiContractMetadata
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.{ComponentApiSourceGenerator, Scala3ComponentGenerator}
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since   Oct. 21, 2026
 * @version Oct. 21, 2026
 */
final class ServiceQualifiedOperationGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Service-qualified operation generation" should {
    "retain the actual modeled owner" which {
      "carry both metadata and method ownership through the actual component transformer" in {
        Given("BookMaterial and KnowledgeMaterial both declare readOriginal with distinct contracts")
        val (component, values) = _fixture()

        When("the normal MComponent to SComponent transformation interprets the component")
        val (transformed, _) = _transform(component, values)

        Then("each metadata definition and every transformed method retains its real service")
        transformed.operationDefinitions.map(_.serviceName) shouldBe Vector(Some("BookMaterial"), Some("KnowledgeMaterial"))
        transformed.services.foreach { service =>
          service.methods.map(_.serviceName) shouldBe Vector(Some(service.serviceName))
          service.methods.map(_.name.name) shouldBe Vector("readOriginal")
        }
        transformed.operationDefinitions.map(_.inputType) shouldBe Vector("BookMaterialReadOriginalQuery", "OriginalSelectionQuery")
      }

      "emit separate request response action and factory contracts for the two services" in {
        Given("legacy BookMaterial resourceId/originalId and logical KnowledgeMaterial materialRef/resourceId inputs")
        val (component, values) = _fixture()

        When("normal Scala 3 component generation emits both service blocks")
        val source = _generate(component, values)
        val book = _block(source, "object BookMaterialService extends ServiceDefinition")
        val knowledge = _block(source, "object KnowledgeMaterialService extends ServiceDefinition")

        Then("each service selects only its own ordered required request fields and response type")
        _request_names(book) shouldBe Vector("resourceId", "originalId")
        _request_names(knowledge) shouldBe Vector("materialRef", "resourceId")
        _require_fields(book, Vector("resourceId", "originalId"))
        _require_fields(knowledge, Vector("materialRef", "resourceId"))
        book should include("DataType.Named(\"BookOriginalResult\")")
        knowledge should include("DataType.Named(\"KnowledgeOriginalResult\")")
        book should include("final case class BookMaterialReadOriginalQuery private (")
        knowledge should include("final case class OriginalSelectionQuery private (")
        book should include("BookMaterialReadOriginalQuery.create(req)")
        knowledge should include("OriginalSelectionQuery.create(req)")
        book should not include "materialRef"
        book should not include "OriginalSelectionQuery"
        knowledge should not include "originalId"
        knowledge should not include "BookMaterialReadOriginalQuery"
        _block(source, "class BookMaterialServiceFactory()") should include("action: BookMaterialReadOriginalQuery")
        _block(source, "class KnowledgeMaterialServiceFactory()") should include("action: OriginalSelectionQuery")
      }

      "supply the actual service owner even for directly authored ownerless SMethods" in {
        Given("an SComponent whose methods have no explicit owner but whose metadata is service-qualified")
        val (component, values) = _fixture()
        val (transformed, context) = _transform(component, values)
        val raw = transformed.copy(componentCore = transformed.componentCore.copy(services = transformed.services.map { service =>
          service.copy(core = service.core.copy(methodCompartment = MethodCompartment(service.methods.map(_.copy(serviceName = None)))))
        }))

        When("the normal generator traverses each actual SService")
        val source = new Scala3ComponentGenerator(context).generate(raw).take.slots.map(_.content).mkString("\n")

        Then("service traversal supplies ownership before metadata selection")
        _request_names(_block(source, "object BookMaterialService extends ServiceDefinition")) shouldBe Vector("resourceId", "originalId")
        _request_names(_block(source, "object KnowledgeMaterialService extends ServiceDefinition")) shouldBe Vector("materialRef", "resourceId")
      }

      "preserve ownership for independently generated names and reversed service and metadata orders" in {
        Given("independent safe owner and schema names with distinct prefixes and both declaration orders")
        val names = Gen.listOfN(7, Gen.alphaLowerChar).map(_.mkString)
        val property = Prop.forAll(names, names, names, names) { (first, second, inputfirst, inputsecond) =>
          Given("two independently named services with distinct prefixes and selected schemas")
          val owners = Vector("Book" + first, "Knowledge" + second)
          val inputs = Vector("Book" + inputfirst + "Query", "Knowledge" + inputsecond + "Query")
          val (component, values) = _fixture(owners, inputs)
          val reversed = component.copy(objectCore = component.objectCore.copy(services = component.services.reverse),
            componentCore = component.componentCore.copy(operationDefinitions = component.operationDefinitions.reverse))

          When("normal generation interprets the original and reversed declarations")
          val sources = Vector(component, reversed).map(_generate(_, values))

          Then("every sampled service retains its own request fields action schema and output")
          sources.foreach { source =>
            owners.zipWithIndex.foreach { case (owner, index) =>
              val block = _block(source, s"object ${owner}Service extends ServiceDefinition")
              _request_names(block) shouldBe _fields(index)
              block should include(s"final case class ${inputs(index)} private (")
              block should include(s"""DataType.Named("${_outputs(index)}")""")
              block should not include inputs(1 - index)
            }
          }
          true
        }

        When("ScalaCheck evaluates the deterministic property without implication filters")
        val result = Test.check(Test.Parameters.default.withInitialSeed(Seed(20261021L)).withMinSuccessfulTests(30), property)

        Then("at least thirty successful samples and zero discards document ordering independence")
        withClue(result.toString) { result.passed shouldBe true }
        result.succeeded should be >= 30
        result.discarded shouldBe 0
      }
    }

    "isolate command controls and compatibility" which {
      "select each same-name command or query kind implementation and access policy" in {
        Given("a COMMAND with echo or sync metadata and a QUERY with its own access policy")
        val (component, values) = _fixture(kinds = Vector("COMMAND", "QUERY"))
        val models = Vector(
          component.copy(componentCore = component.componentCore.copy(operationDefinitions = component.operationDefinitions.updated(0,
            component.operationDefinitions.head.copy(implementation = Some("echo-record"))))),
          component.copy(componentCore = component.componentCore.copy(operationDefinitions = component.operationDefinitions.updated(0,
            component.operationDefinitions.head.copy(execution = Some("sync"))))))

        When("the actual generator emits both services for each command implementation control")
        val sources = models.map(_generate(_, values))
        val definitions = models.map(model => _transform(model, values)._1.operationDefinitions)

        Then("only the command selects echo execution and each service retains its generated kind and modeled access")
        sources.foreach { source =>
          val book = _block(source, "object BookMaterialService extends ServiceDefinition")
          val knowledge = _block(source, "object KnowledgeMaterialService extends ServiceDefinition")
          book should include("extends CommandAction")
          knowledge should include("extends QueryAction")
          book should include("ExecUowM.pure(OperationResponse.create(action.request.toRecord))")
          knowledge should include("uowmNotImplemented")
          knowledge should not include "ExecUowM.pure(OperationResponse.create(action.request.toRecord))"
        }
        definitions.foreach { operations =>
          _owners.zipWithIndex.foreach { case (owner, index) =>
            val definition = operations.find(_.serviceName.contains(owner))
              .getOrElse(fail(s"transformed operation metadata missing: $owner"))
            definition.access.map(_.policy) shouldBe Some(if (index == 0) "manager_only" else "owner_only")
            definition.serviceName shouldBe Some(owner)
          }
        }
      }

      "never borrow another owner's echo or sync control for an unimplemented command" in {
        Given("two COMMANDs where only KnowledgeMaterial declares an echo or sync implementation")
        val (component, values) = _fixture(kinds = Vector("COMMAND", "COMMAND"))
        val models = Vector(Some("echo-record") -> None, None -> Some("sync")).map { case (implementation, execution) =>
          component.copy(componentCore = component.componentCore.copy(operationDefinitions = component.operationDefinitions.updated(1,
            component.operationDefinitions(1).copy(implementation = implementation, execution = execution))))
        }

        When("the real component generator selects the two command programs")
        val sources = models.map(_generate(_, values))

        Then("the first service stays unimplemented while only the second echoes its request")
        sources.foreach { source =>
          val book = _block(source, "object BookMaterialService extends ServiceDefinition")
          val knowledge = _block(source, "object KnowledgeMaterialService extends ServiceDefinition")
          book should include("uowmNotImplemented")
          book should not include "ExecUowM.pure(OperationResponse.create(action.request.toRecord))"
          knowledge should include("ExecUowM.pure(OperationResponse.create(action.request.toRecord))")
        }
      }

      "support unique ownerless legacy metadata and keep method fallback when matching metadata is absent" in {
        Given("one legacy ownerless definition, no definition, or a definition owned only by the other service")
        val (component, values) = _fixture()
        val single = component.copy(objectCore = component.objectCore.copy(services = component.services.take(1)))
        val legacy = single.copy(componentCore = single.componentCore.copy(operationDefinitions = Vector(component.operationDefinitions.head.copy(serviceName = None))))
        val absent = single.copy(componentCore = single.componentCore.copy(operationDefinitions = Vector.empty))
        val foreign = single.copy(componentCore = single.componentCore.copy(operationDefinitions = Vector(component.operationDefinitions(1))))

        When("normal generation processes each compatibility shape")
        val sources = Vector(legacy, absent, foreign).map(_generate(_, values))

        Then("legacy metadata still selects its action while absence and foreign metadata retain the method fallback")
        _request_names(_block(sources.head, "object BookMaterialService extends ServiceDefinition")) shouldBe _fields.head
        sources.head should include("final case class BookMaterialReadOriginalQuery private (")
        sources.tail.foreach { source =>
          val book = _block(source, "object BookMaterialService extends ServiceDefinition")
          book should include("final case class ReadOriginalQuery private (")
          book should not include "OriginalSelectionQuery"
          book should not include "materialRef"
          book should include("DataType.Named(\"Record\")")
        }
      }

      "prefer the exact owner over legacy metadata and reject ownerless or qualified ambiguity" in {
        Given("an exact owner beside a legacy fallback, and separately duplicate legacy or qualified definitions")
        val (component, values) = _fixture()
        val own = component.operationDefinitions.head
        val legacy = component.operationDefinitions(1).copy(serviceName = None)
        val preferred = component.copy(componentCore = component.componentCore.copy(operationDefinitions = Vector(legacy, own)))
        val ambiguous = Vector(Vector(legacy, legacy), Vector(own, own)).map { definitions =>
          component.copy(componentCore = component.componentCore.copy(operationDefinitions = definitions))
        }

        When("normal generation resolves the preferred definition and the ambiguous models")
        val source = _generate(preferred, values)
        val failures = ambiguous.map(model => intercept[Exception] { _generate(model, values) })

        Then("the exact owner wins and ambiguity identifies both operation and service")
        _request_names(_block(source, "object BookMaterialService extends ServiceDefinition")) shouldBe _fields.head
        failures.foreach { failure =>
          failure.getMessage.toLowerCase(java.util.Locale.ROOT) should include("ambiguous")
          failure.getMessage should include("readOriginal")
          failure.getMessage should include("BookMaterial")
        }
      }
    }

    "publish only the selected service's API contract" which {
      "keep source signatures proxy conversions and public type closures local to their owner" in {
        Given("two provided services sharing readOriginal with distinct request and response value graphs")
        val (component, values) = _fixture()
        val (transformed, _) = _transform(component, values)

        When("normal API source and contract metadata generation interpret the same model graph")
        val sources = ComponentApiSourceGenerator.generate(transformed).slots
        val document = ComponentApiContractMetadata.generate(SimpleModel(values :+ component)).right.get

        Then("each API includes its own signature proxy conversion and complete value closure only")
        _owners.zipWithIndex.foreach { case (owner, index) =>
          val source = sources.find(_.path.endsWith(s"/${owner}Api.scala")).get.content
          source should include(s"def readOriginal(request: example.value.${_inputs(index)})")
          source should include(s"Consequence[example.value.${_outputs(index)}]")
          source should include(s"example.value.${_outputs(index)}.createC(record)")
          source should include(s"""SpiOperationSelector("readOriginal", Some("$owner"))""")
          source should not include _inputs(1 - index)
          source should not include _outputs(1 - index)
          val api = document.provided.find(_.service == owner).get
          api.operations shouldBe Vector(ComponentApiContractMetadata.Operation("readOriginal",
            s"example.value.${_inputs(index)}", s"example.value.${_outputs(index)}"))
          api.publicTypes.map(_.className) should contain allOf (s"example.value.${_inputs(index)}", s"example.value.${_outputs(index)}")
          api.publicTypes.map(_.className) should not contain s"example.value.${_inputs(1 - index)}"
          api.publicTypes.map(_.className) should not contain s"example.value.${_outputs(1 - index)}"
        }
      }

      "retain legacy API compatibility and fail ambiguous API source and metadata generation" in {
        Given("one provided service with unique legacy metadata or duplicate selected metadata")
        val (component, values) = _fixture()
        val own = component.operationDefinitions.head
        val single = component.copy(objectCore = component.objectCore.copy(services = component.services.take(1)),
          componentCore = component.componentCore.copy(componentDefinitions = component.componentDefinitions.map(definition =>
            definition.copy(services = definition.services.take(1)))))
        val legacy = single.copy(componentCore = single.componentCore.copy(operationDefinitions = Vector(own.copy(serviceName = None))))
        val ambiguous = Vector(Vector(own, own), Vector(own.copy(serviceName = None), own.copy(serviceName = None)))
          .map(definitions => single.copy(componentCore = single.componentCore.copy(operationDefinitions = definitions)))

        When("both native API consumers select metadata for each contract")
        val (transformed, _) = _transform(legacy, values)
        val source = ComponentApiSourceGenerator.generate(transformed).slots.head.content
        val document = ComponentApiContractMetadata.generate(SimpleModel(values :+ legacy)).right.get
        val sourcefailures = ambiguous.map { model =>
          val (selected, _) = _transform(model, values)
          intercept[Exception] { ComponentApiSourceGenerator.generate(selected) }
        }
        val metadatafailures = ambiguous.map { model =>
          intercept[Exception] { ComponentApiContractMetadata.generate(SimpleModel(values :+ model)) }
        }

        Then("the unique legacy signature survives and neither consumer publishes ambiguous contracts")
        source should include("request: example.value.BookMaterialReadOriginalQuery")
        document.provided.head.operations.head.requestType shouldBe "example.value.BookMaterialReadOriginalQuery"
        (sourcefailures ++ metadatafailures).foreach { failure =>
          failure.getMessage.toLowerCase(java.util.Locale.ROOT) should include("ambiguous")
          failure.getMessage should include("readOriginal")
          failure.getMessage should include("BookMaterial")
        }
      }
    }
  }

  private val _owners = Vector("BookMaterial", "KnowledgeMaterial")
  private val _inputs = Vector("BookMaterialReadOriginalQuery", "OriginalSelectionQuery")
  private val _outputs = Vector("BookOriginalResult", "KnowledgeOriginalResult")
  private val _fields = Vector(Vector("resourceId", "originalId"), Vector("materialRef", "resourceId"))

  private def _value(name: String, fields: Vector[String]): MDomainValue =
    MDomainValue(Description.name(name), MPackageRef("example"), Nil, None, Nil, Nil,
      fields.map(field => MAttribute(Designation(field), MDataType.string, MOne, Nil, None)).toList, Nil)

  private def _fixture(
    owners: Vector[String] = _owners,
    inputs: Vector[String] = _inputs,
    kinds: Vector[String] = Vector("QUERY", "QUERY")
  ): (MDomainComponent, Vector[MDomainValue]) = {
    val pkg = MPackage("example", MPackageRef.default, Vector.empty)
    val values = inputs.zipWithIndex.map { case (name, index) => _value(name, _fields(index)) } ++
      _outputs.map(name => _value(name, Vector("content")))
    val services = owners.zipWithIndex.map { case (owner, index) =>
      val operation = if (kinds(index) == "COMMAND")
        MOperation.command("readOriginal", List(MParameter.record), MResult(MObjectRef.record))
      else MOperation.query("readOriginal", List(MParameter.record), MResult(MObjectRef.record))
      MService(pkg, owner, Seq(operation))
    }
    val definitions = owners.zipWithIndex.map { case (owner, index) =>
      MComponent.OperationDefinition("readOriginal", kinds(index), inputType = inputs(index), outputType = _outputs(index),
        inputValueKind = (if (kinds(index) == "COMMAND") "COMMAND_VALUE" else "QUERY_VALUE"),
        access = Some(MComponent.OperationAccess(policy = (if (index == 0) "manager_only" else "owner_only"))),
        parameters = _fields(index).map(name => MComponent.OperationField(name, "string", required = Some(true))),
        resultFields = Vector(MComponent.OperationField("content", "string")), serviceName = Some(owner))
    }
    val component = MDomainComponent(Description.name("Example"), MObject.Core(MPackageRef("example"), services = services.toList),
      MComponent.Core(entities = Vector.empty, operationDefinitions = definitions,
        componentDefinitions = Vector(MComponent.ComponentDefinition("Example", services = owners.map(owner =>
          MComponent.ComponentServiceDefinition(owner, spiDirection = "provides", spiSocket = true))))))
    (component, values)
  }

  private def _transform(component: MDomainComponent, values: Vector[MDomainValue]): (SComponent, ScalaModel.Context) = {
    val context = ScalaModel.Context.fromObjects(values :+ component)
    val transformed = ScalaModel.Context.withContext(context) {
      new ComponentScalaModelTransformer().apply((component, Purpose.Plain)).take
        .collectFirst { case selected: SComponent => selected }.getOrElse(fail("component model missing"))
    }
    (transformed, context)
  }

  private def _generate(component: MDomainComponent, values: Vector[MDomainValue]): String = {
    val (transformed, context) = _transform(component, values)
    new Scala3ComponentGenerator(context).generate(transformed).take.slots.map(_.content).mkString("\n")
  }

  private def _block(source: String, marker: String): String = {
    val start = source.indexOf(marker)
    if (start < 0) fail(s"generated declaration missing: $marker")
    val opening = source.indexOf('{', start)
    var depth = 1
    var index = opening + 1
    while (index < source.length && depth > 0) {
      source.charAt(index) match {
        case '{' => depth += 1
        case '}' => depth -= 1
        case _ => ()
      }
      index += 1
    }
    if (depth != 0) fail(s"generated declaration not closed: $marker")
    source.substring(start, index)
  }

  private def _request_names(block: String): Vector[String] = {
    val request = block.linesIterator.find(_.contains("request = RequestDefinition(")).getOrElse(fail("request schema missing"))
    "BaseContent\\.simple\\(\"([^\"]+)\"\\)".r.findAllMatchIn(request).map(_.group(1)).toVector
  }

  private def _require_fields(block: String, names: Vector[String]): Unit = {
    val request = block.linesIterator.find(_.contains("request = RequestDefinition(")).getOrElse(fail("request schema missing"))
    names.foreach { name =>
      val field = request.split("ParameterDefinition\\(content = ").find(_.startsWith(s"""org.goldenport.value.BaseContent.simple("$name")"""))
        .getOrElse(fail(s"request field missing: $name"))
      field should include("multiplicity = org.goldenport.schema.Multiplicity.One")
      field should include("required = Some(true)")
    }
  }
}
