package org.simplemodeling.SimpleModeler.generator.componentapi

import org.goldenport.record.v2.{DataType, XInt, XString}
import org.goldenport.values.Designation
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainComponent, MDomainValue}

/*
 * @since   Jul. 12, 2026
 * @version Jul. 12, 2026
 * @author  ASAMI, Tomoharu
 */
final class ComponentApiContractMetadataSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "ComponentApiContractMetadata" should {
    "derive provided and required API contracts from the CML model graph" in {
      Given("a provider API with nested values and a powertype plus a required socket set")
      val model = _contract_model("RunRequest")

      When("component API metadata is generated twice")
      val first = ComponentApiContractMetadata.generate(model).right.get
      val second = ComponentApiContractMetadata.generate(model).right.get

      Then("the provided API contains a closed deterministic public contract")
      first.toCanonicalJson shouldBe second.toCanonicalJson
      val provided = first.provided.head
      provided.contract shouldBe "Provider.Runner"
      provided.apiClass shouldBe "example.api.ExampleRunnerApi"
      provided.operations should contain (ComponentApiContractMetadata.Operation(
        "Run",
        "example.value.RunRequest",
        "example.value.RunResponse"
      ))
      provided.publicTypes.map(_.className) should contain allOf (
        "example.api.ExampleRunnerApi",
        "example.api.ExampleRunnerApi$",
        "example.api.ExampleRunnerApi$Proxy",
        "example.api.ExampleRunnerApi$Proxy$",
        "example.api.ExampleRunnerApi$Provider$",
        "example.api.ExampleRunnerApi$Socket",
        "example.api.ExampleRunnerApi$SocketSet",
        "example.value.RunRequest",
        "example.value.RunResponse",
        "example.datatype.NestedValue",
        "example.RunMode"
      )
      provided.publicTypes.find(_.className == "example.datatype.NestedValue").map(_.kind) shouldBe Some("datatype")
      provided.publicTypes.find(_.className == "example.value.RunRequest").map(_.kind) shouldBe Some("value")
      provided.publicTypes.find(_.className == "example.value.RunRequest").toVector.flatMap(_.attributes)
        .find(_.name == "title").map(_.typeName) shouldBe Some("string")
      provided.publicTypes.find(_.className == "example.value.RunRequest").toVector.flatMap(_.artifactPatterns) should contain allOf (
        "example/value/RunRequest.class",
        "example/value/RunRequest$*.class",
        "example/value/RunRequest.tasty"
      )
      provided.abiHash should startWith("sha256:")

      And("the required API preserves socket multiplicity and requiredness")
      first.required should contain (ComponentApiContractMetadata.RequiredApi(
        "Consumer",
        "Runners",
        "example.api.ExampleRunnerApi",
        "*",
        required = true
      ))
    }

    "treat a single required socket as required when the explicit flag is omitted" in {
      Given("a consumer service with the default single socket multiplicity")
      val model = _contract_model("RunRequest", requiredmultiplicity = None, explicitrequired = false)

      When("component API metadata is generated")
      val required = ComponentApiContractMetadata.generate(model).right.get.required.head

      Then("the generated requirement is singular and mandatory")
      required.multiplicity shouldBe "1"
      required.required shouldBe true
    }

    "change the ABI hash when the public operation signature changes" in {
      Given("two provider models with different request types")
      val baseline = _contract_model("RunRequest")
      val changed = _contract_model("NestedValue")

      When("their metadata is generated")
      val baselinehash = ComponentApiContractMetadata.generate(baseline).right.get.provided.head.abiHash
      val changedhash = ComponentApiContractMetadata.generate(changed).right.get.provided.head.abiHash

      Then("their ABI hashes differ")
      changedhash should not be baselinehash
    }

    "change the ABI hash when a public value structure changes" in {
      Given("two provider models with the same operation types but different request fields")
      val baseline = _contract_model("RunRequest", requestfieldname = "nested")
      val changed = _contract_model("RunRequest", requestfieldname = "renamedNested")

      When("their metadata is generated")
      val baselinehash = ComponentApiContractMetadata.generate(baseline).right.get.provided.head.abiHash
      val changedhash = ComponentApiContractMetadata.generate(changed).right.get.provided.head.abiHash

      Then("their ABI hashes differ")
      changedhash should not be baselinehash
    }

    "change the ABI hash when a scalar field type changes behind a CML datatype name" in {
      Given("two provider models whose request field keeps its name but changes scalar type")
      val baseline = _contract_model("RunRequest", requestscalartype = XString)
      val changed = _contract_model("RunRequest", requestscalartype = XInt)

      When("their metadata is generated")
      val baselinehash = ComponentApiContractMetadata.generate(baseline).right.get.provided.head.abiHash
      val changedhash = ComponentApiContractMetadata.generate(changed).right.get.provided.head.abiHash

      Then("their ABI hashes differ")
      changedhash should not be baselinehash
    }

    "reject implementation and persistence types at the public API boundary" in {
      Given("a provider operation exposing an implementation package")
      val model = _contract_model("example.impl.SecretRequest")

      When("component API metadata is generated")
      val result = ComponentApiContractMetadata.generate(model)

      Then("generation fails before a descriptor is published")
      result.isLeft shouldBe true
      result.left.get should include("forbidden implementation type")
    }

    "reject forbidden types even when they use an otherwise external package prefix" in {
      Given("a provider operation exposing an implementation type under org.goldenport")
      val model = _contract_model("org.goldenport.impl.SecretRequest")

      When("component API metadata is generated")
      val result = ComponentApiContractMetadata.generate(model)

      Then("the external prefix does not bypass the implementation boundary")
      result.left.get should include("forbidden implementation type")
    }

    "reject unresolved qualified public types" in {
      Given("a provider operation exposing a qualified type absent from the model")
      val model = _contract_model("org.example.missing.Request")

      When("component API metadata is generated")
      val result = ComponentApiContractMetadata.generate(model)

      Then("generation fails instead of publishing an incomplete closure")
      result.left.get should include("not resolvable from the CML model")
    }
  }

  private def _contract_model(
    requesttype: String,
    requiredmultiplicity: Option[String] = Some("*"),
    explicitrequired: Boolean = true,
    requestfieldname: String = "nested",
    requestscalartype: DataType = XString
  ): SimpleModel = {
    val packageref = MPackageRef("example")
    val pkg = MPackage("example", MPackageRef.default, Vector.empty)
    val nested = _datatype(packageref, "NestedValue")
    val request = _value(packageref, "RunRequest", List(
      _attribute(requestfieldname, "NestedValue"),
      _scalar_attribute("title", "Title", requestscalartype)
    ))
    val response = _value(packageref, "RunResponse")
    val mode = MPowertype(Description.name("RunMode"), packageref, Nil)
    val operation = MOperation.command(
      "Run",
      List(MParameter("request", request)),
      MResult(response)
    )
    val service = MService(pkg, "Runner", Seq(operation))
    val provider = MDomainComponent(
      Description.name("Provider"),
      MObject.Core(packageref, services = List(service)),
      MComponent.Core(
        entities = Vector.empty,
        operationDefinitions = Vector(_operation_definition(requesttype)),
        componentDefinitions = Vector(MComponent.ComponentDefinition(
          "Provider",
          services = Vector(MComponent.ComponentServiceDefinition(
            "Runner",
            spiDirection = "provides",
            spiSocket = true,
            spiApiName = Some("ExampleRunner")
          ))
        ))
      )
    )
    val requestwithpowertype = request.copy(powertypes = List(MPowertypeRef(packageref, "RunMode")))
    val consumer = MDomainComponent(
      Description.name("Consumer"),
      MObject.Core(packageref),
      MComponent.Core(
        entities = Vector.empty,
        componentDefinitions = Vector(MComponent.ComponentDefinition(
          "Consumer",
          services = Vector(MComponent.ComponentServiceDefinition(
            "Runners",
            spiDirection = "requires",
            spiMultiplicity = requiredmultiplicity,
            spiRequired = explicitrequired,
            spiComponentApi = Some("example.api.ExampleRunnerApi")
          ))
        ))
      )
    )
    SimpleModel(Vector(provider, consumer, nested, requestwithpowertype, response, mode))
  }

  private def _operation_definition(requesttype: String): MComponent.OperationDefinition =
    MComponent.OperationDefinition(
      name = "Run",
      kind = "COMMAND",
      inputType = requesttype,
      outputType = "RunResponse",
      inputValueKind = "command"
    )

  private def _value(
    pkg: MPackageRef,
    name: String,
    attributes: List[MAttribute] = Nil
  ): MDomainValue =
    MDomainValue(
      Description.name(name),
      pkg,
      Nil,
      None,
      Nil,
      Nil,
      attributes,
      Nil
    )

  private def _datatype(
    pkg: MPackageRef,
    name: String,
    attributes: List[MAttribute] = Nil
  ): MStructuredDataType =
    MStructuredDataType(
      Description.name(name),
      pkg,
      Nil,
      None,
      Nil,
      Nil,
      attributes,
      Nil
    )

  private def _attribute(name: String, typename: String): MAttribute =
    MAttribute(
      Designation(name),
      MDataType(Designation(typename), XString, MPackageRef.default),
      MOne,
      Nil,
      None
    )

  private def _scalar_attribute(name: String, typename: String, datatype: DataType): MAttribute =
    MAttribute(
      Designation(name),
      MDataType(Designation(typename), datatype, MPackageRef.default),
      MOne,
      Nil,
      None
    )
}
