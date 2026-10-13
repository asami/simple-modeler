package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainResource
import org.simplemodeling.SimpleModeler.generator.scala.model.{PackageName, ScalaModel, TypeName}
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.generators.scala.Scala3EntityFamilyGenerator

/*
 * @since   Oct.  5, 2026
 * @version Oct.  5, 2026
 * @author  ASAMI, Tomoharu
 */
final class StructuredDataGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Declared built-in structured data (Phase 78; mapping R1, R3, R4, R6, R12)" should {
    "normalize explicit aliases" which {
      "retain distinct semantic identities after case and whitespace normalization" in {
        Given("the admitted JSON and semi-structured declaration spellings")
        val jsonaliases = Vector("json", "xjson", "  XjSoN  ")
        val recordaliases = Vector("record", "semistructureddata", "semi-structured-data", "xsemistructureddata", "  ReCoRd  ")

        When("the attribute parser selects their semantic types")
        val json = jsonaliases.map(MAttributeType.create)
        val records = recordaliases.map(MAttributeType.create)

        Then("every alias selects its canonical object without collapsing the two types")
        json shouldBe Vector.fill(jsonaliases.size)(MBuiltinStructuredDataType.Json)
        records shouldBe Vector.fill(recordaliases.size)(MBuiltinStructuredDataType.SemiStructuredData)
        MBuiltinStructuredDataType.Json.logicalIdentity should not be MBuiltinStructuredDataType.SemiStructuredData.logicalIdentity
        Vector(MBuiltinStructuredDataType.Json, MBuiltinStructuredDataType.SemiStructuredData).foreach { datatype =>
          datatype.constraints shouldBe empty
          datatype.getAffiliation shouldBe Some(MPackageRef(datatype.runtimePackageName))
          datatype.description should not be Description.empty
        }
      }

      "leave unrelated declarations with the legacy datatype parser" in {
        Given("valid String and Integer declarations outside the explicit alias set")
        val unrelated = Vector("string", "integer")

        When("the structured lookup and attribute parser receive them")
        val selected = unrelated.map(MBuiltinStructuredDataType.get)
        val parsed = unrelated.map(MAttributeType.create)
        val legacy = unrelated.map(MDataType.create)
        val payloadlookup = MBuiltinStructuredDataType.get("{\"payload\":true}")

        Then("unknown structured spellings do not infer meaning and the legacy parser is retained")
        selected shouldBe Vector.fill(unrelated.size)(None)
        parsed shouldBe legacy
        payloadlookup shouldBe None
      }

      "retain legacy rejection for an unrecognized declaration" in {
        Given("an unrecognized spelling outside the explicit alias set")
        val unknown = "custom-proposal"

        When("the structured lookup and both legacy parser entrypoints receive it")
        val selected = MBuiltinStructuredDataType.get(unknown)
        val attributefailure = intercept[org.goldenport.exception.SyntaxErrorFaultException] {
          MAttributeType.create(unknown)
        }
        val datatypefailure = intercept[org.goldenport.exception.SyntaxErrorFaultException] {
          MDataType.create(unknown)
        }

        Then("structured lookup finds no type and both parsers preserve the same rejection")
        selected shouldBe None
        attributefailure.getMessage shouldBe datatypefailure.getMessage
        attributefailure.getMessage should include("Unavailable datatype: custom-proposal")
      }
    }

    "preserve actual entity parameter trees" which {
      "retain structured children through scalar, optional, repeated and nonempty multiplicity" in {
        Given("a semantic resource containing String, Json and Record attributes")
        val entity = _proposal_entity()
        val context = ScalaModel.Context.fromObjects(Vector(entity))
        val transformer = new EntityValueReadScalaModelTransformer()

        When("the read entity is transformed with its declared model context")
        val generated = ScalaModel.Context.withContext(context) {
          transformer((entity, ScalaModelTransformer.Purpose.Read)).take.head
        }
        val types = generated.parameterSequence.parameters.map(x => x.name.name -> x.typeName).toMap

        Then("the actual parameters preserve canonical objects and all container children")
        types("originalProposal") shouldBe TypeName.Primitive.string
        types("payload") shouldBe TypeName.Structured(MBuiltinStructuredDataType.Json)
        types("evidence") shouldBe TypeName.record
        types("optionalPayload") shouldBe TypeName.option(TypeName.create(MBuiltinStructuredDataType.Json))
        types("optionalEvidence") shouldBe TypeName.option(TypeName.record)
        types("payloads") shouldBe _vector_type(TypeName.create(MBuiltinStructuredDataType.Json))
        types("evidences") shouldBe _vector_type(TypeName.record)
        types("requiredPayloads") shouldBe TypeName.Container(
          TypeName.Plain(PackageName("cats.data"), "NonEmptyVector"),
          TypeName.create(MBuiltinStructuredDataType.Json)
        )
        types("requiredEvidences") shouldBe TypeName.Container(
          TypeName.Plain(PackageName("cats.data"), "NonEmptyVector"), TypeName.record
        )
        TypeName.record.fullName shouldBe "org.goldenport.record.Record"
        TypeName.create(MBuiltinStructuredDataType.Json).fullName shouldBe "io.circe.Json"
        TypeName.create(MBuiltinStructuredDataType.Json).isPlatform shouldBe true
        TypeName.record.isRequired shouldBe true
        generated.importNames should contain (TypeName.Plain.create("io.circe", "Json"))
        generated.importNames should contain (TypeName.Plain.create("org.goldenport.record", "Record"))
      }

      "retain nested structured leaves in create, update and query parameters" in {
        Given("the same resource and the existing family transformations")
        val entity = _proposal_entity()
        val context = ScalaModel.Context.fromObjects(Vector(entity))
        val routes = Vector(
          (new EntityValueCreateScalaModelTransformer(), ScalaModelTransformer.Purpose.Create),
          (new EntityValueUpdateScalaModelTransformer(), ScalaModelTransformer.Purpose.Update),
          (new EntityValueQueryScalaModelTransformer(), ScalaModelTransformer.Purpose.Query)
        )

        When("the entity input families are transformed")
        val models = ScalaModel.Context.withContext(context) {
          routes.map { case (transformer, purpose) =>
            transformer((entity, purpose)).take.head
          }
        }

        Then("Update and Condition wrappers keep the structured leaf inside repeated containers")
        models.foreach { model =>
          val types = model.parameterSequence.parameters.map(x => x.name.name -> x.typeName).toMap
          _leaf_type(types("payload")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.Json)
          _leaf_type(types("payloads")) shouldBe TypeName.Structured(MBuiltinStructuredDataType.Json)
          _leaf_type(types("evidence")) shouldBe TypeName.record
          _leaf_type(types("evidences")) shouldBe TypeName.record
          _leaf_type(types("originalProposal")) shouldBe TypeName.Primitive.string
        }
        val updateparams = models(1).parameterSequence.parameters
        updateparams.find(_.name.name == "payloads").get.typeName shouldBe TypeName.Container(
          TypeName.Plain.create("org.simplemodeling.model.directive", "Update"),
          _vector_type(TypeName.create(MBuiltinStructuredDataType.Json))
        )
      }
    }

    "generate the real entity family" which {
      "emit canonical application types, schema symbols and admission readers" in {
        Given("a resource whose structured fields use explicit semantic objects")
        val entity = _proposal_entity()
        val context = ScalaModel.Context.fromObjects(Vector(entity))

        When("Scala3EntityFamilyGenerator emits the actual entity sources")
        val artifacts = new Scala3EntityFamilyGenerator(context).generate(entity).take
        val source = artifacts.slots.map(_.content).mkString("\n")

        Then("String remains String and the two structured logical identities remain distinct")
        source should include("originalProposal: String")
        source should include("payload: Json")
        source should include("evidence: Record")
        source should include("optionalPayload: Option[Json]")
        source should include("optionalEvidence: Option[Record]")
        source should include("payloads: Vector[Json]")
        source should include("evidences: Vector[Record]")
        source should include("requiredPayloads: NonEmptyVector[Json]")
        source should include("requiredEvidences: NonEmptyVector[Record]")
        source should include("datatype = org.goldenport.schema.XString")
        source should include("datatype = org.goldenport.schema.XJson")
        source should include("datatype = org.goldenport.schema.XSemiStructuredData")
        source should include("org.goldenport.schema.Multiplicity.ZeroOne")
        source should include("org.goldenport.schema.Multiplicity.ZeroMore")
        source should include("org.goldenport.schema.Multiplicity.OneMore")
        source should include("summon[org.goldenport.convert.ValueReader[io.circe.Json]].readC")
        source should include("_record_get_vector_as_c[io.circe.Json]")
        source should include("_record_get_vector_of_record_c(record, INPUT_KEYS_EVIDENCES)")
        source should not include "DataType.Named(\"json\")"
        source should not include "DataType.Named(\"record\")"
        source should not include "Json.createC"
        source should not include "Json.fromRecord"
      }

      "keep custom Json and Record class names distinct from canonical structured declarations" in {
        Given("explicit custom classes and the exact legacy platform Record declaration")
        val custom = _entity("CustomProposal", List(
          _attribute("customJson", MObjectAttributeType(MObjectRef.create("org.example.custom.Json")), MOne),
          _attribute("customRecord", MObjectAttributeType(MObjectRef.create("org.example.custom.Record")), MOne)
        ))
        val legacy = _entity("LegacyProposal", List(
          _attribute("evidence", MObjectAttributeType(MObjectRef.record), MOne)
        ))
        val context = ScalaModel.Context.fromObjects(Vector(custom, legacy))

        When("the real transformer and entity generator process these declarations")
        val model = ScalaModel.Context.withContext(context) {
          new EntityValueReadScalaModelTransformer().apply(custom).take.head
        }
        val generator = new Scala3EntityFamilyGenerator(context)
        val customsource = generator.generate(custom).take.slots.map(_.content).mkString("\n")
        val legacysource = generator.generate(legacy).take.slots.map(_.content).mkString("\n")

        Then("custom references remain Plain and only the exact platform Record receives the legacy canonical schema")
        val types = model.parameterSequence.parameters.map(x => x.name.name -> x.typeName).toMap
        types("customJson") shouldBe TypeName.Plain.create("org.example.custom", "Json")
        types("customRecord") shouldBe TypeName.Plain.create("org.example.custom", "Record")
        customsource should include("DataType.Named(\"Json\")")
        customsource should include("DataType.Named(\"Record\")")
        customsource should not include "org.goldenport.schema.XJson"
        customsource should not include "org.goldenport.schema.XSemiStructuredData"
        legacysource should include("org.goldenport.schema.XSemiStructuredData")
        legacysource should not include "DataType.Named(\"record\")"
      }
    }
  }

  private def _vector_type(typename: TypeName): TypeName = {
    TypeName.Container(TypeName.Plain(PackageName("scala.collection.immutable"), "Vector"), typename)
  }

  private def _leaf_type(typename: TypeName): TypeName = {
    typename match {
      case TypeName.Container(_, child) => {
        _leaf_type(child)
      }
      case leaf => {
        leaf
      }
    }
  }

  private def _attribute(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute = {
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)
  }

  private def _proposal_entity(): MDomainResource = {
    _entity("Proposal", List(
      _attribute("originalProposal", MDataType.string, MOne),
      _attribute("payload", MBuiltinStructuredDataType.Json, MOne),
      _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MOne),
      _attribute("optionalPayload", MBuiltinStructuredDataType.Json, MZeroOne),
      _attribute("optionalEvidence", MBuiltinStructuredDataType.SemiStructuredData, MZeroOne),
      _attribute("payloads", MBuiltinStructuredDataType.Json, MZeroMore),
      _attribute("evidences", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore),
      _attribute("requiredPayloads", MBuiltinStructuredDataType.Json, MOneMore),
      _attribute("requiredEvidences", MBuiltinStructuredDataType.SemiStructuredData, MOneMore)
    ))
  }

  private def _entity(name: String, attributes: List[MAttribute]): MDomainResource = {
    MDomainResource(
      description = Description.name(name),
      affiliation = MPackageRef("org.example.proposal"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = attributes,
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )
  }
}
