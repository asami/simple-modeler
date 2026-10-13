package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.goldenport.record.v2.{Column, SqlColumn, XString}
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.{Scala3CaseClassGenerator, Scala3EntityFamilyGenerator, Scala3ValueFamilyGenerator}
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.{EntityValueReadScalaModelTransformer, EntityValueCreateScalaModelTransformer, EntityValueUpdateScalaModelTransformer}

/*
 * @since   Oct.  5, 2026
 * @version Oct.  5, 2026
 * @author  ASAMI, Tomoharu
 */
final class EntityStoreMappingGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "E1 Declared logical identities and storage aliases" must afterWord(
    "in spec:entity-store-mapping-generation, example:E1, rules:R1,R2, phase:78"
  ) {
    "emit separate bindings for actual String, Json and Record family fields" in {
      Given("docs/spec/entity-store-mapping-generation.md R1,R2 E1: semantic fields with an explicit SQL column alias")
      val original = _attribute("originalProposal", MDataType.string, MOne).copy(column = Some(
        Column("originalProposal", datatype = XString, multiplicity = org.goldenport.record.v2.MOne,
          sql = SqlColumn(name = "proposal_text", datatype = None))
      ))
      val entity = _entity(List(original,
        _attribute("payload", MBuiltinStructuredDataType.Json, MOne),
        _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MOne)))

      When("the declared entity's real Scala family is generated")
      val products = _family(entity)

      Then("each persistence family declares exact logical/storage names and distinct canonical bindings")
      _persistent_products(products).foreach { source =>
        _field(source, "originalProposal") should include("\"proposal_text\", org.goldenport.schema.ValueDomain(org.goldenport.schema.XString")
        _field(source, "originalProposal") should include("EntityStoreFieldBinding.string")
        _field(source, "payload") should include("EntityStoreFieldBinding.json")
        _field(source, "evidence") should include("EntityStoreFieldBinding.semiStructuredData")
        source should include("override def storeRowMapping")
      }
      _source(products, "/entity/query/Proposal.scala") should not include "_store_row_mapping"
    }
  }

  "E2 Original optional declarations survive update unwrapping" must afterWord(
    "in spec:entity-store-mapping-generation, example:E2, rules:R1,R3, phase:78"
  ) {
    "retain patch nullability while create defaults use their generated optional type" in {
      Given("docs/spec/entity-store-mapping-generation.md R1,R3 E2: required/optional semantic fields and a required auto-complement name")
      val entity = _entity(List(
        _attribute("name", MDataType.string, MOne),
        _attribute("requiredPayload", MBuiltinStructuredDataType.Json, MOne),
        _attribute("optionalPayload", MBuiltinStructuredDataType.Json, MZeroOne)))
      val context = ScalaModel.Context.fromObjects(Vector(entity))

      When("actual Update/Create transformations and family generation consume the same declarations")
      val update = ScalaModel.Context.withContext(context) {
        new EntityValueUpdateScalaModelTransformer().apply(entity).take.head
      }
      val create = ScalaModel.Context.withContext(context) {
        new EntityValueCreateScalaModelTransformer().apply(entity).take.head
      }
      val products = _family(entity)

      Then("the unwrapped Update retains original optionality independently of its constructor type")
      val updateattrs = update.attributeSequence.attributes.map(x => x.name.name -> x).toMap
      updateattrs("optionalPayload").typeName shouldBe TypeName.Container(
        TypeName.Plain.create("org.simplemodeling.model.directive", "Update"), TypeName.create(MBuiltinStructuredDataType.Json))
      updateattrs("optionalPayload").storeDeclaration shouldBe Some(EntityStoreDeclaration(
        TypeName.option(TypeName.create(MBuiltinStructuredDataType.Json)), MZeroOne))
      updateattrs("requiredPayload").storeDeclaration shouldBe Some(EntityStoreDeclaration(TypeName.create(MBuiltinStructuredDataType.Json), MOne))
      val updatesource = _source(products, "/entity/update/Proposal.scala")
      _field(updatesource, "optionalPayload") should include("Multiplicity.ZeroOne")
      _field(updatesource, "requiredPayload") should include("Multiplicity.One")
      _logical_record(updatesource) should include("Field.Value.Single(optionalPayload)")
      _logical_record(updatesource) should not include "toChangesRecord"
      val name = create.attributeSequence.attributes.find(_.name.name == "name").get
      name.typeName shouldBe TypeName.option(TypeName.Primitive.string)
      name.storeDeclaration shouldBe Some(EntityStoreDeclaration(TypeName.Primitive.string, MOne))
      _field(_source(products, "/entity/create/Proposal.scala"), "name") should include("Multiplicity.ZeroOne")
      _field(_source(products, "/entity/Proposal.scala"), "name") should include("Multiplicity.One")
    }
  }

  "E3 Complete typed rows preserve optional and ordered structured values" must afterWord(
    "in spec:entity-store-mapping-generation, example:E3, rules:R2,R3, phase:78"
  ) {
    "emit recursive bindings and direct field rows without omitting empty/null-capable values" in {
      Given("docs/spec/entity-store-mapping-generation.md R2,R3 E3: semantic optional Json/Record and zero/nonempty repeated fields")
      val entity = _entity(List(
        _attribute("optionalPayload", MBuiltinStructuredDataType.Json, MZeroOne),
        _attribute("optionalEvidence", MBuiltinStructuredDataType.SemiStructuredData, MZeroOne),
        _attribute("payloads", MBuiltinStructuredDataType.Json, MZeroMore),
        _attribute("evidences", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore),
        _attribute("requiredPayloads", MBuiltinStructuredDataType.Json, MOneMore)))

      When("the actual persistence families publish their complete logical record and declarations")
      val products = _family(entity)

      Then("optional markers, Json.Null, empty Records and empty arrays stay in direct typed Field.Single rows")
      _persistent_products(products).foreach { source =>
        val logical = _logical_record(source)
        Vector("optionalPayload", "optionalEvidence", "payloads", "evidences", "requiredPayloads").foreach { field =>
          logical should include(s""""$field", org.goldenport.record.Field.Value.Single($field)""")
        }
        logical should include("Record(Vector(")
        Vector("dataAuto", "_to_external_value", "_to_data_store_value", "toDataStore", "toRecord", "toChangesRecord").foreach { conversion =>
          logical should not include conversion
        }
        _field(source, "optionalPayload") should include("Multiplicity.ZeroOne")
        _field(source, "optionalEvidence") should include("EntityStoreFieldBinding.semiStructuredData")
        _field(source, "payloads") should include("EntityStoreFieldBinding.array(EntityStoreFieldBinding.json)")
        _field(source, "evidences") should include("EntityStoreFieldBinding.array(EntityStoreFieldBinding.semiStructuredData)")
        _field(source, "requiredPayloads") should include("Multiplicity.OneMore")
        source should include("Consequence.success(e._logical_store_record)")
      }
      val persistent = _source(products, "/entity/Proposal.scala")
      persistent should include("admitLogicalStoreRecord(r: Record): Consequence[Record] = { Consequence.success(r) }")
      persistent should include("decodeAdmittedLogicalStoreRecord(r: Record): Consequence[Proposal] = { createC(r) }")
      persistent should include("_record_get_vector_of_record_c(record, INPUT_KEYS_EVIDENCES)")
    }
  }

  "E4 Native scalars and application classes capture explicit codecs" must afterWord(
    "in spec:entity-store-mapping-generation, example:E4, rules:R2,R3, phase:78"
  ) {
    "generate actual class-array bindings and typed composite admission without simple-name inference" in {
      Given("docs/spec/entity-store-mapping-generation.md R2,R3 E4: native fields, declared Summary and custom Json/Record classes")
      val summary = _summary
      val entity = _entity(List(
        _attribute("name", MAttributeType.create("name"), MOne),
        _attribute("quantity", MAttributeType.create("int"), MOne),
        _attribute("active", MAttributeType.create("boolean"), MOne),
        _attribute("amount", MAttributeType.create("decimal"), MOne),
        _attribute("summary", MObjectAttributeType(MObjectRef.create("org.example.proposal.value.Summary")), MOne),
        _attribute("optionalSummary", MObjectAttributeType(MObjectRef.create("org.example.proposal.value.Summary")), MZeroOne),
        _attribute("summaries", MObjectAttributeType(MObjectRef.create("org.example.proposal.value.Summary")), MZeroMore),
        _attribute("customJson", MObjectAttributeType(MObjectRef.create("org.example.custom.Json")), MOne),
        _attribute("customRecord", MObjectAttributeType(MObjectRef.create("org.example.custom.Record")), MOne)))
      val context = ScalaModel.Context.fromObjects(Vector(entity, summary))

      When("the real entity and value generators consume their declared semantic context")
      val products = _family(entity, Vector(summary))
      val summarysource = new Scala3ValueFamilyGenerator(context).generate(summary).take.slots.map(_.content).mkString("\n")

      Then("native representations and recursive application bindings capture ClassTag, Encoder and Decoder")
      _persistent_products(products).foreach { source =>
        _field(source, "name") should include("nativeScalar[org.goldenport.datatype.Name]")
        _field(source, "name") should include("EntityStoreScalarRepresentation.Text")
        _field(source, "quantity") should include("nativeScalar[Int](org.goldenport.schema.XInt, EntityStoreScalarRepresentation.Number")
        _field(source, "active") should include("nativeScalar[Boolean](org.goldenport.schema.XBoolean, EntityStoreScalarRepresentation.Boolean")
        _field(source, "amount") should include("nativeScalar[BigDecimal](org.goldenport.schema.XDecimal, EntityStoreScalarRepresentation.Number")
        _field(source, "summary") should include("DataType.Named(\"org.example.proposal.value.Summary\")")
        _field(source, "summary") should include("application[org.example.proposal.value.Summary]")
        _field(source, "summaries") should include("array(EntityStoreFieldBinding.application[org.example.proposal.value.Summary]")
        Vector("ClassTag", "io.circe.Encoder", "io.circe.Decoder").foreach { capture =>
          _field(source, "summary") should include(capture + "[org.example.proposal.value.Summary]")
        }
        _field(source, "customJson") should include("application[org.example.custom.Json]")
        _field(source, "customRecord") should include("application[org.example.custom.Record]")
        _field(source, "customJson") should not include "EntityStoreFieldBinding.json"
        _field(source, "customRecord") should not include "EntityStoreFieldBinding.semiStructuredData"
      }
      val persistent = _source(products, "/entity/Proposal.scala")
      persistent should include("_record_get_as_c[org.example.proposal.value.Summary]")
      persistent should include("_record_get_vector_as_c[org.example.proposal.value.Summary]")
      summarysource should include("case m: Summary => { Consequence.success(m) }")
      summarysource should include("case m: Record => createC(m)")
    }
  }

  "E5 Framework physical ownership remains selected and explicit" must afterWord(
    "in spec:entity-store-mapping-generation, example:E5, rules:R4, phase:78"
  ) {
    "whitelist only actual flattened SimpleEntity keys while declaring ordinary custom fields" in {
      Given("docs/spec/entity-store-mapping-generation.md R4 E5: a real semantic SimpleEntity resource with an ordinary payload")
      val entity = _entity(List(_attribute("payload", MBuiltinStructuredDataType.Json, MOne)))
        .copy(base = Some(MObjectRef.create("org.simplemodeling.model.SimpleEntity")))

      When("the real SimpleEntity family generates its selected framework rows")
      val products = _family(entity)

      Then("framework fields use the existing physical owner and ordinary fields have explicit typed declarations")
      _persistent_products(products).foreach { source =>
        _field(source, "payload") should include("EntityStoreFieldBinding.json")
        val whitelist = source.linesIterator.find(_.contains("physicalFields = Set(")).getOrElse(fail("framework whitelist missing"))
        whitelist should include("\"name\"")
        whitelist should include("\"content\"")
        whitelist should include("\"ownerId\"")
        whitelist should not include "\"payload\""
        whitelist should not include "\"securityAttributes\""
        whitelist should not include "\"auditAttributes\""
        val logical = _logical_record(source)
        logical should include("Field.Value.Single(payload)")
        logical should include("Field.Value.Single(_to_data_store_value(contentAttributes.content))")
        logical should not include "Field.Value.Single(_to_data_store_value(payload))"
        source should not include "EntityStoreField(\"securityAttributes\""
      }
      val persistent = _source(products, "/entity/Proposal.scala")
      _logical_record(persistent) should include("\"permission\", org.goldenport.record.Field.Value.Single(_permission_json(securityAttributes.rights))")
      val update = _source(products, "/entity/update/Proposal.scala")
      _logical_record(update) should not include "\"permission\""
      _logical_record(update) should include("_to_data_store_value(securityAttributes.ownerId)")
      update should include("Update.Noop")
      update should include("Update.SetNull")
      update should include("Update.SetValue(_to_data_store_value(value))")
    }
  }

  "E6 Unsupported declarations fail with field context" must afterWord(
    "in spec:entity-store-mapping-generation, example:E6, rules:R5, phase:78"
  ) {
    "reject optional collections, unordered containers and semantic ranges while generating ordered controls" in {
      Given("docs/spec/entity-store-mapping-generation.md R5 E6: an actual transformed entity AST and bounded/disjoint semantic declarations")
      val entity = _entity(List(_attribute("payload", MBuiltinStructuredDataType.Json, MOne)))
      val context = ScalaModel.Context.fromObjects(Vector(entity))
      val ast = ScalaModel.Context.withContext(context) {
        new EntityValueReadScalaModelTransformer().apply(entity).take.head.asInstanceOf[SCaseClass]
      }
      val json = TypeName.create(MBuiltinStructuredDataType.Json)
      val unsupported = Vector(
        TypeName.option(TypeName.vector(json)), TypeName.set(json),
        TypeName.Container(TypeName.Plain.create("scala", "Array"), json))
      val ranges = Vector[MMultiplicity](
        MRange(org.goldenport.record.v2.MRange(2, 4)),
        MRanges(org.goldenport.record.v2.MRanges(List(
          scalaz.NonEmptyList(org.goldenport.record.v2.MRange(1, 2)),
          scalaz.NonEmptyList(org.goldenport.record.v2.MRange(4, 5))
        )))
      )

      When("actual generation encounters unsupported declarations and supported ordered controls")
      val failures = unsupported.map { typename =>
        intercept[Exception] { _generate_ast(_replace_payload(ast, typename), context) }
      }
      val rangefailures = ranges.map { multiplicity =>
        intercept[Exception] {
          _family(_entity(List(_attribute("payload", MBuiltinStructuredDataType.Json, multiplicity))))
        }
      }
      val ordered = Vector(TypeName.vector(json), TypeName.list(json),
        TypeName.Container(TypeName.Plain.create("scala", "Seq"), json),
        TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"), json))
        .map(t => _generate_ast(_replace_payload(ast, t), context))
      val scalar = _generate_ast(ast, context)
      val nested = _generate_ast(_replace_payload(ast, TypeName.vector(TypeName.vector(json))), context)

      Then("every failure identifies the declaration and supported controls retain explicit recursive bindings")
      (failures ++ rangefailures).foreach { failure =>
        failure.getMessage should include("payload")
      }
      scalar should include("EntityStoreFieldBinding.json")
      ordered.foreach { source =>
        _field(source, "payload") should include("EntityStoreFieldBinding.array(EntityStoreFieldBinding.json)")
      }
      ordered(1) should include(".map(_.map(_.toList))")
      ordered(2) should include(".map(_.map(_.toSeq))")
      ordered(2) should include("scala.Seq(payload)")
      ordered(2) should include("Consequence.success(payload)")
      ordered(3) should include("NonEmptyVector.fromVector(xs)")
      _field(nested, "payload") should include("array(EntityStoreFieldBinding.array(EntityStoreFieldBinding.json))")
    }
  }

  private def _replace_payload(ast: SCaseClass, typename: TypeName): SCaseClass = {
    val parameters = ast.core.parameterSequence.parameters.map { parameter =>
      if (parameter.name.name == "payload") {
        parameter.copy(typeName = typename, storeDeclaration = None)
      } else { parameter }
    }
    ast.copy(core = ast.core.copy(parameterSequence = ParameterSequence(parameters)))
  }

  private def _generate_ast(ast: SCaseClass, context: ScalaModel.Context): String = {
    new Scala3CaseClassGenerator(context).generate(ast).take.slots.map(_.content).mkString("\n")
  }

  private def _family(entity: MDomainResource, values: Vector[MObject] = Vector.empty): Map[String, String] = {
    val context = ScalaModel.Context.fromObjects(Vector(entity) ++ values)
    new Scala3EntityFamilyGenerator(context).generate(entity).take.slots.map(x => x.path -> x.content).toMap
  }

  private def _persistent_products(products: Map[String, String]): Vector[String] = {
    Vector("/entity/Proposal.scala", "/entity/create/Proposal.scala", "/entity/update/Proposal.scala").map(_source(products, _))
  }

  private def _source(products: Map[String, String], suffix: String): String = {
    products.collectFirst { case (path, source) if path.endsWith(suffix) => { source } }
      .getOrElse(fail(s"generated product missing: $suffix"))
  }

  private def _field(source: String, name: String): String = {
    source.linesIterator.find(_.contains("EntityStoreField(" + EntityStoreMappingProjection.stringLiteral(name) + ","))
      .getOrElse(fail(s"declared field missing: $name"))
  }

  private def _logical_record(source: String): String = {
    val marker = "private def _logical_store_record: Record = {"
    val start = source.indexOf(marker)
    start should be >= 0
    val end = source.indexOf("\n  }", start)
    end should be > start
    source.substring(start, end)
  }

  private def _attribute(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute = {
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)
  }

  private def _entity(attributes: List[MAttribute]): MDomainResource = {
    MDomainResource(Description.name("Proposal"), MPackageRef("org.example.proposal"), Nil,
      None, Nil, Nil, attributes, Nil, Nil, Nil)
  }

  private def _summary: MDomainValue = {
    MDomainValue(Description.name("Summary"), MPackageRef("org.example.proposal.value"), Nil,
      None, Nil, Nil, List(_attribute("caption", MDataType.string, MOne),
        _attribute("rank", MAttributeType.create("int"), MOne)), Nil)
  }
}
