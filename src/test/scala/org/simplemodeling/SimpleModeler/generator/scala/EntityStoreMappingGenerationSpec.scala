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
import org.simplemodeling.SimpleModeler.generators.scala.{Scala3CaseClassGenerator, Scala3ComponentGenerator, Scala3EntityFamilyGenerator, Scala3ValueFamilyGenerator}
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.{EntityValueReadScalaModelTransformer, EntityValueCreateScalaModelTransformer, EntityValueUpdateScalaModelTransformer, EntityValueScalaModelTransformer, EntityValueQueryScalaModelTransformer}

/*
 * @since   Oct.  5, 2026
 * @version Oct. 31, 2026
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
        _attribute("requiredPayloads", MBuiltinStructuredDataType.Json, MOneMore),
        _attribute("requiredEvidences", MBuiltinStructuredDataType.SemiStructuredData, MOneMore)))

      When("the actual persistence families publish their complete logical record and declarations")
      val products = _family(entity)

      Then("optional markers, Json.Null, empty Records and empty arrays stay in direct typed Field.Single rows")
      _persistent_products(products).foreach { source =>
        val logical = _logical_record(source)
        Vector("optionalPayload", "optionalEvidence", "payloads", "evidences", "requiredPayloads", "requiredEvidences").foreach { field =>
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
        _field(source, "requiredEvidences") should include("Multiplicity.OneMore")
        _field(source, "requiredEvidences") should include("EntityStoreFieldBinding.array(EntityStoreFieldBinding.semiStructuredData)")
        source should include("Consequence.success(e._logical_store_record)")
      }
      Vector("/entity/update/Proposal.scala", "/entity/query/Proposal.scala").foreach { path =>
        val source = _source(products, path)
        source should include("_record_get_vector_as_c[io.circe.Json](record, INPUT_KEYS_PAYLOADS)")
        source should not include "_record_get_as_c[Vector[Json]]"
        source should include("_record_get_as_c[scala.Vector[org.goldenport.record.Record]](record, INPUT_KEYS_EVIDENCES)")
      }
      _source(products, "/entity/query/Proposal.scala") should include("Condition.any[scala.collection.immutable.Vector[org.goldenport.record.Record]]")
      products.values.foreach { source =>
        source should include("_record_get_as_c[cats.data.NonEmptyVector[io.circe.Json]](record, INPUT_KEYS_REQUIRED_PAYLOADS)")
        source should include("_record_get_as_c[cats.data.NonEmptyVector[org.goldenport.record.Record]](record, INPUT_KEYS_REQUIRED_EVIDENCES)")
        source should not include "NonEmptyVector.fromVector"
        source should not include "private def _record_get_vector_of_record_c"
        source should include("org.simplemodeling.model.value.ModelValueEncoder.encodeC(v).TAKE")
        val external = source.substring(source.indexOf("private def _to_external_value"), source.indexOf("private def _to_data_store_value"))
        Vector("v match", "case m:", "Map[", "toString", ".map(").foreach { traversal =>
          external should not include traversal
        }
      }
      _source(products, "/entity/update/Proposal.scala") should include("Update.noop[cats.data.NonEmptyVector[org.goldenport.record.Record]]")
      _source(products, "/entity/query/Proposal.scala") should include("Condition.any[cats.data.NonEmptyVector[io.circe.Json]]")
      val persistent = _source(products, "/entity/Proposal.scala")
      persistent should include("admitLogicalStoreRecord(r: Record): Consequence[Record] = { Consequence.success(r) }")
      persistent should include("decodeAdmittedLogicalStoreRecord(r: Record): Consequence[Proposal] = { createC(r) }")
      persistent should include("_record_get_as_c[scala.Vector[org.goldenport.record.Record]](record, INPUT_KEYS_EVIDENCES)")
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
        _attribute("smallQuantity", MAttributeType.create("short"), MOne),
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
        _field(source, "smallQuantity") should include("nativeScalar[Short](org.goldenport.schema.XShort, EntityStoreScalarRepresentation.Number")
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
      products.foreach { case (_, source) =>
        source should not include "import org.simplemodeling.datatype.Decimal"
      }
      val persistent = _source(products, "/entity/Proposal.scala")
      persistent should include("amount: BigDecimal")
      persistent should include("Consequence.toDecimal(amount)")
      persistent should not include "Consequence.toBigDecimal"
      persistent should include("_record_get_as_c[org.example.proposal.value.Summary]")
      persistent should include("_record_get_vector_as_c[org.example.proposal.value.Summary]")
      summarysource should include("org.goldenport.convert.ValueReader.recordReader[Summary]((record: Record) => createC(record)).readC(value)")
      summarysource should not include "case m: Summary =>"
      summarysource should not include "case m: Record => createC(m)"
      _source(products, "/entity/update/Proposal.scala") should include(
        "_build_changes_(org.simplemodeling.model.directive.Update.toChangesRecord(record))")
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
        val whitelist = source.linesIterator.find(_.contains("physicalFields = (Set(")).getOrElse(fail("framework whitelist missing"))
        whitelist should include("\"name\"")
        whitelist should include("\"content\"")
        whitelist should include("\"ownerId\"")
        whitelist.take(whitelist.indexOf(".diff(")) should not include "\"payload\""
        whitelist.substring(whitelist.indexOf(".diff(")) should include("\"payload\"")
        whitelist should not include "\"securityAttributes\""
        whitelist should not include "\"auditAttributes\""
        val logical = _logical_record(source)
        logical should include("Field.Value.Single(payload)")
        logical should include("\"content\" -> _to_data_store_value(contentAttributes.content)")
        logical should not include "Field.Value.Single(_to_data_store_value(payload))"
        source should not include "EntityStoreField(\"securityAttributes\""
      }
      val persistent = _source(products, "/entity/Proposal.scala")
      _logical_record(persistent) should include("\"permission\" -> _permission_json(securityAttributes.rights)")
      _field(persistent, "revision") should include("EntityRevision.DATA_TYPE")
      _field(persistent, "revision") should include("EntityStoreScalarRepresentation.Number")
      _field(persistent, "contentReferences") should include("EntityStoreFieldBinding.array")
      _field(persistent, "contentReferences") should include("DataType.Named(\"org.goldenport.value.ContentReferenceOccurrence\")")
      _logical_record(persistent) should include("Field.Value.Single(contentAttributes.references)")
      persistent should include("using classtag: scala.reflect.ClassTag[A]")
      persistent should include("private def _to_media_store_value")
      persistent should not include "EntityStoreFieldBinding.application[org.simplemodeling.model.value.Image]"
      _logical_record(persistent) should include("Record.dataAuto(")
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
      val nestednonempty = _generate_ast(_replace_payload(ast, TypeName.vector(
        TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"), json))), context)
      val nestedrecords = _generate_ast(_replace_payload(ast, TypeName.vector(TypeName.vector(TypeName.record))), context)
      val nestedapplications = _generate_ast(_replace_payload(ast, TypeName.vector(TypeName.vector(
        TypeName.Plain.create("org.example.proposal.value", "Summary")))), context)

      Then("every failure identifies the declaration and supported controls retain explicit recursive bindings")
      (failures ++ rangefailures).foreach { failure =>
        failure.getMessage should include("payload")
      }
      scalar should include("EntityStoreFieldBinding.json")
      scalar should not include "private def _decode_ordered_c"
      scalar should not include "private def _record_get_nested_c"
      scalar should not include "private def _to_media_store_value"
      scalar should not include "using classtag: scala.reflect.ClassTag[A]"
      scalar should not include "private def _record_get_vector_of_record_c"
      nested should include("private def _decode_ordered_c")
      nested should include("private def _record_get_nested_c")
      ordered.foreach { source =>
        _field(source, "payload") should include("EntityStoreFieldBinding.array(EntityStoreFieldBinding.json)")
      }
      ordered(1) should include(".map(_.map(_.toList))")
      ordered(2) should include(".map(_.map(_.toSeq))")
      ordered(2) should include("scala.Seq(payload)")
      ordered(2) should include("Consequence.success(payload)")
      ordered(3) should include("_record_get_as_c[cats.data.NonEmptyVector[io.circe.Json]](record, INPUT_KEYS_PAYLOAD)")
      ordered(3) should include("case Some(value) => Consequence.success(value)")
      ordered(3) should include("case None => Consequence.successOrPropertyNotFound(")
      ordered(3) should not include "NonEmptyVector.fromVector"
      Vector(nested, nestednonempty, nestedrecords, nestedapplications).foreach { source =>
        source should include("org.goldenport.convert.ValueReader.vector(reader).readC(value)")
        source should include("def readC(value: Any): Consequence[A] = decode(value)")
        source should not include "xs.foldLeft"
        source should not include "case xs: Seq[?]"
        source should not include "case xs: Array[?]"
        source should not include "NonEmptyVector.fromVector"
      }
      nestednonempty should include("_decode_non_empty_ordered_c[io.circe.Json](item)")
      nestednonempty should include("summon[org.goldenport.convert.ValueReader[cats.data.NonEmptyVector[A]]].readC(value)")
      nestedrecords should include("summon[org.goldenport.convert.ValueReader[org.goldenport.record.Record]].readC(item)")
      nestedapplications should include("org.goldenport.convert.ValueReader.recordReader[org.example.proposal.value.Summary]((record: Record) => org.example.proposal.value.Summary.createC(record)).readC(item)")
      _field(nested, "payload") should include("array(EntityStoreFieldBinding.array(EntityStoreFieldBinding.json))")
      nested should include("_record_get_nested_c[scala.Vector[scala.Vector[io.circe.Json]]]")
      nested should not include "_record_get_vector_as_c[scala.Vector[io.circe.Json]]"
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

  "E7 Repeated Record Update admission" should {
    "retain Record collections, patch markers and declared List restoration" in {
      Given("an entity with a repeated semi-structured Record field")
      val entity = _entity(List(_attribute("evidences", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore)))
      val context = ScalaModel.Context.fromObjects(Vector(entity))
      val ast = ScalaModel.Context.withContext(context) {
        new EntityValueUpdateScalaModelTransformer().apply(entity).take.head.asInstanceOf[SCaseClass]
      }
      val recordtype = TypeName.create(MBuiltinStructuredDataType.SemiStructuredData)
      val types = Vector(TypeName.vector(recordtype), TypeName.list(recordtype),
        TypeName.Container(TypeName.Plain.create("scala", "Seq"), recordtype))
      val updatewrapper = TypeName.Plain.create("org.simplemodeling.model.directive", "Update")

      When("the actual generator produces Update readers for each supported declared container")
      val sources = types.map { typename =>
        val parameters = ast.parameterSequence.parameters.map { parameter =>
          if (parameter.name.name == "evidences") parameter.copy(typeName = TypeName.Container(updatewrapper, typename))
          else parameter
        }
        val replaced = ast.copy(core = ast.core.copy(parameterSequence = ParameterSequence(parameters)))
        _generate_ast(replaced, context)
      }

      Then("Record decoding and the original SetNull and Noop boundaries remain present")
      sources.foreach { source =>
        source should include("_record_get_as_c[scala.Vector[org.goldenport.record.Record]](record, INPUT_KEYS_EVIDENCES)")
        source should include("Update.set(s)")
        source should include("Update.noop[")
        source should include("_record_get_update_set_null[")
      }
      sources(1) should include(").map(_.map(_.toList)).flatMap {")
      sources(2) should include(").map(_.map(_.toSeq)).flatMap {")
    }
  }

  "E8 Already admitted application collection elements" should {
    "retain the existing Record decoder alongside explicit declared-class admission" in {
      Given("a generated application collection and its Record/createC admission route")
      val entity = _entity(List(_attribute("items",
        MObjectAttributeType(MObjectRef.create("org.example.proposal.entity.Item")), MZeroMore)))

      When("the real entity family generates its public Record readers")
      val products = _family(entity)

      Then("the captured ClassTag and strict Core readers own typed admission, Record decoding and invalid-element failures")
      products.values.foreach { source =>
        source should include("using classtag: scala.reflect.ClassTag[A]")
        source should include("_record_get_as_c[Vector[A]](record, keys)(using org.goldenport.convert.ValueReader.vector(org.goldenport.convert.ValueReader.recordReader[A](decode)))")
        source should include("_record_get_vector_of_record_c(record, INPUT_KEYS_ITEMS)((r: Record) => org.example.proposal.entity.Item.createC(r))")
        source should not include "classtag.runtimeClass"
        source should not include "m.asInstanceOf[A]"
        source should not include "def decode_all"
        source should not include "decode(m).map(a => zs :+ a)"
      }
      And("the ordinary Query receives the declared Vector application argument without legacy List restoration")
      val query = _source(products, "/entity/query/Proposal.scala")
      query should include("items: Condition[Vector[Item]]")
      query should include("Condition.any[Vector[Item]]")
      query should not include "items: Condition[List[Item]]"
      query should not include ".map(_.map(_.toList))"
    }
  }

  "E9 Declared product codec materialization" should {
    "retain explicit product captures and canonical Record JSON semantics" in {
      Given("a declared application Summary and semi-structured Record field")
      val entity = _entity(List(
        _attribute("summary", MObjectAttributeType(MObjectRef.create("org.example.proposal.value.Summary")), MOne),
        _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MOne)))

      When("the real persistence families generate their selected mappings")
      val products = _family(entity, Vector(_summary))

      Then("typed product derivation uses the canonical Record bridge and keeps explicit captures")
      _persistent_products(products).foreach { source =>
        source should include("import io.circe.generic.auto.*")
        source should include("private given _declared_record_json_codec: io.circe.Codec[org.goldenport.record.Record]")
        source should include("XSemiStructuredData.fromJson(cursor.value)")
        source should include("XSemiStructuredData.toJson(record).TAKE")
        _field(source, "summary") should include("summon[io.circe.Encoder[org.example.proposal.value.Summary]]")
        _field(source, "summary") should include("summon[io.circe.Decoder[org.example.proposal.value.Summary]]")
      }
    }
  }

  "E10 Standard EntityStore physical management fields" should {
    "whitelist framework-owned defaults without admitting arbitrary application keys" in {
      Given("an ordinary entity whose typed fields exclude the framework's generated audit columns")
      val entity = _entity(List(_attribute("payload", MBuiltinStructuredDataType.Json, MOne)))

      When("the real entity family generates its declared storage mappings")
      val products = _family(entity)

      Then("all persistence families use closed policy names and exclude explicitly mapped names")
      _persistent_products(products).foreach { source =>
        source should include("SimpleEntityStorageShapePolicy.managementLogicalFields")
        source should include("SimpleEntityStorageShapePolicy.securityIdentityLogicalFields")
        source should include("SimpleEntityStorageShapePolicy.targetName(name)")
        source should include("\"content_byte_size\"")
        source should include(".diff(Set(")
        _field(source, "payload") should include("EntityStoreFieldBinding.json")
      }
    }
  }

  "E11 Existing framework media physical writes" should {
    "encode each explicit physical media array as text while retaining Update markers" in {
      Given("the actual generated SimpleEntity Information family with its existing media attributes")
      val parent = MObjectRef.create("org.simplemodeling.model.SimpleEntity")
      val entity = _entity(List(_attribute("payload", MBuiltinStructuredDataType.Json, MOne)))
        .copy(base = Some(parent))

      When("the real entity producer emits its physical media adapters")
      val products = _family(entity)

      Then("only the known framework media arrays use explicit JSON text before SQL")
      _persistent_products(products).foreach { source =>
        source should include("private def _to_media_store_value")
        Vector("images", "audios", "videos", "attachments").foreach { key =>
          _logical_record(source) should include("\"" + key + "\" -> _to_media_store_value(")
        }
        source should include("Update.SetValue(_to_media_store_value(value))")
      }
    }
  }

  "E12 Compiled consumer fixture provenance" should {
    "emit complete native entity families with nested Json and Record containers" in {
      Given("a semantic entity and native transformed AST declarations for nested ordered containers")
      val entity = MDomainResource(Description.name("Proposal"), MPackageRef("org.example.phase100.nested"), Nil,
        None, Nil, Nil, List(_attribute("id", MAttributeType.create("entityid"), MOne),
          _attribute("payload", MBuiltinStructuredDataType.Json, MZeroMore),
          _attribute("evidence", MBuiltinStructuredDataType.SemiStructuredData, MZeroMore),
          _attribute("nonempty", MBuiltinStructuredDataType.Json, MOneMore)), Nil, Nil, Nil)
      val context = ScalaModel.Context.fromObjects(Vector(entity))
      val json = TypeName.vector(TypeName.vector(TypeName.create(MBuiltinStructuredDataType.Json)))
      val record = TypeName.list(TypeName.vector(TypeName.create(MBuiltinStructuredDataType.SemiStructuredData)))

      When("the actual Scala3 generator receives each transformed AST before producing its source")
      val products = ScalaModel.Context.withContext(context) {
        val transformed = Vector(
          new EntityValueScalaModelTransformer().apply(entity, ScalaModelTransformer.Purpose.Plain),
          new EntityValueCreateScalaModelTransformer().apply(entity),
          new EntityValueUpdateScalaModelTransformer().apply(entity),
          new EntityValueQueryScalaModelTransformer().apply(entity))
        transformed.flatMap { result =>
          val ast = result.take.head.asInstanceOf[SCaseClass]
          val parameters = ast.core.parameterSequence.parameters.map { parameter =>
            val nested = parameter.name.name match {
              case "payload" => Some(json)
              case "evidence" => Some(record)
              case "nonempty" => Some(TypeName.Container(TypeName.Plain.create("cats.data", "NonEmptyVector"), json.containee))
              case _ => None
            }
            nested.map { typename =>
              val target = parameter.typeName match {
                case wrapper: TypeName.Container if Set("org.simplemodeling.model.directive.Update", "org.simplemodeling.model.directive.Condition")
                    .contains(wrapper.container.fullName) => wrapper.copy(containee = typename)
                case _ => typename
              }
              parameter.copy(typeName = target, storeDeclaration = None)
            }.getOrElse(parameter)
          }
          val selected = ast.copy(core = ast.core.copy(parameterSequence = ParameterSequence(parameters)))
          new Scala3CaseClassGenerator(context).generate(selected).take.slots
        }
      }
      val directory = java.nio.file.Paths.get("target/phase100/nested-fixture")
      val written = products.map { slot =>
        val path = directory.resolve(slot.path.stripPrefix("/"))
        java.nio.file.Files.createDirectories(path.getParent)
        val formatted = slot.content.linesIterator.map(_.replaceAll("[ \t]+$", "")).mkString("\n") + "\n"
        java.nio.file.Files.write(path, formatted.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        path
      }

      Then("all four unchanged native products are available for compiled consumer admission checks")
      products should have size 4
      products.foreach { slot =>
        slot.content should include("_record_get_nested_c[")
        slot.content should include("org.goldenport.convert.ValueReader.vector(reader).readC(value)")
        slot.content should include("summon[org.goldenport.convert.ValueReader[org.goldenport.record.Record]].readC(item)")
        slot.content should include("_record_get_nested_c[cats.data.NonEmptyVector[scala.Vector[io.circe.Json]]](record, INPUT_KEYS_NONEMPTY)")
        slot.content should include("_decode_non_empty_ordered_c[scala.Vector[io.circe.Json]](value)")
        slot.content should not include "NonEmptyVector.fromVector"
        slot.content should not include "_record_get_vector_as_c[scala.Vector[io.circe.Json]]"
      }
      written.foreach(path => java.nio.file.Files.isRegularFile(path) shouldBe true)
    }
  }

  "E13 Declared POWERTYPE numeric persistence" should {
    "capture fixed numeric codecs for required, optional and ordered fields in every persistence family" in {
      Given("a resolved POWERTYPE with explicit codes and direct, Option, Vector and NonEmptyVector uses")
      val powertype = _preparation_state(List(
        MPowertypeKind("prepared", Some("0")), MPowertypeKind("unprepared", Some("+20")),
        MPowertypeKind("minimum", Some(Short.MinValue.toString)), MPowertypeKind("maximum", Some(Short.MaxValue.toString))))
      val datatype = MObjectAttributeType(MObjectRef.create(powertype.qualifiedName))
      val entity = _entity(List(
        _attribute("state", datatype, MOne),
        _attribute("optionalState", datatype, MZeroOne),
        _attribute("states", datatype, MZeroMore),
        _attribute("requiredStates", datatype, MOneMore)))

      When("the actual entity family uses its explicit declaration context")
      val products = _family(entity, Vector(powertype))

      Then("all persistence families capture the same declared numeric codec and native XShort domain")
      val classname = powertype.qualifiedName
      val binding = s"EntityStoreFieldBinding.nativeScalar[$classname](org.goldenport.schema.XShort, EntityStoreScalarRepresentation.Number, " +
        EntityStoreMappingProjection.stringLiteral("declared:" + classname) +
        s")(using summon[scala.reflect.ClassTag[$classname]], $classname.numericCodeCodec.jsonCodec, $classname.numericCodeCodec.jsonCodec)"
      _persistent_products(products).foreach { source =>
        Vector("state", "optionalState", "states", "requiredStates").foreach { name =>
          _field(source, name) should include(binding)
          _field(source, name) should not include s"application[$classname]"
          _field(source, name) should not include s"summon[io.circe.Encoder[$classname]]"
        }
        _field(source, "state") should include("Multiplicity.One")
        _field(source, "optionalState") should include("Multiplicity.ZeroOne")
        _field(source, "states") should include("Multiplicity.ZeroMore")
        _field(source, "requiredStates") should include("Multiplicity.OneMore")
        _field(source, "states") should include(s"EntityStoreFieldBinding.array($binding)")
        _field(source, "requiredStates") should include(s"EntityStoreFieldBinding.array($binding)")
        source should include("org.simplemodeling.model.value.ModelValueEncoder.encodeC(v).TAKE")
      }
      And("Query retains its existing external projection and has no persistence mapping")
      val query = _source(products, "/entity/query/Proposal.scala")
      query should include("org.simplemodeling.model.value.ModelValueEncoder.encodeC(v).TAKE")
      query should not include "_store_row_mapping"
      query should not include "numericCodeCodec.jsonCodec"
    }
  }

  "E14 Persisted POWERTYPE declarations require complete unique Short codes" should {
    "reject missing, malformed, null, overflowing and duplicate codes through every admitted wrapper" in {
      Given("invalid resolved POWERTYPE declarations and direct, Option, Vector and NonEmptyVector uses")
      val invalidkinds = Vector(
        List(MPowertypeKind("prepared", None)),
        List(MPowertypeKind("prepared", Some("prepared"))),
        List(MPowertypeKind("prepared", Some("1.0"))),
        List(MPowertypeKind("prepared", Some("0x10"))),
        List(MPowertypeKind("prepared", Some(null))),
        List(MPowertypeKind("prepared", Some("2147483648"))),
        List(MPowertypeKind("prepared", Some("-2147483649"))),
        List(MPowertypeKind("prepared", Some("32768"))),
        List(MPowertypeKind("prepared", Some("-32769"))),
        List(MPowertypeKind("prepared", Some("1")), MPowertypeKind("unprepared", Some("+01"))))
      val multiplicities = Vector[MMultiplicity](MOne, MZeroOne, MZeroMore, MOneMore)

      When("the real entity family attempts to generate each invalid persistent declaration")
      val failures = for {
        kinds <- invalidkinds
        multiplicity <- multiplicities
      } yield {
        val powertype = _preparation_state(kinds)
        val entity = _entity(List(_attribute("state",
          MObjectAttributeType(MObjectRef.create(powertype.qualifiedName)), multiplicity)))
        intercept[Exception] { _family(entity, Vector(powertype)) }
      }

      Then("generation fails with the persistent attribute, owning type and kind before yielding usable products")
      failures.foreach { failure =>
        failure.getMessage should include("state")
        failure.getMessage should include("ProposalPreparationState")
        failure.getMessage should include("prepared")
      }
    }
  }

  "E15 Ordinary Scala Short declarations retain their numeric width" should {
    "select XShort for a plain Scala Short leaf and XInt for a plain Scala Int leaf" in {
      Given("an actual read-family AST with an explicitly selected Scala numeric leaf")
      val entity = _entity(List(_attribute("payload", MAttributeType.create("short"), MOne)))
      val context = ScalaModel.Context.fromObjects(Vector(entity))
      val ast = ScalaModel.Context.withContext(context) {
        new EntityValueReadScalaModelTransformer().apply(entity).take.head.asInstanceOf[SCaseClass]
      }

      When("the real generator receives primitive Short and plain Scala Short or Int declarations")
      val primitive = _generate_ast(ast, context)
      val shortsource = _generate_ast(_replace_payload(ast, TypeName.Plain.create("scala", "Short")), context)
      val intsource = _generate_ast(_replace_payload(ast, TypeName.Plain.create("scala", "Int")), context)

      Then("ordinary Short uses the same signed16 declaration as a POWERTYPE and Int retains its own domain")
      _field(primitive, "payload") should include("nativeScalar[Short](org.goldenport.schema.XShort, EntityStoreScalarRepresentation.Number")
      _field(shortsource, "payload") should include("nativeScalar[scala.Short](org.goldenport.schema.XShort, EntityStoreScalarRepresentation.Number")
      _field(intsource, "payload") should include("nativeScalar[scala.Int](org.goldenport.schema.XInt, EntityStoreScalarRepresentation.Number")
    }
  }

  "E16 POWERTYPE selection preserves declared type identity" should {
    "retain an unresolved qualified application type beside coded or uncoded local vocabularies" in {
      Given("an explicit external.State attribute and unrelated local State declarations")
      val entity = _entity(List(_attribute("state", MObjectAttributeType(MObjectRef.create("external.State")), MOne)))
      val scenarios = Vector(None, Some("7")).flatMap { code =>
        val local = MPowertype(Description.name("State"), MPackageRef(entity.packageName),
          List(MPowertypeKind("ready", code)))
        Vector(Vector[MObject](entity, local), Vector[MObject](local, entity)).map(ScalaModel.Context.fromObjects)
      }

      When("the actual read transformation and entity family consume each declaration context")
      val observations = scenarios.map { context =>
        val ast = ScalaModel.Context.withContext(context) {
          new EntityValueReadScalaModelTransformer().apply(entity).take.head
        }
        val products = new Scala3EntityFamilyGenerator(context).generate(entity).take.slots.map(slot => slot.path -> slot.content).toMap
        (ast.attributeSequence.attributes.find(_.name.name == "state").get.typeName, products)
      }

      Then("neither context order nor a local code assignment rewrites the explicit foreign type")
      observations.foreach { case (typename, products) =>
        typename.fullName shouldBe "external.State"
        _persistent_products(products).foreach { source =>
          _field(source, "state") should include("EntityStoreFieldBinding.application[external.State]")
          _field(source, "state") should not include "numericCodeCodec"
          _field(source, "state") should not include "org.goldenport.schema.XShort"
        }
      }
    }

    "select a resolved imported vocabulary ahead of another scoped State for every declaration ordering" in {
      Given("an explicit imported.State attribute, its coded declaration and an uncoded scoped State")
      val entity = _entity(List(_attribute("state", MObjectAttributeType(MObjectRef.create("imported.State")), MOne)))
      val imported = MPowertype(Description.name("State"), MPackageRef("imported"), List(MPowertypeKind("ready", Some("3"))))
      val local = MPowertype(Description.name("State"), MPackageRef(entity.packageName), List(MPowertypeKind("local", None)))
      val contexts = Vector[MObject](entity, local, imported).permutations.map(ScalaModel.Context.fromObjects).toVector

      When("semantic transformation and actual entity generation run for all model permutations")
      val observations = contexts.map { context =>
        val ast = ScalaModel.Context.withContext(context) {
          new EntityValueReadScalaModelTransformer().apply(entity).take.head
        }
        val products = new Scala3EntityFamilyGenerator(context).generate(entity).take.slots.map(slot => slot.path -> slot.content).toMap
        (ast.attributeSequence.attributes.find(_.name.name == "state").get.typeName, products)
      }

      Then("the generated type and every persisted binding retain the authoritative imported identity")
      observations should have size 6
      observations.foreach { case (typename, products) =>
        typename.fullName shouldBe "imported.State"
        _persistent_products(products).foreach { source =>
          _field(source, "state") should include("nativeScalar[imported.State](org.goldenport.schema.XShort")
          _field(source, "state") should include("imported.State.numericCodeCodec.jsonCodec")
          _field(source, "state") should not include s"${entity.packageName}.State"
        }
      }
    }

    "respect unqualified scope and preserve non-POWERTYPE shadowing without choosing ambiguous vocabularies" in {
      Given("an unqualified State attribute and scoped, unique imported, ambiguous or application declarations")
      val entity = _entity(List(_attribute("state", MObjectAttributeType(MObjectRef.create("State")), MOne)))
      val scoped = MPowertype(Description.name("State"), MPackageRef(entity.packageName), List(MPowertypeKind("ready", Some("1"))))
      val first = scoped.copy(affiliation = MPackageRef("first"))
      val second = scoped.copy(affiliation = MPackageRef("second"))
      val application = _summary.copy(description = Description.name("State"), affiliation = MPackageRef(entity.packageName))
      val contexts = Vector(
        ScalaModel.Context.fromObjects(Vector(entity, scoped, first)),
        ScalaModel.Context.fromObjects(Vector(entity, first)),
        ScalaModel.Context.fromObjects(Vector(entity, first, second)),
        ScalaModel.Context.fromObjects(Vector(entity, application, first)))

      When("the real semantic and persistence generators resolve the four contexts")
      val observations = contexts.map { context =>
        val ast = ScalaModel.Context.withContext(context) {
          new EntityValueReadScalaModelTransformer().apply(entity).take.head
        }
        val products = new Scala3EntityFamilyGenerator(context).generate(entity).take.slots.map(slot => slot.path -> slot.content).toMap
        (ast.attributeSequence.attributes.find(_.name.name == "state").get.typeName, _field(_source(products, "/entity/Proposal.scala"), "state"))
      }

      Then("scoped and unique vocabularies use their own codec while ambiguous names remain unresolved")
      observations(0)._1.fullName shouldBe scoped.qualifiedName
      observations(0)._2 should include(s"nativeScalar[${scoped.qualifiedName}](org.goldenport.schema.XShort")
      observations(1)._1.fullName shouldBe first.qualifiedName
      observations(1)._2 should include("nativeScalar[first.State](org.goldenport.schema.XShort")
      observations(2)._1.fullName shouldBe "State"
      observations(2)._2 should include("EntityStoreFieldBinding.application[State]")
      observations(2)._2 should not include "numericCodeCodec"

      And("an existing scoped application value keeps its legacy non-POWERTYPE selection")
      observations(3)._1.fullName shouldBe application.qualifiedName
      observations(3)._2 should include(s"EntityStoreFieldBinding.application[${application.qualifiedName}]")
      observations(3)._2 should not include "numericCodeCodec"
    }
  }

  "E17 Required shared expiry" should {
    "compile generated families and preserve exact historical expiry through reopen and patch admission" in {
      Given("a required Instant refinement of SimpleEntity resource expiry and the selected CNCF runtime")
      val products = GeneratedRuntimeSpecification.expiryProducts
      val required = java.lang.Boolean.getBoolean("textus.sm.generated-runtime.required")
      if (!GeneratedRuntimeSpecification.isConfigured && !required) cancel("Generated runtime classpaths were not selected")

      When("the actual emitted Scala 3 families are compiled and their public persistence APIs execute")
      val probe = GeneratedRuntimeSpecification.compile(products, GeneratedRuntimeSpecification.expiryProbe, "GeneratedExpiryProbe")
      try {
        Then("nanosecond instants roundtrip through the single declared expiry and requiredness rejects invalid representations")
        val seed = java.util.concurrent.ThreadLocalRandom.current().nextLong()
        // Each sample opens real providers; shrinking must not replay its storage effects.
        val property = org.scalacheck.Prop.forAllNoShrink(org.scalacheck.Gen.choose(1893456000L, 4102444800L), org.scalacheck.Gen.choose(0, 999999999)) { (seconds, nanos) =>
          val samplecontext = s"E17 seed=$seed seconds=$seconds nanos=$nanos: "
          try {
            val facts = probe.invoke("check", Array(java.lang.Long.TYPE, java.lang.Integer.TYPE), Array(Long.box(seconds), Int.box(nanos)))
              .asInstanceOf[java.util.Map[String, java.lang.Boolean]]
            withClue(samplecontext) {
              facts.keySet should contain allOf ("runtime", "roundtrip", "reopen", "single", "missing", "malformed", "aliases", "nested", "create", "query", "update", "clear", "sqlite", "mutators", "groupAliases", "groupClear", "revision")
              import scala.collection.JavaConverters._
              facts.asScala.foreach { case (name, passed) => withClue(s"fact=$name: ") { passed.booleanValue shouldBe true } }
            }
            true
          } catch {
            case scala.util.control.NonFatal(error) =>
              val cause = error match {
                case invocation: java.lang.reflect.InvocationTargetException if invocation.getCause != null => invocation.getCause
                case other => other
              }
              throw new java.lang.AssertionError(samplecontext + GeneratedRuntimeSpecification.describeFailure(cause), cause)
          }
        }
        val result = org.scalacheck.Test.check(org.scalacheck.Test.Parameters.default
          .withInitialSeed(org.scalacheck.rng.Seed(seed)).withMinSuccessfulTests(20), property)
        result.status match {
          case failure: org.scalacheck.Test.PropException =>
            fail(s"E17 seed=$seed status=${result.status} succeeded=${result.succeeded}: ${failure.e.getMessage}", failure.e)
          case _ =>
            withClue(s"E17 seed=$seed status=${result.status} succeeded=${result.succeeded}: ") { result.passed shouldBe true }
        }
        result.succeeded should be >= 20
      } finally probe.close()
    }

    "retain rejection of unrelated and non-refining framework collisions" in {
      Given("ordinary fields trying to reuse a framework key without the exact required Instant refinement")
      val candidates = Vector(
        _attribute("expiresAt", MDataType.string, MOne),
        _attribute("expiresAt", MObjectAttributeType(MObjectRef.create("java.time.Instant")), MZeroOne),
        _attribute("activatedAt", MObjectAttributeType(MObjectRef.create("java.time.Instant")), MOne))
      When("ordinary generation attempts each declaration")
      val failures = candidates.map { attribute =>
        intercept[Exception] { _family(_entity(List(attribute)).copy(base = Some(MObjectRef.create("org.simplemodeling.model.SimpleEntity")))) }
      }
      Then("the existing framework collision boundary rejects each declaration")
      failures.foreach(_.getMessage should include("colliding framework field"))
    }
  }

  private def _preparation_state(kinds: List[MPowertypeKind]): MPowertype = {
    MPowertype(Description.name("ProposalPreparationState"), MPackageRef("org.example.proposal"), kinds)
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

/** Compiles ordinary emitted sources in a Scala 3 loader isolated from the Scala 2 producer. */
private[scala] object GeneratedRuntimeSpecification {
  def isConfigured: Boolean =
    sys.props.contains("textus.sm.cncf.runtime-classpath-file") &&
      sys.props.contains("textus.sm.scala3.compiler-classpath-file")

  def describeFailure(failure: Throwable): String = {
    val seen = new java.util.IdentityHashMap[Throwable, java.lang.Boolean]()
    val messages = scala.collection.mutable.ArrayBuffer.empty[String]
    var current = failure
    var depth = 0
    while (current != null && !seen.containsKey(current) && depth < 32) {
      seen.put(current, java.lang.Boolean.TRUE)
      val message = Option(current.getMessage).getOrElse("<no message>").replace('\n', ' ').replace('\r', ' ')
      val location = current.getStackTrace.take(3).mkString(" | ")
      messages += s"$depth:${current.getClass.getName}: $message [$location]"
      current = Option(current.getCause).orElse(current match {
        case initializer: ExceptionInInitializerError => Option(initializer.getException)
        case _ => None
      }).orNull
      depth += 1
    }
    if (current != null) messages += (if (seen.containsKey(current)) "<cause cycle>" else "<cause chain exceeds 32 levels>")
    messages.mkString(" caused by ")
  }

  def expiryProducts: Map[String, String] = {
    // Same semantic projection as Cozy ModelTypeProjector's CML instant branch.
    val attribute = MAttribute(Designation("expiresAt"), MObjectAttributeType(MObjectRef.create("java.time.Instant")), MOne, Nil, None)
    val entity = MDomainResource(Description.name("Proposal"), MPackageRef("org.example.proposal"), Nil,
      Some(MObjectRef.create("org.simplemodeling.model.SimpleEntity")), Nil, Nil, List(attribute), Nil, Nil, Nil)
    val context = ScalaModel.Context.fromObjects(Vector(entity))
    val entities = new Scala3EntityFamilyGenerator(context).generate(entity).take.slots.map(slot => slot.path -> slot.content).toMap
    // Native bootstrap selects descriptors through the component's registered services.
    // Include an ordinary operation so the producer emits the Proposal service protocol.
    val service = SService("org.example.proposal", "Proposal", MethodCompartment(Vector(
      SMethod(MethodName("inspect"), SMethod.Descriptor.query,
        ParameterSequence(Vector(Parameter(ParameterName("key"), TypeName.Primitive.string))), TypeName.Primitive.string))),
      Vector.empty)
    val component = SComponent(
      core = ClassCore(packageName = PackageName("org.example.proposal"), declaration = ClassDeclaration.Control,
        className = ClassName("ExpiryComponent"), parentClass = Some(TypeName.Plain.create("org.goldenport.cncf.component", "Component"))),
      componentCore = SComponent.ComponentCore(componentName = "org.example.proposal.Expiry", services = List(service),
        entityRuntimeDescriptors = Vector(SComponent.EntityRuntimeDescriptor("Proposal", PackageName("org.example.proposal.entity"),
          revisionModelKind = Some("simple-entity"), revisionRepresentation = Some("embedded")))))
    val components = new Scala3ComponentGenerator(context).generate(component).take.slots.map(slot => slot.path -> slot.content).toMap
    entities ++ components
  }

  final class CompiledProbe private[GeneratedRuntimeSpecification](loader: java.net.URLClassLoader, probe: Class[_]) extends AutoCloseable {
    def invoke(name: String, parameterTypes: Array[Class[_]], arguments: Array[AnyRef]): AnyRef = {
      val thread = Thread.currentThread()
      val previous = thread.getContextClassLoader
      thread.setContextClassLoader(loader)
      try probe.getMethod(name, parameterTypes: _*).invoke(null, arguments: _*)
      finally thread.setContextClassLoader(previous)
    }
    def close(): Unit = loader.close()
  }

  def compile(products: Map[String, String], probeSource: String, probeClass: String): CompiledProbe = {
    import java.nio.file.{Files, Paths}
    import scala.collection.JavaConverters._
    def _classpath_(property: String): Vector[java.nio.file.Path] = {
      val file = Paths.get(sys.props.getOrElse(property, throw new IllegalStateException("Required generated-runtime input missing: " + property)))
      val paths = Files.readAllLines(file).asScala.filter(_.trim.nonEmpty).map(line => Paths.get(line.trim)).toVector
      require(paths.nonEmpty && paths.forall(path => Files.isRegularFile(path)), "Runtime/compiler classpath must contain existing artifacts")
      paths
    }
    val runtime = _classpath_("textus.sm.cncf.runtime-classpath-file")
    val compiler = _classpath_("textus.sm.scala3.compiler-classpath-file")
    val directory = Files.createTempDirectory(Paths.get("target"), "generated-runtime-")
    val output = Files.createDirectory(directory.resolve("classes"))
    val sources = products.toVector.filter(_._1.endsWith(".scala")).map { case (path, content) =>
      val relative = path.stripPrefix("/")
      require(!relative.split('/').contains(".."), "Generated source must stay in fixture directory")
      val file = directory.resolve("sources").resolve(relative)
      Files.createDirectories(file.getParent)
      Files.write(file, content.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      file.toString
    }
    val probefile = directory.resolve(probeClass + ".scala")
    Files.write(probefile, probeSource.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    // Dotty's implementation uses the selected Scala 3/2.13 runtime libraries as well.
    // The platform parent keeps the Scala 2.12 producer outside this loader.
    val compilerclasspath = (compiler ++ runtime).distinct
    val compilerloader = new java.net.URLClassLoader(compilerclasspath.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    val thread = Thread.currentThread()
    val previous = thread.getContextClassLoader
    try {
      thread.setContextClassLoader(compilerloader)
      val main = compilerloader.loadClass("dotty.tools.dotc.Main")
      val arguments = Array("-classpath", runtime.mkString(java.io.File.pathSeparator), "-d", output.toString) ++ sources ++ Array(probefile.toString)
      val reporter = main.getMethod("process", classOf[Array[String]]).invoke(null, arguments.asInstanceOf[AnyRef])
      val errors = reporter.getClass.getMethod("hasErrors").invoke(reporter).asInstanceOf[java.lang.Boolean]
      require(!errors.booleanValue, "Generated Scala 3 compilation failed; diagnostics above, retained source: " + directory)
    } finally {
      thread.setContextClassLoader(previous)
      compilerloader.close()
    }
    val loader = new java.net.URLClassLoader((Vector(output) ++ runtime).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    try new CompiledProbe(loader, loader.loadClass(probeClass))
    catch { case error: Throwable => loader.close(); throw error }
  }

  val expiryProbe: String = """
import java.time.Instant
import org.goldenport.record.Record
import org.goldenport.cncf.entity.{EntityPersistent, EntityPersistentCreate, EntityPersistentUpdate}
import org.simplemodeling.model.datatype.EntityId
import org.example.proposal.entity.Proposal
object GeneratedExpiryProbe {
  def check(epochSecond: Long, nano: Int): java.util.Map[String, java.lang.Boolean] = {
    val expiry = Instant.ofEpochSecond(epochSecond, nano)
    val id = EntityId.restore("test", "runtime", Proposal.collectionId, Instant.EPOCH, "expiry").TAKE
    val row = Record.dataAuto("id" -> id, "expiresAt" -> expiry)
    val persistent = summon[EntityPersistent[Proposal]]
    val entity = Proposal.createC(row).TAKE
    val encoded = persistent.toStoreRecordC(Proposal.collectionId, entity).TAKE
    val reopened = persistent.fromStoreRecordC(Proposal.collectionId, encoded).TAKE
    // A fresh Record containing the historical physical spelling is admitted independently.
    val historical = Record.createFull(encoded.fields.map(field => field.key -> field.value).map {
      case ("expires_at", _) => "expires_at" -> expiry.toString
      case (key, org.goldenport.record.Field.Value.Single(value)) => key -> value
    })
    val old = persistent.fromStoreRecordC(Proposal.collectionId, historical).TAKE
    val created = org.example.proposal.entity.create.Proposal.createC(Record.dataAuto("expires_at" -> expiry)).TAKE
    val query = org.example.proposal.entity.query.Proposal.createC(Record.dataAuto("expires_at" -> expiry)).TAKE
    val patch = org.example.proposal.entity.update.Proposal.createC(Record.dataAuto("expires_at" -> expiry)).TAKE
    val update = summon[EntityPersistentUpdate[org.example.proposal.entity.update.Proposal]]
    val changes = update.toStoreChangesRecordC(Proposal.collectionId, patch).TAKE
    val moved = entity.withExpiresAt(expiry.plusNanos(1))
    val resource = moved.resourceAttributes.copy(expiresAt = Some(expiry.plusSeconds(1).atZone(java.time.ZoneOffset.ofHours(9))))
    val regrouped = moved.withResourceAttributes(resource)
    val movedcreate = created.withResourceAttributes(resource).withExpiresAt(expiry.plusNanos(2))
    val movedpatch = patch.withResourceAttributes(patch.resourceAttributes.copy(
      expiresAt = org.simplemodeling.model.directive.Update.set(expiry.plusSeconds(2).atZone(java.time.ZoneOffset.ofHours(9)))))
      .withExpiresAt(org.simplemodeling.model.directive.Update.set(expiry.plusNanos(3)))
    val equalgroups = Proposal.createC(row ++ Record.dataAuto(
      "resourceAttributes" -> org.simplemodeling.model.value.ResourceAttributes(expiresAt = Some(expiry.atZone(java.time.ZoneOffset.UTC))),
      "resource_attributes" -> org.simplemodeling.model.value.ResourceAttributes(expiresAt = Some(expiry.atZone(java.time.ZoneOffset.ofHours(9))))))
    val database = java.nio.file.Files.createTempDirectory(java.nio.file.Path.of("target"), "expiry-sql-").toAbsolutePath.resolve("application.db")
    val writer = org.goldenport.cncf.datastore.sql.SqlDataStore.jdbc("jdbc:sqlite:" + database)
    locally {
      given org.goldenport.cncf.context.ExecutionContext = _context(writer)
      try {
        val store = org.goldenport.cncf.entity.EntityStore.standard()
        _take("sqlite create", store.create(org.example.proposal.entity.create.Proposal.createC(row).TAKE,
          org.goldenport.cncf.entity.EntityCreateOptions(Set("publication"))))
        val saved = _take("sqlite load before save", store.load[Proposal](id)).get
        _take("sqlite revision-bearing save", store.save(saved, saved.revision))
      } finally writer.closeC().TAKE
    }
    val reader = org.goldenport.cncf.datastore.sql.SqlDataStore.jdbc("jdbc:sqlite:" + database)
    val durable = locally {
      given org.goldenport.cncf.context.ExecutionContext = _context(reader)
      try {
        val restored = _take("sqlite reopened load", org.goldenport.cncf.entity.EntityStore.standard().load[Proposal](id)).get
        (restored.expiresAt == expiry && restored.resourceAttributes.expiresAt.exists(_.toInstant == expiry),
          restored.revision.value == 2L)
      } finally reader.closeC().TAKE
    }
    val facts = new java.util.LinkedHashMap[String, java.lang.Boolean]()
    facts.put("mutators", moved.expiresAt.compareTo(expiry.plusNanos(1)) == 0 &&
      moved.resourceAttributes.expiresAt.exists(_.toInstant.compareTo(moved.expiresAt) == 0) &&
      regrouped.expiresAt.compareTo(expiry.plusSeconds(1)) == 0 &&
      regrouped.resourceAttributes.expiresAt.exists(_.toInstant.compareTo(regrouped.expiresAt) == 0) &&
      movedcreate.expiresAt.compareTo(expiry.plusNanos(2)) == 0 &&
      movedcreate.resourceAttributes.expiresAt.exists(_.toInstant.compareTo(movedcreate.expiresAt) == 0) &&
      movedpatch.expiresAt.fold(false, _.compareTo(expiry.plusNanos(3)) == 0, false) &&
      movedpatch.resourceAttributes.expiresAt.fold(false, _.toInstant.compareTo(expiry.plusNanos(3)) == 0, false))
    facts.put("groupAliases", equalgroups.isSuccess && equalgroups.TAKE.expiresAt.compareTo(expiry) == 0)
    facts.put("groupClear", scala.util.Try(entity.withResourceAttributes(entity.resourceAttributes.copy(expiresAt = None))).isFailure &&
      scala.util.Try(patch.withResourceAttributes(patch.resourceAttributes.copy(expiresAt = org.simplemodeling.model.directive.Update.setNull[java.time.ZonedDateTime]))).isFailure)
    facts.put("sqlite", durable._1)
    facts.put("revision", durable._2)
    facts.put("runtime", org.goldenport.cncf.CncfVersion.current == "0.5.3-SNAPSHOT")
    facts.put("roundtrip", reopened.expiresAt == expiry && reopened.resourceAttributes.expiresAt.exists(_.toInstant == expiry))
    facts.put("reopen", old.expiresAt == expiry && old.id == id)
    facts.put("single", encoded.fields.count(field => field.key == "expires_at" || field.key == "expiresAt") == 1)
    facts.put("missing", !Proposal.createC(Record.dataAuto("id" -> id)).isSuccess)
    facts.put("malformed", !Proposal.createC(Record.dataAuto("id" -> id, "expiresAt" -> "not-an-instant")).isSuccess)
    facts.put("aliases", !Proposal.createC(row ++ Record.dataAuto("expires_at" -> expiry.plusNanos(1))).isSuccess)
    facts.put("nested", !Proposal.createC(row ++ Record.dataAuto("resourceAttributes" -> org.simplemodeling.model.value.ResourceAttributes(expiresAt = Some(expiry.plusSeconds(1).atZone(java.time.ZoneOffset.UTC))))).isSuccess)
    facts.put("create", created.expiresAt == expiry && created.resourceAttributes.expiresAt.exists(_.toInstant == expiry))
    facts.put("query", query.expiresAt == org.simplemodeling.model.directive.Condition.is(expiry) &&
      !org.example.proposal.entity.query.Proposal.createC(Record.dataAuto("expires_at" -> "bad-instant")).isSuccess)
    facts.put("update", patch.resourceAttributes.expiresAt.fold(false, _.toInstant == expiry, false) && changes.getAny("expires_at").nonEmpty)
    facts.put("clear", !org.example.proposal.entity.update.Proposal.createC(Record.dataAuto("expiresAt" -> org.simplemodeling.model.directive.Update.setNull[Instant])).isSuccess)
    facts
  }

  private def _context(provider: org.goldenport.cncf.datastore.DataStore): org.goldenport.cncf.context.ExecutionContext = {
    val subsystem = org.goldenport.cncf.subsystem.Subsystem(name = "expiry-generated-runtime",
      configuration = org.goldenport.configuration.ResolvedConfiguration(
        org.goldenport.configuration.Configuration.empty, org.goldenport.configuration.ConfigurationTrace.empty))
    val component = _take("generated component factory", new org.example.proposal.ExpiryComponent.Factory().createPrimaryC(
      org.goldenport.cncf.component.ComponentCreate(subsystem, org.goldenport.cncf.component.ComponentOrigin.Main)))
    val bootstrapped = _take("generated component bootstrap", new org.goldenport.cncf.component.ComponentFactory().bootstrapC(component))
    val collection = bootstrapped.entitySpace.entityOption(Proposal.collectionId).get
    val context = org.goldenport.cncf.context.ExecutionContext.create()
    context.dataStoreSpace.useDataStore(provider)
    context.entitySpace.registerEntity(collection.descriptor.plan.entityName, collection)
    context
  }

  private def _take[A](stage: String, result: => org.goldenport.Consequence[A]): A = {
    val evaluated = try result catch {
      case error: ExceptionInInitializerError => throw new IllegalStateException(stage, error)
      case scala.util.control.NonFatal(error) => throw new IllegalStateException(stage, error)
    }
    evaluated match {
      case org.goldenport.Consequence.Success(value) => value
      case org.goldenport.Consequence.Failure(conclusion) =>
        throw new IllegalStateException(stage + ": " + (conclusion.show +: conclusion.causes.map(_.display)).mkString(" | "), conclusion.getException.orNull)
    }
  }
}
"""
}
