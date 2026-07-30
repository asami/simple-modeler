package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.record.v2.XEntityId
import org.goldenport.values.Designation
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainResource
import org.simplemodeling.SimpleModeler.generator.scala.model.SCaseClass
import org.simplemodeling.SimpleModeler.generators.scala.Scala3EntityFamilyGenerator
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose

/*
 * @since   Jul. 25, 2026
 * @version Jul. 29, 2026
 * @author  ASAMI, Tomoharu
 */
final class SimpleEntityRevisionGenerationSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  "SimpleEntity Scala generation" should {
    "project one managed revision on outputs and none on application inputs" in {
      Given("a SimpleEntity model carrying the canonical revision attribute")
      val entity = _entity
      ScalaModelTransformer.clearObjectRegistry()
      ScalaModelTransformer.registerObject(entity)

      When("the complete Scala Entity family is generated")
      val sources = new Scala3EntityFamilyGenerator()
        .generate(entity)
        .take
        .slots
        .map(x => x.path -> x.content)
        .toMap
      val plainmodel = new EntityValueScalaModelTransformer()
        .apply((entity, Purpose.Plain))
        .take
        .collectFirst { case m: SCaseClass => m }
        .getOrElse(fail("generated plain Entity model missing"))

      Then("every Entity output implements one typed read-only revision")
      _output_paths.foreach { path =>
        val source = _source(sources, path)
        source should include(
          "override val id: EntityId, override val revision: EntityRevision"
        )
        _occurrences(source, "override val revision: EntityRevision") shouldBe 1
        source should include("\"revision\" -> _to_external_value(revision)")
      }

      And("the generated revision metadata is system-owned and read-only")
      val revisionparameter = plainmodel.core.parameterSequence.parameters
        .find(_.name.name == "revision")
        .getOrElse(fail("generated revision parameter missing"))
      revisionparameter.web.system shouldBe true
      revisionparameter.web.readonly shouldBe true

      And("Entity builders initialize the framework-owned revision without application input")
      _output_paths.foreach { path =>
        val source = _source(sources, path)
        source should include(
          "revision.getOrElse(org.simplemodeling.model.datatype.EntityRevision.INITIAL)"
        )
      }

      And("create, update, and query inputs do not expose revision")
      _input_paths.foreach { path =>
        val source = _source(sources, path)
        source should not include "revision: EntityRevision"
        source should not include "Update[EntityRevision]"
        source should not include "Condition[EntityRevision]"
        source should not include "\"revision\" ->"
      }

      And("the persistence codec uses the exact canonical EntityId scalar without owner rebinding")
      val entitysource = _source(sources, "/entity/Person.scala")
      entitysource should not include "EntityStoreDecodeContext"
      entitysource should not include "EntityPersistent.restoreCollectionIdentity"

      And("optional and repeated EntityId attributes use the generated store encode/decode route")
      entitysource should include("primaryFacilityId: Option[EntityId]")
      entitysource should include("relatedFacilityIds: Vector[EntityId]")
      entitysource should include(
        "\"primaryFacilityId\" -> _to_data_store_value(primaryFacilityId)"
      )
      entitysource should include(
        "\"relatedFacilityIds\" -> _to_data_store_value(relatedFacilityIds)"
      )
      entitysource should include(
        "_record_get_as_c[EntityId](record, INPUT_KEYS_PRIMARY_FACILITY_ID)"
      )
      entitysource should include(
        "_record_get_vector_as_c[org.simplemodeling.model.datatype.EntityId](record, INPUT_KEYS_RELATED_FACILITY_IDS)"
      )

      And("the generated storage decoder applies the declared scalar projection")
      entitysource should include(
        "EntityStoreRecordProjection.project(r, _store_record_attributes).flatMap(createC)"
      )
    }

    "preserve an ordinary revision attribute on non-SimpleEntity inputs" in {
      Given("an Entity whose business model defines revision without extending SimpleEntity")
      val entity = _ordinary_entity
      ScalaModelTransformer.clearObjectRegistry()
      ScalaModelTransformer.registerObject(entity)

      When("the complete Scala Entity family is generated")
      val sources = new Scala3EntityFamilyGenerator()
        .generate(entity)
        .take
        .slots
        .map(x => x.path -> x.content)
        .toMap

      Then("create retains the ordinary revision value")
      _source(sources, "/entity/create/Document.scala") should include(
        "revision: String"
      )

      And("update and query retain their ordinary revision directives")
      _source(sources, "/entity/update/Document.scala") should include(
        "revision: Update[String]"
      )
      _source(sources, "/entity/query/Document.scala") should include(
        "revision: Condition[String]"
      )

      And("the generated storage metadata identifies the direct String field")
      val entitysource = _source(sources, "/entity/Document.scala")
      entitysource should include(
        """EntityStoreAttribute.scalarString("name", "name")"""
      )
      entitysource should include(
        "EntityStoreRecordProjection.project(r, _store_record_attributes).flatMap(createC)"
      )
    }
  }

  private def _source(sources: Map[String, String], suffix: String): String =
    sources
      .collectFirst { case (path, source) if path.endsWith(suffix) => source }
      .getOrElse(fail(s"generated source missing: $suffix"))

  private def _occurrences(source: String, token: String): Int =
    source.sliding(token.length).count(_ == token)

  private def _entity: MDomainResource =
    MDomainResource(
      description = Description.name("Person"),
      affiliation = MPackageRef("example"),
      base = Some(MObjectRef.create("org.simplemodeling.model.SimpleEntity")),
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
          Designation("revision"),
          MObjectAttributeType(
            MObjectRef.create("org.simplemodeling.model.datatype.EntityRevision")
          ),
          MOne,
          Nil,
          None,
          readonly = true
        ),
        MAttribute(
          Designation("name"),
          MDataType.string,
          MOne,
          Nil,
          None
        ),
        MAttribute(
          Designation("primaryFacilityId"),
          MDataType(Designation("EntityId"), XEntityId, MPackageRef.default),
          MZeroOne,
          Nil,
          None
        ),
        MAttribute(
          Designation("relatedFacilityIds"),
          MDataType(Designation("EntityId"), XEntityId, MPackageRef.default),
          MZeroMore,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

  private def _ordinary_entity: MDomainResource =
    MDomainResource(
      description = Description.name("Document"),
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
          Designation("revision"),
          MDataType.string,
          MOne,
          Nil,
          None
        ),
        MAttribute(
          Designation("name"),
          MDataType.string,
          MOne,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

  private val _output_paths = Vector(
    "/entity/Person.scala",
    "/entity/read/Person.scala",
    "/entity/operation/Person.scala",
    "/entity/aggregate/Person.scala",
    "/entity/view/Person.scala",
    "/entity/view/summary/Person.scala",
    "/entity/view/detail/Person.scala"
  )

  private val _input_paths = Vector(
    "/entity/create/Person.scala",
    "/entity/update/Person.scala",
    "/entity/query/Person.scala"
  )
}
