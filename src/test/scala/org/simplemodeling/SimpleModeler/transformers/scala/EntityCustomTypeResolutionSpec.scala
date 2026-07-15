package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.goldenport.record.v2.XString
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model.{PackageName, TypeName}
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.generators.scala.Scala3EntityFamilyGenerator

/*
 * @since   Apr.  9, 2026
 *  version May. 23, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
final class EntityCustomTypeResolutionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Entity custom type resolution" should {
    "avoid deriving a query codec through a shared model value without a codec contract" in {
      Given("an entity query containing a shared SimpleModeling value")
      val identitypresentation = MDomainValue(
        description = Description.name("IdentityPresentation"),
        affiliation = MPackageRef("org.simplemodeling.model.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(Designation("displayName"), MDataType.string, MOne, Nil, None)
        ),
        operations = Nil
      )
      val userprofile = MDomainResource(
        description = Description.name("UserProfile"),
        affiliation = MPackageRef("org.example.account"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("identityPresentation"),
            MDataType(Designation("IdentityPresentation"), XString, MPackageRef("org.simplemodeling.model.value")),
            MZeroOne,
            Nil,
            None
          )
        ),
        associations = Nil,
        operations = Nil,
        stateMachines = Nil
      )

      ScalaModelTransformer.clearObjectRegistry()
      ScalaModelTransformer.registerObject(identitypresentation)
      ScalaModelTransformer.registerObject(userprofile)

      When("the Scala entity family is generated")
      val artifacts = new Scala3EntityFamilyGenerator().generate(userprofile).take
      val query = artifacts.slots
        .find(_.path.contains("entity/query/UserProfile.scala"))
        .getOrElse(fail("query source missing"))
        .content

      Then("the query uses the structured Condition type without assuming a nested Circe codec")
      query should include("identityPresentation: Condition[IdentityPresentation]")
      query should not include "derives Codec.AsObject"
    }

    "resolve simplemodeling-model value types as generated Scala types" in {
    Given("an entity attribute referencing a registered SimpleModeling value type")
    val address = MDomainValue(
      description = Description.name("Address"),
      affiliation = MPackageRef("org.simplemodeling.model.value"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("value"), MDataType.string, MOne, Nil, None)
      ),
      operations = Nil
    )
    val userprofile = MDomainResource(
      description = Description.name("UserProfile"),
      affiliation = MPackageRef("org.example.account"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("address"), MDataType(Designation("Address"), XString, MPackageRef.default), MZeroOne, Nil, None)
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

    ScalaModelTransformer.clearObjectRegistry()
    ScalaModelTransformer.registerObject(userprofile)

    When("the entity read model is generated")
    val tx = new EntityValueReadScalaModelTransformer()
    val result = tx((userprofile, ScalaModelTransformer.Purpose.Read))
    val generated = result.take.head
    val addressparam = generated.parameterSequence.parameters.find(_.name.name == "address").getOrElse(fail("address parameter missing"))

    Then("the generated parameter uses the registered value class")
    addressparam.typeName shouldBe TypeName.option(TypeName.Plain(PackageName("org.simplemodeling.model.value"), "Address"))
  }

    "resolve custom value types from another component package" in {
    Given("an entity attribute referencing a registered external component value")
    val tenantcode = MDomainValue(
      description = Description.name("TenantCode"),
      affiliation = MPackageRef("org.example.shared.value"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("value"), MDataType.string, MOne, Nil, None)
      ),
      operations = Nil
    )
    val account = MDomainResource(
      description = Description.name("Account"),
      affiliation = MPackageRef("org.example.account"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("tenantCode"), MDataType(Designation("TenantCode"), XString, MPackageRef.default), MOne, Nil, None)
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

    ScalaModelTransformer.clearObjectRegistry()
    ScalaModelTransformer.registerObject(tenantcode)
    ScalaModelTransformer.registerObject(account)

    When("the entity read model is generated")
    val tx = new EntityValueReadScalaModelTransformer()
    val result = tx((account, ScalaModelTransformer.Purpose.Read))
    val generated = result.take.head
    val tenantcodeparam = generated.parameterSequence.parameters.find(_.name.name == "tenantCode").getOrElse(fail("tenantCode parameter missing"))

    Then("the generated parameter keeps the external component package")
    tenantcodeparam.typeName shouldBe TypeName.Plain(PackageName("org.example.shared.value"), "TenantCode")
  }

    "generate external collection record readers through ValueReader" in {
    Given("an entity with a repeated external value attribute")
    val externalref = MDomainValue(
      description = Description.name("ExternalRef"),
      affiliation = MPackageRef("org.example.external"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("value"), MDataType.string, MOne, Nil, None)
      ),
      operations = Nil
    )
    val account = MDomainResource(
      description = Description.name("Account"),
      affiliation = MPackageRef("org.example.account"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(
          Designation("externalRefs"),
          MDataType(Designation("ExternalRef"), XString, MPackageRef("org.example.external")),
          MZeroMore,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

    ScalaModelTransformer.clearObjectRegistry()
    ScalaModelTransformer.registerObject(externalref)
    ScalaModelTransformer.registerObject(account)

    When("the Scala entity family is generated")
    val family = new Scala3EntityFamilyGenerator()
    val artifacts = family.generate(account).take
    val source = artifacts.slots.map(_.content).mkString("\n")
    val readerstart = source.indexOf("private def _record_get_vector_as_c")
    val readerend = source.indexOf("private def _record_get_vector_of_record_c", readerstart)
    val requestreader = source.substring(readerstart, readerend)

    Then("the generated decoder delegates each supplied value to ValueReader without inventing comma syntax")
    source should include("_record_get_vector_as_c[org.example.external.ExternalRef]")
    requestreader should not include "s.split(\",\""
    source should not include "org.example.external.ExternalRef.createC"

    And("external collection values do not gain an implicit datastore migration syntax")
    source should not include "private def _normalize_store_record_collections"
  }

    "scalarize nested generated values inside update directives" in {
    Given("an entity with a repeated generated value attribute")
    val tag = MDomainValue(
      description = Description.name("Tag"),
      affiliation = MPackageRef("org.example.account.value"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("value"), MDataType.string, MOne, Nil, None)
      ),
      operations = Nil
    )
    val account = MDomainResource(
      description = Description.name("Account"),
      affiliation = MPackageRef("org.example.account"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(
          Designation("tags"),
          MDataType(Designation("Tag"), XString, MPackageRef("org.example.account.value")),
          MZeroMore,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

    ScalaModelTransformer.clearObjectRegistry()
    ScalaModelTransformer.registerObject(tag)
    ScalaModelTransformer.registerObject(account)

    When("the Scala entity family is generated")
    val family = new Scala3EntityFamilyGenerator()
    val artifacts = family.generate(account).take
    val source = artifacts.slots.map(_.content).mkString("\n")

    Then("the update datastore projection recursively scalarizes the repeated values")
    source should include("case _: org.simplemodeling.model.directive.Update.Noop.type => org.simplemodeling.model.directive.Update.Noop")
    source should include("case _: org.simplemodeling.model.directive.Update.SetNull.type => org.simplemodeling.model.directive.Update.SetNull")
    source should include("case org.simplemodeling.model.directive.Update.SetValue(value) => org.simplemodeling.model.directive.Update.SetValue(_to_data_store_value(value))")
    source should include("case m: org.example.account.value.Tag => m.toDataStore()")
    source should include("private def _normalize_store_record_collections")
    source should include("s.split(\",\")")
    source should include("z.upsertSingle(key, values)")
    source should include("createC(r).orElse(createC(_normalize_store_record_collections(r)))")
  }

    "generate simplemodeling datatype collection record readers through ValueReader" in {
    Given("an entity with a repeated SimpleModeling datatype attribute")
    val account = MDomainResource(
      description = Description.name("Account"),
      affiliation = MPackageRef("org.example.account"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(
          Designation("entityIds"),
          MDataType(Designation("EntityId"), XString, MPackageRef("org.simplemodeling.model.datatype")),
          MZeroMore,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )

    ScalaModelTransformer.clearObjectRegistry()
    ScalaModelTransformer.registerObject(account)

    When("the Scala entity family is generated")
    val family = new Scala3EntityFamilyGenerator()
    val artifacts = family.generate(account).take
    val source = artifacts.slots.map(_.content).mkString("\n")

    Then("the generated decoder uses the canonical SimpleModeling value type")
    source should include("_record_get_vector_as_c[org.simplemodeling.model.value.EntityId]")
    source should not include "_record_get_as_c[Vector[EntityId]]"
  }
  }
}
