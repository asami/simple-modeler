package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
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
 * @version May. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityCustomTypeResolutionSpec extends AnyFunSuite with Matchers {
  test("resolve simplemodeling-model value types as generated Scala types") {
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
    val userProfile = MDomainResource(
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
    ScalaModelTransformer.registerObject(userProfile)

    val tx = new EntityValueReadScalaModelTransformer()
    val result = tx((userProfile, ScalaModelTransformer.Purpose.Read))
    val generated = result.take.head
    val addressParam = generated.parameterSequence.parameters.find(_.name.name == "address").getOrElse(fail("address parameter missing"))

    addressParam.typeName shouldBe TypeName.option(TypeName.Plain(PackageName("org.simplemodeling.model.value"), "Address"))
  }

  test("resolve custom value types from another component package") {
    val tenantCode = MDomainValue(
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
    ScalaModelTransformer.registerObject(tenantCode)
    ScalaModelTransformer.registerObject(account)

    val tx = new EntityValueReadScalaModelTransformer()
    val result = tx((account, ScalaModelTransformer.Purpose.Read))
    val generated = result.take.head
    val tenantCodeParam = generated.parameterSequence.parameters.find(_.name.name == "tenantCode").getOrElse(fail("tenantCode parameter missing"))

    tenantCodeParam.typeName shouldBe TypeName.Plain(PackageName("org.example.shared.value"), "TenantCode")
  }

  test("generate external collection record readers through ValueReader") {
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

    val family = new Scala3EntityFamilyGenerator()
    val artifacts = family.generate(account).take
    val source = artifacts.slots.map(_.content).mkString("\n")

    source should include("_record_get_vector_as_c[org.example.external.ExternalRef]")
    source should not include "org.example.external.ExternalRef.createC"
  }

  test("generate simplemodeling datatype collection record readers through ValueReader") {
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

    val family = new Scala3EntityFamilyGenerator()
    val artifacts = family.generate(account).take
    val source = artifacts.slots.map(_.content).mkString("\n")

    source should include("_record_get_vector_as_c[org.simplemodeling.model.value.EntityId]")
    source should not include "_record_get_as_c[Vector[EntityId]]"
  }
}
