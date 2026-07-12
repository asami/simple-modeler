package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.goldenport.record.v2.{XRecordInstance, XString}
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ValueFamilyGenerator

/*
 * @since   Mar. 25, 2026
 *  version May. 23, 2026
 * @version Jul. 13, 2026
 * @author  ASAMI, Tomoharu
 */
class ValueScalaModelTransformerSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  "Value Scala model transformation" should {
    "generate built-in record attributes with every collection multiplicity" in {
      Given(
        "a value containing required, optional, one-or-more, and zero-or-more record attributes"
      )
      val value = MDomainValue(
        description = Description.name("RecordEnvelope"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("payload"),
            MDataType(XRecordInstance),
            MOne,
            Nil,
            None
          ),
          MAttribute(
            Designation("optionalPayload"),
            MDataType(XRecordInstance),
            MZeroOne,
            Nil,
            None
          ),
          MAttribute(
            Designation("requiredRecords"),
            MDataType(XRecordInstance),
            MOneMore,
            Nil,
            None
          ),
          MAttribute(
            Designation("records"),
            MDataType(XRecordInstance),
            MZeroMore,
            Nil,
            None
          )
        ),
        operations = Nil
      )

      When("the value model and its Scala source family are generated")
      val generated = new ValueScalaModelTransformer()(
        (value, ScalaModelTransformer.Purpose.Plain)
      ).take.head
      val generatedtypes = generated.parameterSequence.parameters
        .map(x => x.name.name -> x.typeName.fullName)
        .toMap
      val source =
        new Scala3ValueFamilyGenerator().generate(value).take.slots.head.content

      Then("each multiplicity uses its canonical Scala Record representation")
      generatedtypes("payload") shouldBe "org.goldenport.record.Record"
      generatedtypes(
        "optionalPayload"
      ) shouldBe "scala.Option[org.goldenport.record.Record]"
      generatedtypes(
        "requiredRecords"
      ) shouldBe "cats.data.NonEmptyVector[org.goldenport.record.Record]"
      generatedtypes(
        "records"
      ) shouldBe "scala.collection.immutable.Vector[org.goldenport.record.Record]"
      source should include("payload: Record")
      source should include("optionalPayload: Option[Record]")
      source should include("requiredRecords: NonEmptyVector[Record]")
      source should include("records: Vector[Record]")
      source should include(
        "case m: cats.data.NonEmptyVector[?] => m.toVector.map(_to_external_value)"
      )
      source should include(
        "case m: cats.data.NonEmptyVector[?] => m.toVector.map(_to_data_store_value)"
      )
      source should include(
        "case Some(xs) => Consequence.successOrPropertyNotFound("
      )
      source should include("NonEmptyVector.fromVector(xs))")
      source should include(
        "case None => Consequence.successOrPropertyNotFound("
      )
      source should not include (
        "NonEmptyVector.fromVector(xs).map(Some(_))"
      )
    }

    "generate a constrained case class in the value package" in {
      Given("a constrained value attribute")
      val value = MDomainValue(
        description = Description.name("value"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("code"),
            MDataType.string,
            MOne,
            List(
              new MConstraint {
                override def name: String = "min"
                override def value: Any = 1
              },
              new MConstraint {
                override def name: String = "pattern"
                override def value: Any = "^[A-Z]+$"
              }
            ),
            None
          )
        ),
        operations = Nil
      )

      When("the value model and its source family are generated")
      val tx = new ValueScalaModelTransformer()
      val result = tx((value, ScalaModelTransformer.Purpose.Plain))
      val generated = result.take.head
      val family = new Scala3ValueFamilyGenerator()
      val artifacts = family.generate(value).take
      val source = artifacts.slots.head.content

      Then(
        "the generated case class remains in the value package with its constraints"
      )
      generated.packageName.name shouldBe "domain.value"
      generated.className.name shouldBe "value"
      generated.declaration shouldBe ClassDeclaration.CaseClass
      generated.parameterSequence.parameters.map(_.name.name) should contain(
        "code"
      )
      generated.attributeSequence.attributes.map(_.name.name) should contain(
        "code"
      )
      generated.parameterSequence.parameters.head.typeName.fullName shouldBe "String"
      generated.parameterSequence.parameters.head.constraints.map(c =>
        s"${c.name}=${c.literal}"
      ) should contain allOf ("min=1", "pattern=^[A-Z]+$")
      source should include("private def validate(): Unit = {")
      source should include(
        """require(BigDecimal(code.toString) >= BigDecimal("1"), "code must be >= 1")"""
      )
      source should include(
        """require(code == null || code.toString.matches("^[A-Z]+$"), "code must match ^[A-Z]+$")"""
      )
    }

    "resolve short entity references to the generated entity package" in {
      Given("an entity and a value containing a short repeated reference to it")
      val account = MDomainResource(
        description = Description.name("Account"),
        affiliation = MPackageRef("org.example"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(Designation("name"), MDataType.string, MOne, Nil, None)
        ),
        associations = Nil,
        operations = Nil,
        stateMachines = Nil
      )
      val snapshot = MDomainValue(
        description = Description.name("Snapshot"),
        affiliation = MPackageRef("org.example.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("accounts"),
            MDataType(Designation("Account"), XString, MPackageRef.default),
            MZeroMore,
            Nil,
            None
          )
        ),
        operations = Nil
      )

      When("the registered model is transformed")
      ScalaModelTransformer.clearObjectRegistry()
      ScalaModelTransformer.registerObject(account)
      ScalaModelTransformer.registerObject(snapshot)

      val tx = new ValueScalaModelTransformer()
      val generated =
        tx((snapshot, ScalaModelTransformer.Purpose.Plain)).take.head

      Then("the short reference resolves to the generated entity package")
      generated.parameterSequence.parameters.head.typeName.fullName shouldBe "scala.collection.immutable.Vector[org.example.entity.Account]"
    }

    "generate scalar datastore representation for a single-field datatype-backed value" in {
      Given("a single-field value")
      val value = MDomainValue(
        description = Description.name("ExhibitionDate"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(Designation("value"), MDataType.string, MOne, Nil, None)
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val family = new Scala3ValueFamilyGenerator()
      val source = family.generate(value).take.slots.head.content

      Then("the API remains record-shaped while datastore storage is scalar")
      source should include("def toRecord(): Record")
      source should include("\"value\" -> _to_external_value(value)")
      source should include("def toDataStore(): String")
      source should include("value")
      source should not include ("def toDataStore(): Record")
    }

    "keep structured datastore representation for multi-field values" in {
      Given("a value with multiple fields")
      val value = MDomainValue(
        description = Description.name("DisplayPeriod"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(Designation("start"), MDataType.string, MOne, Nil, None),
          MAttribute(Designation("end"), MDataType.string, MOne, Nil, None)
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val family = new Scala3ValueFamilyGenerator()
      val source = family.generate(value).take.slots.head.content

      Then("datastore storage remains record-shaped")
      source should include("def toDataStore(): Record")
      source should include("Record.dataAuto(")
      source should include("\"start\" -> _to_data_store_value(start)")
      source should include("\"end\" -> _to_data_store_value(end)")
    }

    "generate structured datatype classes in the datatype package" in {
      Given("a structured datatype")
      val datatype = MStructuredDataType(
        description = Description.name("DisplayPeriod"),
        affiliation = MPackageRef("domain.datatype"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(Designation("start"), MDataType.string, MOne, Nil, None),
          MAttribute(Designation("end"), MDataType.string, MOne, Nil, None)
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val family = new Scala3ValueFamilyGenerator()
      val artifacts = family.generate(datatype).take
      val source = artifacts.slots.head.content

      Then(
        "the generated class and persistence contract remain in the datatype package"
      )
      artifacts.slots.head.path should include(
        "domain/datatype/DisplayPeriod.scala"
      )
      source should include("package domain.datatype")
      source should include("case class DisplayPeriod(")
      source should include("start: String")
      source should include("end: String")
      source should include("def toRecord(): Record")
      source should include("\"start\" -> _to_external_value(start)")
      source should include("\"end\" -> _to_external_value(end)")
      source should include("def toDataStore(): Record")
      source should include("\"start\" -> _to_data_store_value(start)")
      source should include("\"end\" -> _to_data_store_value(end)")
    }
  }
}
