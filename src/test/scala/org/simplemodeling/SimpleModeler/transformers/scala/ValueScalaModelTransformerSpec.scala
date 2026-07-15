package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.goldenport.record.v2.{XInt, XRecordInstance, XString}
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ValueFamilyGenerator

/*
 * @since   Mar. 25, 2026
 *  version May. 23, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
final class ValueScalaModelTransformerSpec
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
          ),
          MAttribute(
            Designation("tags"),
            MDataType.string,
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
      generatedtypes("tags") shouldBe "scala.collection.immutable.Vector[String]"
      source should include("payload: Record")
      source should include("optionalPayload: Option[Record]")
      source should include("requiredRecords: NonEmptyVector[Record]")
      source should include("records: Vector[Record]")
      source should include("tags: Vector[String]")
      source should include(
        "_record_get_record(record, INPUT_KEYS_OPTIONAL_PAYLOAD).map(_ orElse optionalPayload)"
      )
      source should include(
        "_record_get_vector_of_record_c(record, INPUT_KEYS_RECORDS)((r: Record) => Consequence.success(r))"
      )
      source should include(
        "_record_get_vector_as_c[String](record, INPUT_KEYS_TAGS)"
      )
      source should include(
        "case m: cats.data.NonEmptyVector[?] => m.toVector.map(_to_external_value)"
      )
      source should include(
        "case m: cats.data.NonEmptyVector[?] => m.toVector.map(_to_data_store_value)"
      )
      source should include(
        "case Some(xs) => Consequence.successOrPropertyNotFound("
      )
      source should include(
        "_record_get_vector_of_record_c(record, INPUT_KEYS_REQUIRED_RECORDS)((r: Record) => Consequence.success(r))"
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
                override def name: String = "min_length"
                override def value: Any = 2
              },
              new MConstraint {
                override def name: String = "max_length"
                override def value: Any = 12
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
      ) should contain allOf (
        "min_length=2",
        "max_length=12",
        "pattern=^[A-Z]+$"
      )
      source should include("private def validate(): Unit = {")
      source should include(
        """require(_text_constraint_values(code).forall(_.length >= 2), "code must have length >= 2")"""
      )
      source should include(
        """require(_text_constraint_values(code).forall(_.length <= 12), "code must have length <= 12")"""
      )
      source should include(
        "case x: org.goldenport.datatype.I18nTitle => x.toI18nString.entries.toVector.map(_._2)"
      )
      source should include(
        "case x: org.goldenport.value.ContentBody => Vector(x.value)"
      )
      source should include(
        """require(_text_constraint_values(code).forall(_.matches("^[A-Z]+$")), "code must match ^[A-Z]+$")"""
      )
    }

    "generate numeric range validation only for numeric values" in {
      Given("an integer value constrained by a numeric range")
      val value = MDomainValue(
        description = Description.name("Quantity"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("value"),
            MDataType(XInt),
            MOne,
            List(_constraint("min", 1), _constraint("max", 10)),
            None
          )
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val source = new Scala3ValueFamilyGenerator().generate(value).take.slots.head.content

      Then("the generated validation compares numeric values and never treats them as text")
      source should include(
        """require(BigDecimal(value.toString) >= BigDecimal("1"), "value must be >= 1")"""
      )
      source should include(
        """require(BigDecimal(value.toString) <= BigDecimal("10"), "value must be <= 10")"""
      )
      source should not include "_text_constraint_values(value)"
    }

    "escape predefined format regular expressions as Scala string literals" in {
      Given("email and E.164 phone attributes with canonical format constraints")
      val value = MDomainValue(
        description = Description.name("Contact"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("email"),
            MDataType(XString),
            MOne,
            List(_constraint("format", "email")),
            None
          ),
          MAttribute(
            Designation("phone"),
            MDataType(XString),
            MOne,
            List(_constraint("format", "phone")),
            None
          )
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val source = new Scala3ValueFamilyGenerator().generate(value).take.slots.head.content

      Then("regex escapes remain valid in the generated Scala source")
      source should include(
        """_.matches("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$")"""
      )
      source should include(
        """_.matches("^\\+?[1-9]\\d{6,14}$")"""
      )
    }

    "reject numeric range constraints on text values" in {
      Given("a string value incorrectly constrained with numeric min")
      val value = MDomainValue(
        description = Description.name("InvalidTextRange"),
        affiliation = MPackageRef("domain.value"),
        stereotypes = Nil,
        base = None,
        traits = Nil,
        powertypes = Nil,
        attributes = List(
          MAttribute(
            Designation("value"),
            MDataType.string,
            MOne,
            List(_constraint("min", 1)),
            None
          )
        ),
        operations = Nil
      )

      When("the Scala source family is generated")
      val failure = the[RuntimeException] thrownBy {
        new Scala3ValueFamilyGenerator().generate(value).take
      }

      Then("generation explains that text requires length constraints")
      failure.getMessage should include("use min-length/max-length for text")
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

    "generate a constrained nominal scalar for a plain datatype" in {
      Given("a named string datatype with canonical length and pattern constraints")
      val datatype = MNominalDataType(
        description = Description.name("LoginName"),
        affiliation = MPackageRef("domain.datatype"),
        datatype = XString,
        constraints = List(
          _constraint("min_length", 3),
          _constraint("max_length", 64),
          _constraint("pattern", "^[A-Za-z0-9._-]+$")
        )
      )

      When("the nominal scalar source is generated")
      val artifacts = new Scala3ValueFamilyGenerator().generate(datatype).take
      val source = artifacts.slots.head.content

      Then("the generated contract keeps nominal identity over a scalar wire value")
      artifacts.slots.head.path should include("domain/datatype/LoginName.scala")
      source should include("case class LoginName(")
      source should include("value: String")
      source should include("def toDataStore(): String")
      source should include("given org.goldenport.convert.ValueReader[LoginName]")
      source should include("case other => summon[org.goldenport.convert.ValueReader[String]].readC(other)")
      source should include("given Codec[LoginName] = Codec.from(")
      source should include("summon[io.circe.Encoder[String]].contramap(_.value)")
      source should include("value must have length >= 3")
      source should include("value must have length <= 64")
      source should include("value must match ^[A-Za-z0-9._-]+$")
      source should not include("derives Codec.AsObject")
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

  private def _constraint(constraintName: String, constraintValue: Any): MConstraint =
    new MConstraint {
      override def name: String = constraintName
      override def value: Any = constraintValue
    }
}
