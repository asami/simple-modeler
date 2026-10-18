package org.simplemodeling.SimpleModeler.generator.scala

import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model.{MPackageRef, MPowertype, MPowertypeKind}
import org.simplemodeling.SimpleModeler.generators.scala.Scala3PowertypeFamilyGenerator
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala.PowertypeScalaModelTransformer

/*
 * @since   Jul. 27, 2026
 * @version Oct. 19, 2026
 * @author  ASAMI, Tomoharu
 */
final class PowertypeConsequenceGenerationSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  "Powertype record decoding" should {
    "use the semantic record-not-found utility" in {
      Given("a powertype whose generated decoder requires the value field")
      val powertype = MPowertype(
        Description.name("RunMode"),
        MPackageRef("example"),
        List(MPowertypeKind("Active", Some("1")))
      )

      When("the Scala powertype family is generated")
      val source = new Scala3PowertypeFamilyGenerator()
        .generate(powertype)
        .take
        .slots
        .map(_.content)
        .mkString("\n")

      Then("a missing value is represented by the current structured Consequence API")
      source should include("""Consequence.recordNotFound("value", record)""")

      And("the deprecated compatibility API is not emitted")
      source should not include "Consequence.failRecordNotFound"
    }

    "emit the typed numeric correspondence without changing the external symbolic codec" in {
      Given("a finite vocabulary with explicitly assigned signed Int boundary codes")
      val powertype = _powertype(List(
        MPowertypeKind("prepared", Some(Int.MinValue.toString)),
        MPowertypeKind("unprepared", Some(Int.MaxValue.toString))))

      When("the normal POWERTYPE family is generated")
      val source = _generate(powertype)

      Then("the companion delegates bidirectional numeric conversion to Core using its fixed maps")
      source should include("given numericCodeCodec: org.goldenport.convert.NumericCodeCodec[ProposalPreparationState] = org.goldenport.convert.NumericCodeCodec.instance(value => dbValueOf(value.value), fromDbValue)")
      source should include(s"${Int.MinValue} -> prepared")
      source should include(s"${Int.MaxValue} -> unprepared")
      source should include("def fromDbValue(value: Int): Option[ProposalPreparationState] = _by_db_value.get(value)")
      And("symbolic values and default external Circe derivation retain their existing form")
      source should include("""ProposalPreparationState("prepared")""")
      source should include("""ProposalPreparationState("unprepared")""")
      source should include("derives Codec.AsObject")
      source should not include "given Codec[ProposalPreparationState] ="
    }

    "retain explicit code correspondence across kind reorderings and label changes" in {
      Given("arbitrary fixed signed Int codes for two symbolic kinds")
      val codes = Gen.choose(Int.MinValue, Int.MaxValue)

      When("the normal producer receives both source orders and changed display labels")
      val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(25), Prop.forAll(codes) { code =>
        val othercode = code ^ 1
        val kinds = List(MPowertypeKind("prepared", Some(code.toString), Some("Prepared")),
          MPowertypeKind("unprepared", Some(othercode.toString), Some("Unprepared")))
        val original = _generate(_powertype(kinds))
        val reordered = _generate(_powertype(kinds.reverse.map(_.copy(labelText = Some("Changed label")))))
        Vector(original, reordered).forall { source =>
          source.contains(s"$code -> prepared") && source.contains(s"$othercode -> unprepared") &&
            source.contains("""ProposalPreparationState("prepared")""") &&
            source.contains("""ProposalPreparationState("unprepared")""")
        }
      })

      Then("only each explicit numeric assignment determines the generated mapping")
      result.passed shouldBe true
    }

    "preserve omitted standalone codes without inventing numeric assignments" in {
      Given("a standalone transient vocabulary with no explicit persistence codes")
      val powertype = _powertype(List(MPowertypeKind("prepared", None), MPowertypeKind("unprepared", None)))

      When("the semantic transformer and normal producer receive that standalone vocabulary")
      val transformed = new PowertypeScalaModelTransformer().apply((powertype, Purpose.Plain)).take.head
      val source = _generate(powertype)

      Then("both kinds keep None and the inverse numeric map has no entries")
      transformed.directive.enumerationValues.map(_.dbValue) shouldBe Vector(None, None)
      source should include("private val _by_db_value: Map[Int, ProposalPreparationState] = Vector().toMap")
      source should include("""ProposalPreparationState("prepared")""")
    }

    "reject invalid or duplicate explicit codes even for standalone vocabularies" in {
      Given("malformed, fractional, nondecimal, null and overflowing explicit codes, plus numerically duplicate assignments")
      val malformed = Vector("", "prepared", "1.0", "0x10", " 1 ", "2147483648", "-2147483649", null)
      val kinds = malformed.map(value => List(MPowertypeKind("prepared", Some(value)))) :+
        List(MPowertypeKind("prepared", Some("1")), MPowertypeKind("unprepared", Some("+01")))

      When("the normal standalone producer consumes each declaration")
      val failures = kinds.map(values => intercept[Exception] { _generate(_powertype(values)) })

      Then("invalid explicit input is diagnosed with its owning type and kind rather than becoming omission")
      failures.foreach { failure =>
        failure.getMessage should include("ProposalPreparationState")
        failure.getMessage should include("prepared")
      }
    }

  }

  private def _powertype(kinds: List[MPowertypeKind]): MPowertype = {
    MPowertype(Description.name("ProposalPreparationState"), MPackageRef("example"), kinds)
  }

  private def _generate(powertype: MPowertype): String = {
    val context = ScalaModel.Context.fromObjects(Vector(powertype))
    new Scala3PowertypeFamilyGenerator(context).generate(powertype).take.slots.map(_.content).mkString("\n")
  }
}
