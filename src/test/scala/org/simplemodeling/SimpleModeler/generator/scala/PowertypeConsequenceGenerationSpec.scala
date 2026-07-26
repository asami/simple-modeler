package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model.{MPackageRef, MPowertype, MPowertypeKind}
import org.simplemodeling.SimpleModeler.generators.scala.Scala3PowertypeFamilyGenerator

/*
 * @since   Jul. 27, 2026
 * @version Jul. 27, 2026
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
  }
}
