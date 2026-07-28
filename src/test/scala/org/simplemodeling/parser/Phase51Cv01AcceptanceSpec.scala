package org.simplemodeling.parser

import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class Phase51Cv01AcceptanceSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _parser = SimpleModelParser(SimpleModelParser.Config.default)
  private val _source = """* Resource

** Person

#+caption: 特性一覧
| 特性 | 名前 | 型 | 多重度 | ラベル |
|------+------|----|--------|--------|
| 属性 | id | string | 1 | User ID |
"""

  "simple-modeler CV-01 scaffold identities" should {
    "register CAR compile target versus runtime range separation" in {
      Given("a valid model source and generated build metadata")
      When("the production model parser prepares the generation model")
      val model = _parser(_source)
      val root = model.root
      Then("the later metadata stage owns consistency")
      root.entities should have size 1
      cancel("CV-06 owns consistency")
    }
    "register collection identity" in {
      Given("valid model sources with generated entity identities")
      val entityname = for {
        head <- Gen.alphaUpperChar
        tail <- Gen.listOf(Gen.alphaNumChar)
      } yield (head +: tail).mkString
      val property = Prop.forAll(entityname) { name =>
        val model = _parser(_source.replace("** Person", s"** $name"))
        model.root.entities.map(_.qualifiedName).contains(name)
      }
      When("the production model parser collects model entities")
      val model = _parser(_source)
      val names = model.root.entities.map(_.qualifiedName)
      val propertyresult = Test.check(
        Test.Parameters.default.withMinSuccessfulTests(50),
        property
      )
      Then("the later identity stage owns exact identity")
      names should contain("Person")
      propertyresult.passed shouldBe true
      cancel("CI-01 owns identity")
    }
    "register persisted scalar projection" in {
      Given("a valid model source containing a scalar entity field")
      When("the production model parser prepares the persistence input")
      val model = _parser(_source)
      val person = model.root.getObject("Person")
      Then("the later persistence stage owns the projection")
      person shouldBe defined
      cancel("SP-01 owns projection")
    }
  }
}
