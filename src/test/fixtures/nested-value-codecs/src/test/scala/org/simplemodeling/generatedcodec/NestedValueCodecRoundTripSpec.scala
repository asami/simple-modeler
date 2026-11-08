package org.simplemodeling.generatedcodec

import cats.data.NonEmptyVector
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.generatedcodec.value.{Branch, Envelope, Leaf}

final class NestedValueCodecRoundTripSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Generated nested value codecs" should {
    "encode and decode optional and ordered products exactly" in {
      Given("generated products with optional, ordered and nonempty nested values")
      val value = Envelope(Some(Branch(Leaf("first"))),
        Vector(Branch(Leaf("second")), Branch(Leaf("third"))),
        NonEmptyVector.of(Leaf("fourth"), Leaf("fifth")))
      val json = """{"optional":{"leaf":{"text":"first"}},"ordered":[{"leaf":{"text":"second"}},{"leaf":{"text":"third"}}],"nonempty":[{"text":"fourth"},{"text":"fifth"}]}"""
      When("the generated Encoder and Decoder are applied")
      val encoded = value.asJson.noSpaces
      Then("the exact field values and collection order survive")
      encoded shouldBe json
      decode[Envelope](json) shouldBe Right(value)
    }

    "roundtrip varied text, optionality and ordering" in {
      Given("generated nested values with varied ordered text")
      val texts = Gen.listOf(Gen.alphaStr)
      val property = Prop.forAll(texts, Gen.option(Gen.alphaStr), Gen.alphaStr) { (items, optional, head) =>
        val value = Envelope(optional.map(x => Branch(Leaf(x))),
          items.toVector.map(x => Branch(Leaf(x))), NonEmptyVector.one(Leaf(head)))
        decode[Envelope](value.asJson.noSpaces) == Right(value)
      }
      When("the generated codecs process those values")
      val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(100), property)
      Then("all generated cases roundtrip")
      result.passed shouldBe true
    }
  }
}
