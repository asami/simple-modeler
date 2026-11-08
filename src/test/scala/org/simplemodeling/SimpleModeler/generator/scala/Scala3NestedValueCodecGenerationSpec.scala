package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.values.Designation
import org.goldenport.record.v2.XRecordInstance
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainValue
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ValueFamilyGenerator

final class Scala3NestedValueCodecGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _package = "org.simplemodeling.generatedcodec.value"
  private def _value(name: String, attributes: List[MAttribute]): MDomainValue =
    MDomainValue(Description.name(name), MPackageRef(_package), Nil, None, Nil, Nil, attributes, Nil)
  private def _field(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute =
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)
  private def _ref(name: String): MAttributeType = MObjectAttributeType(MObjectRef.create(name))
  private def _source(value: MDomainValue, values: Vector[MDomainValue]): String =
    new Scala3ValueFamilyGenerator(ScalaModel.Context.fromObjects(values)).generate(value).take.slots.map(_.content).mkString("\n")

  "Nested value codec generation" should {
    "derive codecs for the actual acyclic family and supported multiplicities" in {
      Given("a leaf, branch and envelope containing optional, ordered and nonempty products")
      val values = NestedValueCodecFixtureGenerator.values
      When("the whole family is generated with its declaration context")
      val sources = values.map(value => _source(value, values))
      Then("every generated product declares an object codec with typed nested fields")
      sources.foreach(_ should include("derives Codec.AsObject"))
      sources.last should include("Option[Branch]")
      sources.last should include("Vector[Branch]")
      sources.last should include("NonEmptyVector[Leaf]")
    }

    "admit varied acyclic declared names consistently" in {
      Given("a family of differently named acyclic products")
      val property = org.scalacheck.Prop.forAll(org.scalacheck.Gen.choose(1, 10000)) { number =>
        val leaf = _value("Leaf" + number, List(_field("text", MDataType.string, MOne)))
        val parent = _value("Parent" + number, List(_field("item", _ref(leaf.qualifiedName), MZeroMore)))
        _source(parent, Vector(leaf, parent)).contains("derives Codec.AsObject")
      }
      When("each family is generated")
      val result = org.scalacheck.Test.check(org.scalacheck.Test.Parameters.default.withMinSuccessfulTests(30), property)
      Then("admission follows the declaration graph")
      result.passed shouldBe true
    }

    "reject transitive products whose generation uses a different package base" in {
      Given("A and C in one generated package and B in a different generated package")
      val otherpackage = "org.simplemodeling.generatedcodec.other.value"
      val leaf = _value("CrossLeaf", List(_field("text", MDataType.string, MOne)))
      val branch = MDomainValue(Description.name("CrossBranch"), MPackageRef(otherpackage),
        Nil, None, Nil, Nil, List(_field("leaf", _ref(leaf.qualifiedName), MOne)), Nil)
      val parent = _value("CrossParent", List(_field("branch", _ref(branch.qualifiedName), MOne)))
      val values = Vector(leaf, branch, parent)
      When("each product is generated in the same declaration context")
      val sources = values.map(value => _source(value, values))
      Then("the leaf derives a codec while both dependent products decline derivation")
      sources.head should include("derives Codec.AsObject")
      sources(1) should not include "derives Codec.AsObject"
      sources(2) should not include "derives Codec.AsObject"
    }

    "retain exclusions for cycles, missing declarations and unsupported nested fields" in {
      Given("recursive, unknown and Record-bearing value graphs")
      val cycle = _value("Cycle", List(_field("next", _ref(_package + ".Cycle"), MZeroOne)))
      val missing = _value("Missing", List(_field("item", _ref(_package + ".Unknown"), MOne)))
      val record = _value("RecordLeaf", List(_field("payload", MDataType(XRecordInstance), MOne)))
      val parent = _value("Parent", List(_field("item", _ref(_package + ".RecordLeaf"), MOne)))
      val first = _value("First", List(_field("other", _ref(_package + ".Second"), MOne)))
      val second = _value("Second", List(_field("other", _ref(_package + ".First"), MZeroOne)))
      val external = _value("External", List(_field("item", _ref("org.example.external.Thing"), MOne)))
      val entity = _value("EntityHolder", List(_field("item", _ref("org.simplemodeling.generatedcodec.entity.Thing"), MOne)))
      val values = Vector(cycle, missing, record, parent, first, second, external, entity)
      When("those values are generated")
      val sources = values.map(value => _source(value, values))
      Then("no unsupported graph is given a derived codec")
      sources.foreach(_ should not include "derives Codec.AsObject")
    }
  }
}
