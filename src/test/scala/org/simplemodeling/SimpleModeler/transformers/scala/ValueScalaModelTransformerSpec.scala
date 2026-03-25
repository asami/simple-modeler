package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainValue
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ValueFamilyGenerator

/*
 * @since   Mar. 25, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class ValueScalaModelTransformerSpec extends AnyFunSuite with Matchers {
  test("generate a case class in the value package") {
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

    val tx = new ValueScalaModelTransformer()
    val result = tx((value, ScalaModelTransformer.Purpose.Plain))
    val generated = result.take.head

    generated.packageName.name shouldBe "domain.value"
    generated.className.name shouldBe "value"
    generated.declaration shouldBe ClassDeclaration.CaseClass
    generated.parameterSequence.parameters.map(_.name.name) should contain("code")
    generated.attributeSequence.attributes.map(_.name.name) should contain("code")
    generated.parameterSequence.parameters.head.typeName.fullName shouldBe "String"
    generated.parameterSequence.parameters.head.constraints.map(c => s"${c.name}=${c.literal}") should contain allOf ("min=1", "pattern=^[A-Z]+$")

    val family = new Scala3ValueFamilyGenerator()
    val artifacts = family.generate(value).take
    val source = artifacts.slots.head.content
    source should include("private def validate(): Unit = {")
    source should include("""require(BigDecimal(code.toString) >= BigDecimal("1"), "code must be >= 1")""")
    source should include("""require(code == null || code.toString.matches("^[A-Z]+$"), "code must match ^[A-Z]+$")""")
  }
}
