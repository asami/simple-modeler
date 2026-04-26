package org.simplemodeling.SimpleModeler.generator.scala.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.goldenport.record.v2.{XAge, XDateTime, XString}

/*
 * @since   Apr. 26, 2026
 * @version Apr. 26, 2026
 * @author  ASAMI, Tomoharu
 */
class ScalaModelStorageScalarSpec extends AnyFunSuite with Matchers {
  test("map supported date-time scalar to an explicit Java time type") {
    TypeName.create(XDateTime).fullName shouldBe "java.time.ZonedDateTime"
  }

  test("reject unsupported scalar marshalling instead of falling back to String") {
    TypeName.Primitive.createMarshalling(XString) shouldBe TypeName.Primitive.string
    TypeName.Primitive.createMarshalling(XAge) shouldBe TypeName.Primitive.int

    val thrown = intercept[Throwable] {
      TypeName.Primitive.createMarshalling(XDateTime)
    }
    thrown.getMessage should include ("Unsupported scalar marshalling datatype: dateTime")
  }
}
