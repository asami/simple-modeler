package org.simplemodeling.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Jan.  6, 2020
 * @version Jan.  6, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  val parser = SimpleModelParser(SimpleModelParser.Config.default)

  "SimpleModelParser" should {
    "empty" in {
      val src = """
"""
      val model = parser.apply(src)
    }
  }
}
