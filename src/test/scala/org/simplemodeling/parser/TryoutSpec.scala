package org.simplemodeling.parser

import org.scalatest.funsuite.AnyFunSuite

/*
 * @since   Jan.  6, 2020
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class TryoutSpec extends AnyFunSuite {
  val parser = SimpleModelParser(SimpleModelParser.Config.default)

  test("empty") {
    val src = """
"""
    val model = parser.apply(src)
  }
}
