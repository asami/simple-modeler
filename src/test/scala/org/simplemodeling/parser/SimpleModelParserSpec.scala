package org.simplemodeling.parser

import org.scalatest.funsuite.AnyFunSuite

/*
 * @since   Nov.  2, 2019
 *  version Nov.  4, 2019
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class SimpleModelParserSpec extends AnyFunSuite {
  val parser = SimpleModelParser(SimpleModelParser.Config.default)

  test("empty") {
    val src = """
"""
    val model = parser.apply(src)
  }
  test("simple") {
    val src = """* Resource

** Person

#+caption: 特性一覧
| 特性 | 名前                           | 型     | 多重度 | ラベル               |
|------+--------------------------------+--------+--------+----------------------|
| 属性 | id                             | string | 1      | User ID              |
| 属性 | name                           | string | 1      | 名前                 |
"""
    val model = parser.apply(src)
    println(model)
  }
  test("association") {
    val src = """* Resource

** Person

#+caption: 特性一覧
| 特性 | 名前                           | 型     | 多重度 | ラベル               |
|------+--------------------------------+--------+--------+----------------------|
| 属性 | id                             | string | 1      | User ID              |
| 属性 | name                           | string | 1      | 名前                 |
| 関連 | company                        | Company | 1      | 会社                 |
"""
    val model = parser.apply(src)
    println(model)
  }
}
