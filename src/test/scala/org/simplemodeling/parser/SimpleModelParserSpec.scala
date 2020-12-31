package org.simplemodeling.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Nov.  2, 2019
 * @version Nov.  4, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SimpleModelParserSpec extends WordSpec with Matchers with GivenWhenThen {
  val parser = SimpleModelParser(SimpleModelParser.Config.default)

  "SimpleModelParser" should {
    "empty" in {
      val src = """
"""
      val model = parser.apply(src)
    }
    "simple" in {
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
    "association" in {
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
}
