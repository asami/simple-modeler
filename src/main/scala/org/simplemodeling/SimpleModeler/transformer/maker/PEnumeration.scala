package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
// import org.simplemodeling.dsl.SPowertypeKind
import org.simplemodeling.model._

/*
 * @since   Nov. 26, 2012
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
trait PEnumeration {
  val name: String
  val value: Either[String, Int]
  val label: String

  def sqlValue = {
    value match {
      case Left(v) => "'" + v + "'"
      case Right(v) => v.toString
    }
  }

  def sqlLabel = {
    "'" + label + "'"
  }
}
