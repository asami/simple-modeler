package org.simplemodeling.SimpleModeler.transformer.maker

import scala.collection.mutable.{Buffer, ArrayBuffer}
// import org.simplemodeling.SimpleModeler.entity._
import org.simplemodeling.model._

// derived from GaejConstraint since Apr. 10, 2009
/*
 * @since   Apr. 23, 2011
 *  version Feb.  7, 2012
 *  version Jan.  5, 2020
 * @version Mar.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class PConstraint(val name: String, val value: Any) {
  def literal: String = {
    value match {
      case v: Byte => value.toString
      case v: Short => value.toString
      case v: Int => value.toString
      case v: Long => value.toString
      case v: Float => value.toString
      case v: Double => value.toString
      case o: AnyRef => value.toString
    }
  }

  def stringLiteral: String = {
    value match {
      case v: Byte => "\"" + value + "\""
      case v: Short => "\"" + value + "\""
      case v: Int => "\"" + value + "\""
      case v: Long => "\"" + value + "\""
      case v: Float => "\"" + value + "\""
      case v: Double => "\"" + value + "\""
      case o: AnyRef => "\"\\\"" + value + "\\\"\""
    }
  }

  def params(keys: String*): String = {
    value match {
      case map: Map[Any, Any] => {
        keys.flatMap { k =>
          map.get(k).map("%s = %s".format(k, _))
        }.mkString(",")
      }
      case l: List[_] => {
        keys.zip(l).map {
          case (k, v) => "%s = %s".format(k, v)
        }.mkString(",")
      }
      case _ => value.toString // XXX warning
    }
  }

  def getLongValue(): Option[Long] = {
    value match {
      case v: Byte => Some(v)
      case v: Short => Some(v)
      case v: Int => Some(v)
      case v: Long => Some(v)
      case v: String => Some(java.lang.Long.parseLong(v))
      case None => None
    }
  }
}

object PConstraint {
  def apply(p: MConstraint): PConstraint = ???
}
