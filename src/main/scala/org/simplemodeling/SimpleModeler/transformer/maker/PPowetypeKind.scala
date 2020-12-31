package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
import org.apache.commons.lang3.StringUtils
import org.simplemodeling.model._
// import org.simplemodeling.dsl.SPowertypeKind

/*
 * @since   Nov. 26, 2012
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class PPowertypeKind(name: String, value: Either[String, Int], label: String, model: MPowertypeKind) extends PEnumeration

object PPowertypeKind {
  def create(k: MPowertypeKind) = {
    val value = k.value match {
      case Some(v) => {
        v.parseInt.fold(_ => Left(k.name), x => Right(x))
      }
      case None => Left(k.name)
    }
    val l = k.label
    val label = if (StringUtils.isNotBlank(l)) l
    else k.name
    new PPowertypeKind(k.name, value, label, k)
  }
}











