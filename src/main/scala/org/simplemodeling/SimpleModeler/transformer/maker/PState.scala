package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
import org.apache.commons.lang3.StringUtils
import org.simplemodeling.model._

/*
 * @since   Nov. 26, 2012
 *  version Feb.  6, 2013
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class PState(name: String, value: Either[String, Int], label: String, model: MState) extends PEnumeration {
//  def lifecycle = model.lifecycle
}

object PState {
  def create(s: MState) = {
    // val a = s.label.toText
    // val b = if (StringUtils.isNotBlank(a)) a else s.name
    val b = s.name
    new PState(s.name, s.value, b, s)
  }
}
