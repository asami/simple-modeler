package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.goldenport.values.Designation
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 25, 2020
 *  version May.  3, 2020
 * @version Jun. 13, 2020
 * @author  ASAMI, Tomoharu
 */
trait MPElement extends PElement {
  def model: MElement

  def designation: Designation = model.designation
  override def getAffiliation = model.getAffiliation
}
