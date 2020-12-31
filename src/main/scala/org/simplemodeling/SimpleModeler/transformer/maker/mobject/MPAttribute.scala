package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 25, 2020
 *  version Apr. 26, 2020
 *  version May.  3, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPAttribute(model: MAttribute) extends PAttribute with MPElement {
  def description: Description = Description.empty
  def attributeType: PObjectType = PObjectType(model.attributeType)
  def readonly = model.readonly
}
