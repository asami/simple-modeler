package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 26, 2020
 *  version Apr. 28, 2020
 *  version May. 24, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPAssociationAttribute(model: MAssociation) extends PAttribute with MPElement {
  def description: Description = Description.empty
  def attributeType: PObjectType = PObjectReferenceType(model.objectRef)
  def readonly = false
}
