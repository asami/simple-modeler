package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.RAISE
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Migrated from SValue and SMValue.
 * 
 * @since   Sep. 15, 2008
 *  version Jan.  5, 2020
 *  version Mar.  1, 2020
 *  version Apr. 26, 2020
 *  version May.  3, 2020
 *  version Jun. 13, 2020
 *  version Aug.  1, 2020
 * @version Dec. 20, 2020
 * @author  ASAMI, Tomoharu
 */
case class PValue(
  designation: Designation,
  affiliation: MPackageRef,
  description: Description = Description.empty,
  stereotypes: List[MStereotype] = Nil
) extends PObject {
  def attributes: List[PAttribute] = Nil
  def operations: List[POperation] = Nil
  def getBaseObjectType: Option[PObjectReferenceType] = None
  def getTraitObjects: List[PObjectReferenceType] = Nil
  def associationEntityAttributes: List[PAttribute] = Nil

//  override def typeName: String = "value"
  def contentType = attributes(0).attributeType
}
