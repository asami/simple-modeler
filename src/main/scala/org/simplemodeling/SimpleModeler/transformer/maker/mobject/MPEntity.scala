package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 25, 2020
 *  version Apr. 26, 2020
 *  version May.  3, 2020
 *  version Aug.  1, 2020
 * @version Sep. 21, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPEntity(
  val model: MEntity
) extends PEntity with MPObject {
  override def modelObject: MEntity = model
  def description: Description = model.description
  lazy val attributes: List[PAttribute] = model.attributes.map(MPAttribute)
  lazy val operations: List[POperation] = model.operations.map(MPOperation)
  lazy val getBaseObjectType: Option[PObjectReferenceType] =
    model.base.map(x => new PObjectReferenceType(x.objectName, x.packageRef.packageName))
  lazy val getTraitObjects: List[PObjectReferenceType] =
    model.traits.map(x => new PObjectReferenceType(x.traitName, x.packageRef.packageName))
  lazy val associationEntityAttributes: List[PAttribute] =
    model.associations.map(MPAssociationAttribute)
}
