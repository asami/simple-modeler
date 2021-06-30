package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * Derived from SMAssociationType.
 *
 * @since   Oct. 24, 2008
 *  version May. 25, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Sep. 26, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MAssociationType(
  override val designation: Designation,
  entityRef: MEntityRef
) extends MRelationshipType {
  val target = entityRef.target

  def typeObject(implicit sm: SimpleModel): MEntity =
    sm.getEntity(entityRef).getOrElse(RAISE.noReachDefect)
}

object MAssociationType {
  def apply(name: Designation, ref: MObjectRef): MAssociationType = MAssociationType(
    name,
    ref.toEntityRef
  )
}
