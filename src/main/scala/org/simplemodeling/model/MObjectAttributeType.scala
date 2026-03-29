package org.simplemodeling.model

import org.smartdox.Description

/*
 * @since   Mar. 30, 2026
 * @version Mar. 30, 2026
 * @author  ASAMI, Tomoharu
 */
final case class MObjectAttributeType(ref: MObjectRef) extends MAttributeType {
  def description: Description = Description.name(ref.objectName)
  override def getAffiliation: Option[MPackageRef] = Some(ref.packageRef)
  override def name: String = ref.objectName
  override def constraints: Seq[MConstraint] = Nil
}
