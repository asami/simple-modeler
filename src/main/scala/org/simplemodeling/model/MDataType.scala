package org.simplemodeling.model

import org.smartdox.Description
import org.goldenport.values.Designation
import org.goldenport.record.v2._

/*
 * derived from SDatatype and SMDatatype.
 *
 * @since   Sep. 10, 2008
 *  version Oct. 24, 2008
 *  version Jan. 18, 2009
 *  version Jul. 23, 2011
 *  version Aug.  7, 2019
 *  version Nov.  4, 2019
 *  version Dec. 15, 2019
 *  version Apr. 25, 2020
 *  version May. 16, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 *  version Feb. 10, 2026
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
case class MDataType(
  override val designation: Designation,
  datatype: DataType,
  affiliation: MPackageRef,
  description: Description = Description.empty,
  resolveDeclaredType: Boolean = true
) extends MAttributeType {
  override def getAffiliation = Some(affiliation)
}

object MDataType {
  val string = MDataType(XString)

  def apply(p: DataType): MDataType = apply(p, MPackageRef.default)

  def apply(p: DataType, pkg: MPackageRef): MDataType =
    MDataType(Designation.nameLabel(p.name, p.labelI18N), p, pkg)

  def create(p: String): MDataType = MDataType(Designation(p), DataType.to(p), MPackageRef.default)
}
