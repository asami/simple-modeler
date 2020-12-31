package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * derived from SRelationship and SMRelationship.
 *
 * @since   Sep. 10, 2008
 *  version Nov. 21, 2008
 *  version Aug.  7, 2019
 *  version Apr. 25, 2020
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 *  version Aug. 12, 2020
 * @version Sep. 26, 2020
 * @author  ASAMI, Tomoharu
 */
trait MReference extends MRelationship {
  def designation: Designation = Designation.empty
  def description: Description = Description.empty
  def getAffiliation: Option[MPackageRef] = None
}
