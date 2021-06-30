package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from SUse and SMUse.
 * 
 * @since   Nov. 10, 2008
 *  version Aug. 13, 2020
 *  version Sep. 22, 2020
 *  version Oct.  3, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MUse(
  override val designation: Designation,
  description: Description = Description.empty
) extends MRelationship {
  def relationshipType = MRelationshipType(???, ???)
  // def targetName: String = ???
  // def targetPackageName: String = ???
  def getAffiliation = None
}
