package org.simplemodeling.model

/*
 * Derived from SRoleRelationship and SMRoleRelationship.
 * 
 * @since   Nov. 19, 2008
 *  version Feb. 27, 2009
 *  version Aug.  8, 2019
 *  version May.  9, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MRoleRef(
  packageRef: MPackageRef,
  roleName: String,
  multiplicity: MMultiplicity = MOne
) extends MReference {
  def relationshipType = MRelationshipType(roleName, packageRef)
  // def targetName: String = roleName
  // def targetPackageName: String = packageRef.packageName
  def role(implicit sm: SimpleModel): MRole = ???
}
