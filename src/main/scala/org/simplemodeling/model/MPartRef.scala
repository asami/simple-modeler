package org.simplemodeling.model

/*
 * Derived from DomainEntityPart, SMEntityPart and SMDomainEntityPart.
 * 
 * @since   Jan.  7, 2009
 *  version Jun. 20, 2009
 *  version Oct. 12, 2012
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPartRef(packageRef: MPackageRef, partName: String) extends MReference {
  def relationshipType = MRelationshipType(partName, packageRef)
  // def targetName: String = partName
  // def targetPackageName: String = packageRef.packageName
  def part(implicit sm: SimpleModel): MEntity = ???
}
