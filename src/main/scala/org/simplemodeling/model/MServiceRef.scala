package org.simplemodeling.model

/*
 * Derived from SServiceRelationship and SMServiceRelationship.
 * 
 * @since   Nov. 19, 2008
 *  version Dec. 17, 2012
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MServiceRef(packageRef: MPackageRef, serviceName: String) extends MReference {
  def relationshipType = MRelationshipType(serviceName, packageRef)
  // def targetName: String = serviceName
  // def targetPackageName: String = packageRef.packageName
  def service(implicit sm: SimpleModel): MService = ???
}
