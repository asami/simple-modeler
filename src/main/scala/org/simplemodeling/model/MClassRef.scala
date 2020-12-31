package org.simplemodeling.model

/*
 * @since   Aug.  7, 2019
 *  version May.  6, 2020
 * @version Sep. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class MClassRef(packageRef: MPackageRef, className: String) extends MReference {
//  def targetName: String = className
//  def targetPackageName: String = packageRef.packageName
  val relationshipType: MRelationshipType = MRelationshipType(className, packageRef)
}
