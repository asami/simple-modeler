package org.simplemodeling.model

/*
 * Derived from STraitRelationship and SMTraitRelationship.
 * 
 * @since   Oct. 16, 2012
 *  since   Aug.  8, 2019
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MTraitRef(packageRef: MPackageRef, traitName: String) extends MReference {
  def relationshipType = MRelationshipType(traitName, packageRef)
  // def targetName: String = traitName
  // def targetPackageName: String = packageRef.packageName
  def mixinTrait(implicit sm: SimpleModel): MTrait = ???
}
