package org.simplemodeling.model

/*
 * Derived from SRuleRelationship and SMRuleRelationship.
 * 
 * @since   Nov. 19, 2008
 *  version Dec. 17, 2012
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MRuleRef(packageRef: MPackageRef, ruleName: String) extends MReference {
  def relationshipType = MRelationshipType(ruleName, packageRef)
  // def targetName: String = ruleName
  // def targetPackageName: String = packageRef.packageName
  def rule(implicit sm: SimpleModel): MRule = ???
}
