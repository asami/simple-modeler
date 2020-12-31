package org.simplemodeling.model

/*
 * Derived from SStateMachineRelationship and SMStateMachineRelationship.
 * 
 * @since   Dec. 20, 2008
 *  version Mar. 18, 2009
 *  version Nov. 26, 2012
 *  version Feb.  6, 2013
 *  version Aug.  8, 2019
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MStateMachineRef(packageRef: MPackageRef, stateMachineName: String) extends MReference {
  def relationshipType = MRelationshipType(stateMachineName, packageRef)
  // def targetName: String = stateMachineName
  // def targetPackageName: String = packageRef.packageName
  def statemachine(implicit sm: SimpleModel): MStateMachine = ???
}
