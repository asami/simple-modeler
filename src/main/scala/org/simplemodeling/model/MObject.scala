package org.simplemodeling.model

import org.goldenport.RAISE

/*
 * derived from ModelElement since Mar. 18, 2007.
 * derived from SObject and SMObject.
 *
 * @since   Sep. 10, 2008
 *  version Jul. 13, 2011
 *  version Sep. 18, 2011
 *  version Feb.  7, 2012
 *  version Apr.  8, 2012
 *  version Oct. 21, 2012
 *  version Nov. 25, 2012
 *  version Dec. 18, 2012
 *  version Jan. 10, 2013
 *  version Feb.  7, 2013
 *  version Aug.  8, 2019
 *  version Apr. 25, 2020
 *  version May. 17, 2020
 * @version Sep. 26, 2020
 * @author  ASAMI, Tomoharu
 */
trait MObject extends MElement { // Classifier
  def affiliation: MPackageRef
  override def getAffiliation: Option[MPackageRef] = Some(affiliation)
  def packageName: String = affiliation.packageName
  def stereotypes: List[MStereotype]
  def base: Option[MObjectRef]
  def traits: List[MTraitRef]
  def powertypes: List[MPowertypeRef]
  def stateMachines: List[MStateMachineRef]
  def attributes: List[MAttribute]
  def associations: List[MAssociation]
  def operations: List[MOperation]
  def ports: List[MPort]
  def roles: List[MRoleRef]
  def services: List[MServiceRef]
  def rules: List[MRuleRef]
  def vouchers: List[MVoucherRef]
  // uses
  // participants
  // actions
  // displays

  def getBaseObject(implicit sm: SimpleModel): Option[MObject] = base.flatMap(sm.getObject)
  def derivedObjects(implicit sm: SimpleModel): List[MObject] = sm.takeDerivedObjects(this)

  /*
   * legacy
   */
  def typeName: String = ??? // TODO usage
  def kindName: String = ??? // TODO usage
  def powertypeName: String = ??? // TODO usage
}
