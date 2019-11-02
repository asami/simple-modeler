package org.simplemodeling.model

/*
 * derived from ModelElement since Mar. 18, 2007.
 * derived from SObject and SMObject.
 *
 * @since   Sep. 10, 2008
 *  version Sep. 18, 2011
 *  version Oct. 21, 2012
 *  version Nov. 22, 2012
 *  version Dec. 13, 2012
 *  version Jan. 10, 2013
 *  version Feb.  7, 2013
 * @version Aug.  8, 2019
 * @author  ASAMI, Tomoharu
 */
trait MObject extends MElement {
  def base: Option[MObjectRef]
  def traits: List[MTraitRef]
  def attributes: List[MAttribute]
  def associations: List[MAssociation]
  def operations: List[MOperation]
  // ports
  def stateMachines: List[MStateMachineRef]
  // roles
  // services
  // rules
  // documents
  // uses
  // participants
  // actions
  // displays
}
