package org.simplemodeling.model

import org.smartdox.Description

/*
 * Derived from SPowertype and SMPowertype.
 *
 * @since   Dec. 20, 2008
 *  version Jan. 20, 2009
 *  version Oct. 25, 2009
 *  version Sep. 19, 2011
 *  version Nov. 23, 2012
 *  version Nov.  3, 2019
 *  version Jan.  5, 2020
 *  version May. 10, 2020
 * @version Oct. 22, 2023
 * @author  ASAMI, Tomoharu
 */
case class MPowertype(
  description: Description,
  affiliation: MPackageRef,
  kinds: List[MPowertypeKind],
  stereotypes: List[MStereotype] = Nil,
  isKnowledge: Boolean = false
) extends MObject {
  def base: Option[MObjectRef] = None
  def traits: List[MTraitRef] = Nil
  def powertypes: List[MPowertypeRef] = Nil
  def attributes: List[MAttribute] = Nil
  def associations: List[MAssociation] = Nil
  def operations: List[MOperation] = Nil
  def stateMachines: List[MStateMachineRef] = Nil
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}
