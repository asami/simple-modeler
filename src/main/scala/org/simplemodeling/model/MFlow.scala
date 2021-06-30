package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from Flow and SMFlow.
 * 
 * @since   Jan.  4, 2011
 *  version Mar. 26, 2011
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MFlow(
  override val designation: Designation,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil,
  description: Description = Description.empty
) extends MObject {
  def base: Option[MObjectRef] = None
  def traits: List[MTraitRef] = Nil
  def attributes: List[MAttribute] = Nil
  def associations: List[MAssociation] = Nil
  def operations: List[MOperation] = Nil
  def ports: List[MPort] = Nil
  def powertypes: List[MPowertypeRef] = Nil
  def stateMachines: List[MStateMachineRef] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}
