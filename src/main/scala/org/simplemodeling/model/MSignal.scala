package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   Jul.  9, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class MSignal(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil
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
