package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   Jul.  9, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class MEvent(
  description: Description,
  affiliation: MPackageRef = MPackageRef.default,
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

object MEvent {
  def apply(name: String): MEvent = {
    val desc = Description.name(name)
    new MEvent(desc)
  }
}
