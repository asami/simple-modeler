package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SUsecase and SMUsecase.
 * 
 * @since   Nov.  6, 2008
 *  version Dec.  7, 2008
 *  version Jan. 18, 2009
 *  version Nov.  4, 2011
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MUsecase extends MStoryObject {
  // def stereotypes: List[MStereotype] = Nil
  def base: Option[MObjectRef] = None
  def traits: List[MTraitRef] = Nil
  def powertypes: List[MPowertypeRef] = Nil
  def stateMachines: List[MStateMachineRef] = Nil
  def attributes: List[MAttribute] = Nil
  def associations: List[MAssociation] = Nil
  def operations: List[MOperation] = Nil
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}
