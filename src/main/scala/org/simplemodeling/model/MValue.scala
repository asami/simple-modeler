package org.simplemodeling.model

import org.goldenport.i18n.I18NString

/*
 * Migrated from SValue and SMValue.
 * 
 * @since   Sep. 10, 2008
 *  version Oct. 22, 2008
 *  version Jan. 18, 2009
 *  version Sep. 19, 2011
 *  version Jan.  5, 2020
 *  version Apr. 25, 2020
 *  version May. 10, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
trait MValue extends MObject {
//  override def typeName: String = "value"

  // var datatype: SDatatype = null // XXX SNull
  // var invariants: SData => Boolean = null
  def associations: List[MAssociation] = ???
  // def attributes: List[MAttribute] = ???
  // def base: Option[MObjectRef] = ???
  // def operations: List[MOperation] = ???
  def stateMachines: List[MStateMachineRef] = ???
  // def traits: List[MTraitRef] = ???
  def ports: List[org.simplemodeling.model.MPort] = ???
  def roles: List[org.simplemodeling.model.MRoleRef] = ???
  def rules: List[org.simplemodeling.model.MRuleRef] = ???
  def services: List[org.simplemodeling.model.MServiceRef] = ???
  def vouchers: List[org.simplemodeling.model.MVoucherRef] = ???
}
