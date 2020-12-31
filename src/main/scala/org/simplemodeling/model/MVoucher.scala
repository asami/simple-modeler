package org.simplemodeling.model

import org.goldenport.i18n.I18NString

/*
 * Migrated from SDocument and SMDocument.
 * 
 * @since   Sep. 11, 2008
 * @since   Jan. 19, 2009
 *  version Jan.  5, 2020
 *  version Apr. 25, 2020
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MVoucher extends MObject {
//  override def typeName: String = "voucher"
  def associations: List[MAssociation] = Nil
  def stateMachines: List[MStateMachineRef] = Nil
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}

// object NullDocument extends PVoucher(NullVoucher)
