package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from STrait and SMTrait.
 * 
 * @since   Oct. 15, 2012
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MTrait extends MObject {
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}
