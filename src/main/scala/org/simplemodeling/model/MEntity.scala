package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SEntity and SMEntity.
 * 
 * @since   Sep. 10, 2008
 *  version Jun.  5, 2009
 *  version Nov.  6, 2009
 *  version Sep. 18, 2011
 *  version Oct. 15, 2012
 *  version Nov. 25, 2012
 *  version Aug.  7, 2019
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MEntity extends MObject {
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}
