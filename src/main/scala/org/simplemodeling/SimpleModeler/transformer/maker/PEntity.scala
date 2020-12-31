package org.simplemodeling.SimpleModeler.transformer.maker

import org.simplemodeling.model._

// derived from GaejEntityEntity: since Apr. 10, 2009
// derived from PEntityEntity: Apr. 23, 2011 - Nov. 25, 2012
/*
 * @since   Jan.  1, 2020
 *  version Jan.  5, 2020
 *  version Mar.  2, 2020
 * @version Apr. 25, 2020
 * @author  ASAMI, Tomoharu
 */
trait PEntity extends PObject {
  def idPolicy: MIdPolicy = ???
  override def idAttr: PAttribute = ???
  def voucherName: String = s"${name.capitalize}Voucher"
  def isLogicalOperation: Boolean = ???  //    entityType.entity.modelEntity.appEngine.logical_operation
  def idName: String = ???
//  def modelEntity: MEntity = ???
  def joinEntities: List[PEntity] = ???
  def getInheritancePowertypeAttributeValue: Option[(PAttribute, Either[String, Int])] = ???
}
