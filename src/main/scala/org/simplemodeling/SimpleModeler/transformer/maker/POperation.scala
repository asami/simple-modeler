package org.simplemodeling.SimpleModeler.transformer.maker

import org.simplemodeling.model._

/*
 * @since   Jan.  4, 2020
 *  version Jan.  5, 2020
 *  version Mar.  2, 2020
 * @version Apr. 25, 2020
 * @author  ASAMI, Tomoharu
 */
trait POperation extends PElement {
  def in: Option[PVoucherType] = ???
  def out: Option[PVoucherType] = ???
}
