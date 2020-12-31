package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SqlDocumentEntity.
 *
 * @since   Nov.  1, 2012
 *  version Mar.  3, 2020
 *  version Apr. 25, 2020
 *  version May.  3, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlVoucher(aContext: SqlContext) extends SqlObject(aContext) with PVoucher {
  def description: Description = Description.empty
  def attributes: List[PAttribute] = ???
  def operations: List[POperation] = ???
}
