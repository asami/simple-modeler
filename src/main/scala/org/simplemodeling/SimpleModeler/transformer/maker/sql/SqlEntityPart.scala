package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.smartdox.Description
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   May. 17, 2012
 *  version Mar.  2, 2020
 *  version Apr. 25, 2020
 *  version May.  3, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlEntityPart(aContext: SqlContext) extends SqlObject(aContext) with PEntityPart {
  def description: Description = Description.empty
  def attributes: List[PAttribute] = ???
  def operations: List[POperation] = ???
}
