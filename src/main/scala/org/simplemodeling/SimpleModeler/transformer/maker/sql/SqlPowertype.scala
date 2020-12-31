package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.smartdox.Description
import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May. 17, 2012
 *  version Dec. 25, 2012
 *  version Mar.  3, 2020
 *  version Apr. 25, 2020
 *  version May.  3, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlPowertypeEntity(sqlContext: SqlContext) extends SqlObject(sqlContext) with PPowertype {
  def description: Description = Description.empty
  def attributes: List[PAttribute] = ???
  def operations: List[POperation] = ???
  // override def class_Definition = {
  //   new Sql92SqlClassDefinition(sqlContext, Nil, SqlPowertypeEntity.this)
  // }
}
