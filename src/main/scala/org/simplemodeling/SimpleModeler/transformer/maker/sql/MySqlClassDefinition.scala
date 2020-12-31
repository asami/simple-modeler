package org.simplemodeling.SimpleModeler.transformer.maker.sql

import scalaz._
import Scalaz._
import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   Dec. 29, 2012
 *  version Feb. 21, 2013
 *  version Mar.  2, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class MySqlClassDefinition(
  aContext: SqlContext,
  model: PModel,
  anAspects: Seq[SqlAspect],
  sqlObject: SqlObject,
  maker: JavaMaker = null
) extends SqlClassDefinition(aContext, model, anAspects, sqlObject, maker) {
  override protected def attribute(attr: PAttribute): ATTR_DEF = {
    new MySqlClassAttributeDefinition(aContext, model, anAspects, attr, this, jm_maker)
  }

  override protected def create_Epilogue {
    jm_p(" DEFAULT CHARSET=utf8")
  }
}
