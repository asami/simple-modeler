package org.simplemodeling.SimpleModeler.transformer.maker.sql

import scalaz._
import Scalaz._
import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May.  3, 2012
 *  version Mar.  2, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class Sql92SqlClassDefinition(
  aContext: SqlContext,
  model: PModel,
  anAspects: Seq[SqlAspect],
  sqlObject: SqlObject,
  maker: JavaMaker = null
) extends SqlClassDefinition(aContext, model, anAspects, sqlObject, maker) {
  override protected def attribute(attr: PAttribute): ATTR_DEF = {
    new Sql92SqlClassAttributeDefinition(aContext, model, anAspects, attr, this, jm_maker)
  }
}
