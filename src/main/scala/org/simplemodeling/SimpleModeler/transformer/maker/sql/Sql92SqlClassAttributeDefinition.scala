package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May.  3, 2012
 *  version Mar.  2, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class Sql92SqlClassAttributeDefinition(
  aContext: SqlContext,
  model: PModel,
  anAspects: Seq[SqlAspect],
  attr: PAttribute,
  owner: SqlClassDefinition,
  maker: JavaMaker = null
) extends SqlClassAttributeDefinition(aContext, model, anAspects, attr, owner, maker) {
}
