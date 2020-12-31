package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May.  3, 2012
 *  version May.  3, 2012
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class SqlAspect extends GenericAspect with JavaMakerHolder {
  def openSqlClass(els: SqlClassDefinition) {
    // XXX
  }
}
