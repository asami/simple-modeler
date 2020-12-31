package org.simplemodeling.SimpleModeler.transformer.maker

import org.simplemodeling.model._

/*
 * Derived from PTraitEntity.
 * 
 * @since   Nov.  6, 2012
 *  version Jan. 29, 2013
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
/*
 * Jan. 29, 2013:
 * Trials that PTraitEntity be PEntityEntity and PDocumentEntity does not work.*/
trait PTrait extends PObject {
  var isDocument = false
}
