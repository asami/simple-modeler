package org.simplemodeling.SimpleModeler.transformer.maker

import org.simplemodeling.model._

/*
 * Migrated from PDocumentEntity.
 * 
 * @since   Apr. 23, 2011
 *  version Nov.  2, 2012
 *  version Jan.  5, 2020
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
trait PVoucher extends PObject {
  /**
   * Currently no one uses the variable.
   */
  // var modelDocumentOption: Option[SMDocument] = None
  /**
   * Referenced by GenericClassAttributeDefinition#is_composition_property,
   * 
   * Setted by GenericClassDefinition constructor.
   */
  // var modelEntityOption: Option[SMEntity] = None

  // protected final def id_var_name(attr: PAttribute) = {
  //   pContext.variableName4RefId(attr)
  // }

  // protected final def id_attr_type_name(attr: PAttribute) = {
  //   pContext.attributeTypeName4RefId(attr)
  // }

  // override protected def write_Content(out: BufferedWriter) {
  // }
}

// object PNullDocument extends PVoucher(NullVoucher)
