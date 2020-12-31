package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.goldenport.RAISE
import org.simplemodeling.model.MStereotype
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   May.  3, 2012
 *  version May.  3, 2012
 *  version Nov.  1, 2012
 *  version Mar.  3, 2020
 *  version Apr. 26, 2020
 *  version May.  3, 2020
 * @version Dec. 20, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class SqlObject(val sqlContext: SqlContext) extends PObject {
  val fileSuffix = "sql"
  def designation = RAISE.notImplementedYetDefect
  def affiliation = RAISE.notImplementedYetDefect
  def getBaseObjectType: Option[PObjectReferenceType] = None
  def getTraitObjects: List[PObjectReferenceType] = Nil
  def associationEntityAttributes: List[PAttribute] = Nil
  def stereotypes: List[MStereotype] = Nil // TODO
}
