package org.simplemodeling.SimpleModeler.transformer.maker

import scala.collection.mutable.ArrayBuffer
// import org.simplemodeling.SimpleModeler.entity._
import com.asamioffice.goldenport.text.UString.notNull
// import org.simplemodeling.dsl._
import java.text.SimpleDateFormat
import java.util.TimeZone

/* 
 * @since   Nov. 11, 2012
 *  version Jan.  5, 2020
 *  version Mar.  1, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class GenericClassOperationDefinition(
  val pContext: PContext,
  val model: PModel,
  val aspects: Seq[GenericAspect],
  val op: POperation,
  val owner: GenericClassDefinition
) {
  def method: Unit
}

object NullClassOperationDefinition extends GenericClassOperationDefinition(null, null, Nil, null, null) {
  def method { }
}
