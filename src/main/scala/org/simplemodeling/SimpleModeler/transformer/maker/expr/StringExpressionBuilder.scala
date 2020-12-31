package org.simplemodeling.SimpleModeler.transformer.maker.expr

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Jan. 11, 2013
 *  version Jan. 15, 2013
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class StringExpressionBuilder(
  c: PContext, e: MExpression
) extends ExpressionBuilder(c, e) {
  type RESULT_TYPE = String

  protected def root_Result = ""
}
