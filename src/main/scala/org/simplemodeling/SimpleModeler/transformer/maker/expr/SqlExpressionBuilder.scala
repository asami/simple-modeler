package org.simplemodeling.SimpleModeler.transformer.maker.expr

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Dec. 21, 2012
 *  version Jan. 11, 2013
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlExpressionBuilder(
  expr: MExpression
)(implicit c: PContext, joinedAttributes: Seq[(PAttribute, String)]) extends StringExpressionBuilder(c, expr) {
  val attributes = joinedAttributes.map(_._1)
  var current: List[(PAttribute, String)] = Nil // TODO work around

  def isTarget(attr: PAttribute) = {
    expr_eval("", expr.tree)
//    println("SqlExpressionBuilder#isTarget(%s) = %s".format(expr, current))
    current.exists(_._1 == attr)
  }

  override protected def expr_identifier(parent: String, x: MEIdentifier): String = {
//    println("SqlExpressionBuilder#expr_identifier = %s".format(x.name))
    get_association_class(x).map(_expr_identifier_association_class(x)) orElse
    get_attribute(x).map(_expr_identifier_attribute(x)) getOrElse
    current.headOption.map(a => "%s.%s".format(a._2, x.name)) | x.name
  }

  private def _expr_identifier_attribute(x: MEIdentifier)(a: PAttribute): String = {
//    println("SqlExpressionBuilder#_expr_identifier_attribute(%s) = %s".format(x, a))
    current = joinedAttributes.find(_._1 == a).get :: current
    val t = current.head._2
    "%s.%s".format(t, context.sqlColumnName(a))
  }

  private def _expr_identifier_association_class(x: MEIdentifier)(a: PAttribute): String = {
//    println("SqlExpressionBuilder#_expr_identifier_attribute(%s) = %s".format(x, a))
    current = joinedAttributes.find(_._1 == a).get :: current
    val t = current.head._2
    "%s.%s".format(t, context.sqlColumnName(a))
  }
}
