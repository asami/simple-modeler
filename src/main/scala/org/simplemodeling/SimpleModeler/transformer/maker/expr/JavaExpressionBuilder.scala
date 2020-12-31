package org.simplemodeling.SimpleModeler.transformer.maker.expr

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Dec. 21, 2012
 *  version Jan. 29, 2013
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
class JavaExpressionBuilder(
  c: PContext,
  val klass: JavaClassDefinition,
  val attr: JavaClassAttributeDefinition,
  expr: MExpression
) extends StringExpressionBuilder(c, expr) {
  val attributes = klass.wholeAttributeDefinitions.map(_.attr)

  override protected def expr_boolean(parent: String, expr: Tree[MExpressionNode], x: MEBoolean): String = {
    x.toJava
  }

  override protected def expr_bracket(parent: String, expr: Tree[MExpressionNode], x: MEBracket): String = {
    expr.drawTree
  }

  override protected def expr_choice(parent: String, expr: Tree[MExpressionNode], x: MEChoice): String = {
     expr.drawTree
  }

  override protected def expr_composite(parent: String, expr: Tree[MExpressionNode], x: MEComposite): String = {
     expr.drawTree
  }

  override protected def expr_dot(parent: String, expr: Tree[MExpressionNode], x: MEDot): String = {
    val cond = x.containerPath.map(p => expr_eval(parent, p) + " != null").mkString(" && ")
    "(%s) ? %s : null".format(
      cond,
      expr_eval(parent, x.lhs) + "." + expr_eval(parent, x.rhs))
  }

  override protected def expr_method(parent: String, expr: Tree[MExpressionNode], x: MEMethod): String = {
    expr.drawTree    
  }

  override protected def expr_nested(parent: String, expr: Tree[MExpressionNode], x: MENested): String = {
    expr.drawTree
  }

  override protected def expr_null(parent: String, expr: Tree[MExpressionNode], x: MENull): String = {
    x.toJava
  }

  override protected def expr_number(parent: String, x: MENumber): String = {
    x.toJava
  }

  override protected def expr_property(parent: String, expr: Tree[MExpressionNode], x: MEProperty): String = {
    expr.drawTree
  }

  override protected def expr_string(parent: String, x: MEString): String = {
     x.toJava
  }

  override protected def expr_text(parent: String, x: METext): String = {
    x.toJava
  }

  override protected def expr_unary(parent: String, expr: Tree[MExpressionNode], x: MEUnary): String = {
     expr.drawTree
  }

  override protected def expr_identifier(parent: String, x: MEIdentifier): String = {
    _get_attribute(x) orElse
    _get_association_class(x) getOrElse {
      record_warning("式中の「%s」が見つかりません。".format(x))
      "null"
    }
  }

  override protected def expr_binary_operator(parent: String, expr: MEBinaryOperator): String = {
    _binary_operator_string(parent, expr)
  }

  private def _binary_operator_string(parent: String, expr: MEBinaryOperator): String = {
    _binary_operator_string_lib(parent, expr)
  }

  private def _binary_operator_string_raw(parent: String, expr: MEBinaryOperator): String = {
    expr_eval(parent, expr.lhs) + " " + expr.toJavaOperator + " " + expr_eval(parent, expr.rhs)
  }

  private def _binary_operator_string_lib(parent: String, expr: MEBinaryOperator): String = {
    "USimpleModeler." + expr.toOperatorMethodName + "(" +
    expr_eval(parent, expr.lhs) + ", " + expr_eval(parent, expr.rhs) + ")"
  }

  private def _get_attribute(x: MEIdentifier): Option[String] = {
    attributes.exists(_.name == x.name) option {
      UJavaString.makeGetterName(x.toJava) + "()"
    }
  }

  private def _get_association_class(x: MEIdentifier): Option[String] = {
    for (a <- attributes.find(is_association_class(x.name))) yield {
      UJavaString.makeGetterName(a.name) + "().get(0)"
    }
  }
}
