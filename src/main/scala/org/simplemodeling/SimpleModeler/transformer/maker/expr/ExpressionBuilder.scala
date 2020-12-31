package org.simplemodeling.SimpleModeler.transformer.maker.expr

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString
import org.goldenport.recorder.Recordable
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Dec. 21, 2012
 *  version Jan. 12, 2013
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class ExpressionBuilder(val context: PContext, val expr: MExpression) extends Recordable {
  implicit def MExpressionShow: Show[MExpressionNode] = ??? // showA
  type RESULT_TYPE

  set_Recorder(context)

  def attributes: Seq[PAttribute]
  protected def root_Result: RESULT_TYPE

  def build: RESULT_TYPE = {
    expr_eval(root_Result, expr.tree)
  }

  protected def expr_eval(parent: RESULT_TYPE, expr: Tree[MExpressionNode]): RESULT_TYPE = {
    expr.rootLabel match {
      case x: MEBoolean => expr_boolean(parent, expr, x)
      case x: MEBracket => expr_bracket(parent, expr, x)
      case x: MEChoice => expr_choice(parent, expr, x)
      case x: MEComposite => expr_composite(parent, expr, x)
      case x: MEDot => expr_dot(parent, expr, x)
      case x: MEEval => expr_eval(parent, x.children(0))
      case x: MEIdentifier => expr_identifier(parent, x)
      case x: MEMethod => expr_method(parent, expr, x)
      case x: MENested => expr_nested(parent, expr, x)
      case x: MENull => expr_null(parent, expr, x)
      case x: MENumber => expr_number(parent, x)
      case x: MEProperty => expr_property(parent, expr, x)
      case x: MEString => expr_string(parent, x)
      case x: METext => expr_text(parent, x)
      case x: MEUnary => expr_unary(parent, expr, x)
      case x: MEBOAdd => expr_binary_operator(parent, x)
      case x: MEBOAnd => expr_binary_operator(parent, x)
      case x: MEBODiv => expr_binary_operator(parent, x)
      case x: MEBOEq => expr_binary_operator(parent, x)
      case x: MEBOGe => expr_binary_operator(parent, x)
      case x: MEBOGt => expr_binary_operator(parent, x)
      case x: MEBOLe => expr_binary_operator(parent, x)
      case x: MEBOLt => expr_binary_operator(parent, x)
      case x: MEBOMod => expr_binary_operator(parent, x)
      case x: MEBOMul => expr_binary_operator(parent, x)
      case x: MEBONe => expr_binary_operator(parent, x)
      case x: MEBOOr => expr_binary_operator(parent, x)
      case x: MEBOSub => expr_binary_operator(parent, x)
    }
  }

  protected def expr_boolean(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEBoolean): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_bracket(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEBracket): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_choice(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEChoice): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_composite(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEComposite): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_dot(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEDot): RESULT_TYPE = {
    expr_eval(parent, x.lhs)
    expr_eval(parent, x.rhs)
  }

  protected def expr_method(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEMethod): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_nested(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MENested): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_null(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MENull): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_number(parent: RESULT_TYPE, x: MENumber): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_property(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEProperty): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_string(parent: RESULT_TYPE, x: MEString): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_text(parent: RESULT_TYPE, x: METext): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_unary(parent: RESULT_TYPE, expr: Tree[MExpressionNode], x: MEUnary): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_identifier(parent: RESULT_TYPE, x: MEIdentifier): RESULT_TYPE = {
    sys.error("???")
  }

  protected def expr_binary_operator(parent: RESULT_TYPE, expr: MEBinaryOperator): RESULT_TYPE = {
    sys.error("???")
  }

  protected final def get_attribute(x: MEIdentifier): Option[PAttribute] = {
    attributes.find(_.name == x.name)
  }

  protected final def get_attribute_entity(x: MEIdentifier): Option[PAttribute] = {
    attributes.find(is_attribute_entity(x.name))
  }

  protected final def is_attribute_entity(name: String)(a: PAttribute): Boolean = {
    a.name == name && a.isEntity
  }

  protected final def get_association_class(x: MEIdentifier): Option[PAttribute] = {
    attributes.find(is_association_class(x.name))
  }

  protected final def is_association_class(name: String)(a: PAttribute): Boolean = {
    a.platformParticipation.map(_.source.name == name) | false
  }
}
