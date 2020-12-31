package org.simplemodeling.model

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString
import org.apache.commons.lang3.StringUtils

/*
 * Derived from SExpression and SMExpression.
 *
 * @since   Oct. 28, 2012
 *  version Nov.  1, 2012
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MExpression(dslExpression: String) {
  require (dslExpression != null, "dslExpression must null")
//  require (StringUtils.isNotBlank(dslExpression.expr), "dslExpression must not empty")
  final def name = sys.error("not implemented yet")
  final def value = sys.error("not implemented yet")

  lazy val tree: Tree[MExpressionNode] = {
    // MExpressionJuel.tree("${" + dslExpression.expr + "}")
    ???
  }
}

sealed trait MExpressionNode {
  def children: Seq[Tree[MExpressionNode]]
}

trait MLeafExpressionNode extends MExpressionNode {
  val children = Nil
  def toJava: String
}

case class MEBoolean(value: Boolean) extends MLeafExpressionNode {
  def toJava = value.toString
}

case class MEBracket(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

case class MEChoice(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

case class MEComposite(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

case class MEDot(
  children: Seq[Tree[MExpressionNode]]
) extends MExpressionNode {
  require (children.length == 2, "Dot must have 2 children.")
  def lhs = children(0)
  def rhs = children(1)
  def containerPath: Seq[Tree[MExpressionNode]] = {
    rhs match {
      case xs: MEDot => lhs +: xs.containerPath
      case _ => List(lhs)
    }
  }
}

case class MEEval(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

case class MEIdentifier(name: String) extends MLeafExpressionNode {
  def toJava = name
}

case class MEMethod() extends MLeafExpressionNode {
  def toJava = "MEMethod: not implemented yet"
}

case class MENested(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

case class MENull() extends MLeafExpressionNode {
  def toJava = "null"
}

case class MENumber(value: Number) extends MLeafExpressionNode {
  def toJava = value.toString
}

case class MEProperty() extends MLeafExpressionNode {
  def toJava = "MEProperty: not implemented yet"
}

case class MEString(value: String) extends MLeafExpressionNode {
  def toJava = UJavaString.stringLiteral(value)
}

case class METext(value: String) extends MLeafExpressionNode {
  def toJava = UJavaString.stringLiteral(value)
}

case class MEUnary(
  children: Seq[Tree[MExpressionNode]]) extends MExpressionNode {
}

trait MEBinaryOperator extends MExpressionNode {
  def children: Seq[Tree[MExpressionNode]]
  require (children.length == 2, "Binary operator must have 2 children.")
  def lhs = children(0)
  def rhs = children(1)
  def toJavaOperator: String
  def toOperatorMethodName: String
}

case class MEBOAdd(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "+"
  def toOperatorMethodName = "add"
}

case class MEBOAnd(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "&&"
  def toOperatorMethodName = "and"
}

case class MEBODiv(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "/"
  def toOperatorMethodName = "divide"
}

case class MEBOEq(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "=="
  def toOperatorMethodName = "euqal"
}

case class MEBOGe(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = ">="
  def toOperatorMethodName = "greaterEqualThan"
}

case class MEBOGt(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = ">"
  def toOperatorMethodName = "greaterThan"
}

case class MEBOLe(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "<="
  def toOperatorMethodName = "lesserEqualThan"
}

case class MEBOLt(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "<"
  def toOperatorMethodName = "lesserThan"
}

case class MEBOMod(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "%"
  def toOperatorMethodName = "mod"
}

case class MEBOMul(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "*"
  def toOperatorMethodName = "multiply"
}

case class MEBONe(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "!="
  def toOperatorMethodName = "notEqual"
}

case class MEBOOr(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "||"
  def toOperatorMethodName = "or"
}

case class MEBOSub(
  children: Seq[Tree[MExpressionNode]]
) extends MEBinaryOperator {
  def toJavaOperator = "-"
  def toOperatorMethodName = "negate"
}
