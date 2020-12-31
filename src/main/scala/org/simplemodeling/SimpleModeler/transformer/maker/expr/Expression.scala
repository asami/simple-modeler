package org.simplemodeling.SimpleModeler.transformer.maker.expr

import scalaz._, Scalaz._
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SqlSchemaExpressionBuilder.
 * 
 * @since   Jan. 13, 2013
 *  version Jan. 13, 2013
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait Expression {
}

case class RootExpression() extends Expression

case class ValueAttributeExpression(
  model: MEIdentifier,
  attribute: PAttribute
) extends Expression

case class EntityAttributeExpression(
  model: MEIdentifier,
  attribute: PAttribute,
  target: PEntity
) extends Expression

case class AssociationClassExpression(
  model: MEIdentifier,
  attribute: PAttribute,
  association: PEntity,
  tosource: PAttribute,
  totarget: PAttribute,
  target: PEntity
) extends Expression

case class DotContExpression(
  model: MEDot,
  lhs: Expression
) extends Expression

case class DotExpression(
  model: MEDot,
  lhs: Expression,
  rhs: Expression
) extends Expression
