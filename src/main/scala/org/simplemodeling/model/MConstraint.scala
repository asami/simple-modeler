package org.simplemodeling.model

import org.goldenport.record.v2

/*
 * derived from SConstraint and SMConstraint.
 *
 * @since   Jun. 18, 2009
 *  version Feb.  9, 2012
 *  version Apr. 11, 2012
 *  version Aug.  7, 2019
 *  version Nov.  4, 2019
 *  version Jan.  5, 2020
 *  version Mar. 25, 2026
 * @version Apr. 19, 2026
 * @author  ASAMI, Tomoharu
 */
trait MConstraint {
  def name: String = ???
  def value: Any = ???
}

case class RConstraint(constraint: v2.Constraint) extends MConstraint {
  override lazy val name: String = constraint match {
    case _: v2.CMaxLength => "max"
    case _: v2.CMinLength => "min"
    case _: v2.CRegex => "pattern"
    case m => m.label
  }

  override lazy val value: Any = constraint match {
    case m: v2.CMaxLength => m.length
    case m: v2.CMinLength => m.length
    case m: v2.CRegex => m.regex.regex
    case m => m.label
  }
}

case class LiteralConstraint(
  override val name: String,
  override val value: Any
) extends MConstraint

object MConstraint {
  def create(p: String): MConstraint = new MConstraint {
    override val name: String = p
    override val value: Any = p
  }
}
