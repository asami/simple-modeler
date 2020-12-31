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
 * @version May.  6, 2020
 * @author  ASAMI, Tomoharu
 */
trait MConstraint {
  def name: String = ???
  def value: Any = ???
}

case class RConstraint(constraint: v2.Constraint) extends MConstraint {
}

object MConstraint {
  def create(p: String): MConstraint = ???
}
