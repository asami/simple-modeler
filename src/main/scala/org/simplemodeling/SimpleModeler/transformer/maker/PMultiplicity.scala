package org.simplemodeling.SimpleModeler.transformer.maker

/**
 * @since   Apr. 23, 2011
 *  version Nov. 24, 2012
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait PMultiplicity {
  def mark: String
}
case object POne extends PMultiplicity {
  def mark = ""
}
case object PZeroOne extends PMultiplicity {
  def mark = "?"
}
case object POneMore extends PMultiplicity {
  def mark = "+"
}
case object PZeroMore extends PMultiplicity {
  def mark = "*"
}
case class PRange() extends PMultiplicity {
  def mark = "-"
}
