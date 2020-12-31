package org.simplemodeling.model

import org.goldenport.record.v2

/*
 * derived from SMultiplicity and SMMultiplicity.
 *
 * @since   Sep. 11, 2008
 *  version Oct. 12, 2008
 *  version Jan. 18, 2009
 *  version Nov. 12, 2010
 *  version Nov. 23, 2012
 *  version Aug.  7, 2019
 *  version Nov.  4, 2019
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MMultiplicity {
  def multiplicity: v2.Multiplicity
  def mark: String = multiplicity.mark // +
  def label: String = multiplicity.label // 1..*
  def keywords: List[String] = Nil
}

object MMultiplicity {
  def create(p: String): MMultiplicity = v2.Multiplicity.to(p) match {
    case v2.MOne => MOne
    case v2.MZeroOne => MZeroOne
    case v2.MOneMore => MOneMore
    case v2.MZeroMore => MZeroMore
    case m: v2.MRange => MRange(m)
    case m: v2.MRanges => MRanges(m)
  }
}

case object MOne extends MMultiplicity {
  val multiplicity = v2.MOne
}

case object MZeroOne extends MMultiplicity {
  val multiplicity = v2.MZeroOne
}

case object MOneMore extends MMultiplicity {
  val multiplicity = v2.MOneMore
}

case object MZeroMore extends MMultiplicity {
  val multiplicity = v2.MZeroMore
}

case class MRange(multiplicity: v2.MRange) extends MMultiplicity {
}

case class MRanges(multiplicity: v2.MRanges) extends MMultiplicity {
}
