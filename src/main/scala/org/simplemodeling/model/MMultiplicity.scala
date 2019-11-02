package org.simplemodeling.model

/*
 * derived from SMultiplicity and SMMultiplicity.
 *
 * @since   Sep. 11, 2008
 *  version Nov. 12, 2010
 * @version Aug.  7, 2019
 * @author  ASAMI, Tomoharu
 */
trait MMultiplicity {
}

case class MOne() extends MMultiplicity {
}

case class MZeroOne() extends MMultiplicity {
}

case class OneMore() extends MMultiplicity {
}

case class ZeroMore() extends MMultiplicity {
}
