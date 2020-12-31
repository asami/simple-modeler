package org.simplemodeling.model

/*
 * Derived from SPowertype and SMPowertype.
 *
 * @since   Dec. 20, 2008
 *  version Jan. 20, 2009
 *  version Oct. 25, 2009
 *  version Sep. 19, 2011
 *  version Nov. 23, 2012
 *  version Nov.  3, 2019
 *  version Jan.  5, 2020
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MPowertype extends MObject {
  def kinds: List[MPowertypeKind]
  def isKnowledge: Boolean = ???
}
