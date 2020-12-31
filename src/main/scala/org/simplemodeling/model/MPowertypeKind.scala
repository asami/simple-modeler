package org.simplemodeling.model

/*
 * derived from SPowertypeKind.
 *
 * @since   Dec. 20, 2008
 *  version Oct. 25, 2009
 *  version Nov. 13, 2012
 *  version Nov.  3, 2019
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPowertypeKind(
  name: String,
  value: Option[String]
) {
  def label: String = name // TODO
}
