package org.simplemodeling.model

/*
 * derived from SPowertypeKind.
 *
 * @since   Dec. 20, 2008
 *  version Oct. 25, 2009
 *  version Nov. 13, 2012
 *  version Nov.  3, 2019
 * @version Apr.  3, 2026
 * @author  ASAMI, Tomoharu
 */
case class MPowertypeKind(
  name: String,
  value: Option[String],
  labelText: Option[String] = None
) {
  def label: String = labelText.getOrElse(name)
}
