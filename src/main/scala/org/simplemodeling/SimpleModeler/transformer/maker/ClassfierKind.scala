package org.simplemodeling.SimpleModeler.transformer.maker

/*
 * @since   Feb. 20, 2012
 *  version Feb. 20, 2012
 *  version Jan.  5, 2020
 * @version Mar.  1, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait ClassifierKind {
  def keyword: String
}
object ClassClassifierKind extends ClassifierKind {
  val keyword = "class"
}
object InterfaceClassifierKind extends ClassifierKind {
  val keyword = "interface"
}
object EnumClassifierKind extends ClassifierKind {
  val keyword = "enum"
}
