package org.simplemodeling.model

import scala.collection.mutable.LinkedHashMap
import org.smartdox.Description
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation

/*
 * Derived from SMGuard.
 * 
 * @since   Mar. 15, 2009
 *  version May. 10, 2020
 *  version Jun. 13, 2020
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class MGuard(
  ownerStateMachine: MStateMachine,
  description: Description = Description.empty,
  mark: String = "???"
) extends MElement {
  def getAffiliation = None
}

object MGuard {
  val empty = MGuard(null, null) // TODO
}
