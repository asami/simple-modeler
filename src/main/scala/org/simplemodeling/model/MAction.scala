package org.simplemodeling.model

import scala.collection.mutable.LinkedHashMap
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * Derived from SMAction.
 * 
 * @since   Mar. 15, 2009
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class MAction(
  description: Description,
  ownerStateMachine: MStateMachine
) extends MElement {
  def getAffiliation = None
}

object MAction {
  def apply(sm: MStateMachine, name: String): MAction = MAction(Description.name(name), sm)
}

