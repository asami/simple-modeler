package org.simplemodeling.model

import scala.collection.mutable.LinkedHashMap
import org.goldenport.values.Designation
import org.goldenport.i18n.I18NString
import org.smartdox.Description

/*
 * Derived from STransition and SMTransition.
 * 
 * @since   Dec. 20, 2008
 *  version Mar. 18, 2009
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MTransition(
  designation: Designation,
  ownerStateMachine: MStateMachine,
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = None

  def event: MObject = ???
  def preState: MState = ???
  def postState: MState = ???
  def guard: MGuard = ???
  def action: MAction = ???
}
