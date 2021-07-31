package org.simplemodeling.model

import scala.collection.mutable.LinkedHashMap
import org.goldenport.RAISE
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
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 * @version Jul.  9, 2021
 * @author  ASAMI, Tomoharu
 */
case class MTransition(
  ownerStateMachine: MStateMachine,
  event: Option[MObject],
  guard: Option[MGuard],
  preState: MState,
  postState: MState,
  action: Option[MAction],
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = None
}
