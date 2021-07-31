package org.simplemodeling.model.domain

import org.goldenport.collection.VectorMap
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * 
 * @since   Jun. 27, 2021
 *  version Jun. 30, 2021
 * @version Jul.  3, 2021
 * @author  ASAMI, Tomoharu
 */
class MDomainStateMachine(
  val description: Description,
  val affiliation: MPackageRef,
  val stereotypes: List[MStereotype] = Nil //  stereotype: MDomainStereotype,
) extends MStateMachine {
  private var _state_map: VectorMap[String, MState] = VectorMap.empty
  var subStateMachine: VectorMap[String, MDomainStateMachine] = VectorMap.empty

  def stateMap = _state_map

  def setStates(p: VectorMap[String, MState]) = {
    _state_map = p
    this
  }
}

object MDomainStateMachine {
  def create(name: String): MDomainStateMachine = {
    val desc = Description.name(name)
    val pkg = MPackageRef.default // TODO
    val stereotypes = Nil
    new MDomainStateMachine(desc, pkg, stereotypes)
  }
}
