package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from FlowNode and SMFlowStep.
 * 
 * @since   Jan.  4, 2011
 *  version Mar. 26, 2011
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MFlowStep(
  designation: Designation,
  affiliation: MPackageRef
) extends MElement {
  def getAffiliation = Some(affiliation)
  def description: Description = Description.empty
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct
}
