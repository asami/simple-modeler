package org.simplemodeling.model

import org.goldenport.values.Designation
import org.simplemodeling.model._

/*
 * Derived from ActionStep and SMActionStep.
 * 
 * @since   Dec.  5, 2008
 * @version Jul. 25, 2020
 * @author  ASAMI, Tomoharu
 */
case class MActionStep(
  designation: Designation,
  affiliation: MPackageRef
) extends MStep {
  def getAffiliation = Some(affiliation)
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct

  def action: String = ???
}
