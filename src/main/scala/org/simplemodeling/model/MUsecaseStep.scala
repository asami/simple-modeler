package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Dox
import org.simplemodeling.model._

/*
 * Derived from SUsecaseStep and SMUsecaseStep.
 * 
 * @since   Dec. 10, 2008
 * @version Jul. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class MUsecaseStep(
  designation: Designation,
  affiliation: MPackageRef
) extends MStep {
  def getAffiliation = Some(affiliation)
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct

  final def getUsecaseTerm: Dox = ??? // new SMTermRef(dslUsecase)
}
