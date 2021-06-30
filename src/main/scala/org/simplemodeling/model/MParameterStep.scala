package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Dox
import org.simplemodeling.model._

/*
 * Derived from SParameterStep and SMParameterStep.
 * 
 * @since   Dec. 12, 2008
 *  version Jul. 27, 2020
 *  version Aug.  8, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MParameterStep(
  override val designation: Designation,
  affiliation: MPackageRef
) extends MStep {
  def getAffiliation = Some(affiliation)
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct

  final def getVoucherTerm: Dox = {
    // new SMTermRef(dslParameterStep.document)
    ???
  }

  final def getParameterTerm: Dox = {
    // dslParameterStep.parameter
    ???
  }
}
