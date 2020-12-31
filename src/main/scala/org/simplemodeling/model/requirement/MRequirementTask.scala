package org.simplemodeling.model.requirement

import org.goldenport.values.Designation
import org.smartdox.{Dox, Description}
import org.simplemodeling.model._

/*
 * Derived from RequirementTask and SMRequirementTask.
 * 
 * @since   Dec. 10, 2008
 *  version Dec. 18, 2010
 *  version Nov.  5, 2012
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Jul. 25, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MRequirementTask(
  designation: Designation,
  affiliation: MPackageRef,
  description: Description = Description.empty,
  stereotypes: List[MStereotype] = Nil
) extends MTask {
  final def userRequirementUsecasesLiteral: Dox = {
    // objects_literal(userRequirementUsecases)
    ???
  }
}
