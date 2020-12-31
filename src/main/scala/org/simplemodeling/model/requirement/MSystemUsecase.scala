package org.simplemodeling.model.requirement

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   May.  9, 2020
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MSystemUsecase(
  designation: Designation,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil,
  description: Description = Description.empty
) extends MRequirementUsecase {
}
