package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * 
 * @since   Jun. 27, 2021
 * @version Jun. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class MDomainStateMachine(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil //  stereotype: MDomainStereotype,
) extends MStateMachine {
}
