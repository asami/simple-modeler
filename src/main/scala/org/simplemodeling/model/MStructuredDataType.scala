package org.simplemodeling.model

import org.smartdox.Description

/*
 * @since   Jul.  9, 2026
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
case class MStructuredDataType(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype],
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  operations: List[MOperation]
) extends MValue {
}
