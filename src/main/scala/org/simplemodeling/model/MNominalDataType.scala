package org.simplemodeling.model

import org.goldenport.record.v2.DataType
import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * @since   Jul. 15, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
case class MNominalDataType(
  description: Description,
  affiliation: MPackageRef,
  datatype: DataType,
  constraints: List[MConstraint]
) extends MValue {
  val stereotypes: List[MStereotype] = Nil
  val base: Option[MObjectRef] = None
  val traits: List[MTraitRef] = Nil
  val powertypes: List[MPowertypeRef] = Nil
  val attributes: List[MAttribute] = List(
    MAttribute(
      Designation("value"),
      MDataType(datatype),
      MOne,
      constraints,
      None
    )
  )
  val operations: List[MOperation] = Nil
}
