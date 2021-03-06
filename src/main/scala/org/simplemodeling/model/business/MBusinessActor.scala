package org.simplemodeling.model.business

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from BusinessActor and SMBusinessActor.
 * 
 * @since   Nov.  4, 2012
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MBusinessActor(
  override val designation: Designation,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype],
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef],
  description: Description = Description.empty
) extends MBusinessEntity {
}
