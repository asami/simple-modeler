package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * derived from DomainResource since Nov. 22, 2007
 * derived from DomainResource and SMDomainResource.
 * 
 *
 * @since   Sep. 11, 2008
 *  version Oct. 18, 2008
 *  version Sep. 18, 2011
 *  version Oct. 21, 2012
 *  version Aug. 10, 2019
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Nov. 18, 2020
 * @author  ASAMI, Tomoharu
 */
case class MDomainResource(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil,
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef]
) extends MDomainEntity {
  def designation: Designation = description.designation getOrElse Designation.empty
}
