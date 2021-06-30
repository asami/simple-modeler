package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from DomainRole since Nov. 25, 2007
 * Derived from DomainRole and SMDomainRole.
 * 
 * @since   Sep. 11, 2008
 *  version Feb. 27, 2009
 *  version Nov. 12, 2010
 *  version Sep. 18, 2011 
 *  version Oct. 21, 2012
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MDomainRole(
  override val designation: Designation,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil,
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef],
  description: Description = Description.empty
) extends MDomainEntity {
}
