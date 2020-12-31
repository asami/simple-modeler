package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from DomainSummary since Nov. 22, 2007
 * Derived from DomainSummary and SMDomainSummary.
 * 
 * @since   Sep. 15, 2008
 *  version Jan. 18, 2009
 *  version Mar. 17, 2009
 *  version Sep. 18, 2011
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MDomainSummary(
  designation: Designation,
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
