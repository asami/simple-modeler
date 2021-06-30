package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from DomainActor since Nov. 22, 2007
 * Derived from DomainActor and SMDomainActor.
 * 
 * @since   Sep. 11, 2008
 *  version Jan. 18, 2009
 *  version Mar. 17, 2009
 *  version Sep. 18, 2011
 *  version Oct. 21, 2012
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Nov. 18, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MDomainActor(
  override val description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype] = Nil, //  stereotype: MDomainStereotype,
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef]
) extends MDomainEntity {
//  def designation: Designation = description.designation
}
