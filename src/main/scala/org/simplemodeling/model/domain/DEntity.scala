package org.simplemodeling.model.domain

import org.simplemodeling.model._

/*
 * derived from DomainEntity since Nov. 22, 2007
 * derived from SMDomainEntity and DomainEntity.
 *
 * Nov. 12, 2010
 * @since   Sep. 10, 2008
 *  version Oct. 21, 2012
 * @version Aug. 10, 2019
 * @author  ASAMI, Tomoharu
 */
case class DEntity(
  stereotype: DStereotype,
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef]
) extends MEntity {
}
