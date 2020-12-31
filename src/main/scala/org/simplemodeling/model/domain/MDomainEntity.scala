package org.simplemodeling.model.domain

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.parser.SimpleModelParser.CreateObjectCommand

/*
 * derived from DomainEntity since Nov. 22, 2007
 * derived from SMDomainEntity and DomainEntity.
 *
 * @since   Sep. 10, 2008
 *  version Nov. 12, 2010
 *  version Sep. 18, 2011
 *  version Oct. 21, 2012
 *  version Aug. 10, 2019
 *  version Nov.  3, 2019
 *  version Dec. 15, 2019
 *  version Apr. 25, 2020
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  8, 2020
 * @version Nov. 18, 2020
 * @author  ASAMI, Tomoharu
 */
trait MDomainEntity extends MEntity {
}

object MDomainEntity {
  def apply(
    description: Description,
    affiliation: MPackageRef,
    stereotype: MDomainStereotype,
    base: Option[MObjectRef],
    traits: List[MTraitRef],
    powertypes: List[MPowertypeRef],
    attributes: List[MAttribute],
    associations: List[MAssociation],
    operations: List[MOperation],
    stateMachines: List[MStateMachineRef]
  ): MDomainEntity = stereotype match {
    case MDomainResourceStereotype => MDomainResource(
      description,
      affiliation,
      List(stereotype),
      base,
      traits,
      powertypes,
      attributes,
      associations,
      operations,
      stateMachines
    )
    case MDomainActorStereotype => MDomainActor(
      description,
      affiliation,
      List(stereotype),
      base,
      traits,
      powertypes,
      attributes,
      associations,
      operations,
      stateMachines
    )
    case MDomainEventStereotype => MDomainEvent(
      description,
      affiliation,
      List(stereotype),
      base,
      traits,
      powertypes,
      attributes,
      associations,
      operations,
      stateMachines
    )
    // case MDomainTraitStereotype => MDomainTrait(
    //   designation,
    //   affiliation,
    //   stereotype,
    //   base,
    //   traits,
    //   powertypes,
    //   attributes,
    //   associations,
    //   operations,
    //   stateMachines
    // )
    // case MDomainVoucherStereotype => MDomainVoucher(
    //   designation,
    //   affiliation,
    //   stereotype,
    //   base,
    //   traits,
    //   powertypes,
    //   attributes,
    //   associations,
    //   operations,
    //   stateMachines
    // )
    case _ => RAISE.noReachDefect("MDomainEntity")
  }

  def apply(p: CreateObjectCommand): MDomainEntity = apply(
    p.description,
    p.affiliation,
    p.stereotype,
    p.base,
    p.traits,
    p.powertypes,
    p.attributes,
    p.associations,
    p.operations,
    p.stateMachines
  )
}
