package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.parser.SimpleModelParser.CreateObjectCommand

/*
 * Derived from DomainTrait and SMDomainTrait.
 * 
 * @since   Oct. 15, 2012
 *  version Nov.  9, 2012
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Nov. 18, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MDomainTrait(
  override val description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype],
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  associations: List[MAssociation],
  operations: List[MOperation],
  stateMachines: List[MStateMachineRef]
) extends MTrait {
  // def designation: Designation = description.designation
}

object MDomainTrait {
  def apply(p: CreateObjectCommand): MDomainTrait = apply(
    p.description,
    p.affiliation,
    List(p.stereotype),
    p.base,
    p.traits,
    p.powertypes,
    p.attributes,
    p.associations,
    p.operations,
    p.stateMachines
  )
}
