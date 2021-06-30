package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.parser.SimpleModelParser.CreateObjectCommand

/*
 * Derived from DomainValue since Nov. 23, 2007
 * Derived from DomainValue and SMDomainDocument.
 * 
 * @since   Sep. 10, 2008
 *  version Nov. 13, 2010
 *  version Apr. 17, 2011
 *  version Sep. 19, 2011
 *  version Oct. 21, 2012
 *  version Jul. 24, 2020
 *  version Aug.  1, 2020
 *  version Nov. 18, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MDomainValue(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype],
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  operations: List[MOperation]
) extends MValue {
  // def designation: Designation = description.designation
}
object MDomainValue {
  def apply(p: CreateObjectCommand): MDomainValue = {
    // TODO precondition p.stateMachines
    // TODO precondition p.associations
    apply(
      p.description,
      p.affiliation,
      List(p.stereotype),
      p.base,
      p.traits,
      p.powertypes,
      p.attributes,
      p.operations
    )
  }
}
