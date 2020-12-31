package org.simplemodeling.model.domain

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.parser.SimpleModelParser.CreateObjectCommand

/*
 * Derived from DomainDocument since Nov. 23, 2007
 * Derived from DomainDocument and SMDomainDocument.
 * 
 * @since   Sep. 11, 2008
 *  version Mar. 17, 2009
 *  version Apr. 17, 2011
 *  version May. 17, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Nov. 18, 2020
 * @author  ASAMI, Tomoharu
 */
case class MDomainVoucher(
  description: Description,
  affiliation: MPackageRef,
  stereotypes: List[MStereotype],
  base: Option[MObjectRef],
  traits: List[MTraitRef],
  powertypes: List[MPowertypeRef],
  attributes: List[MAttribute],
  operations: List[MOperation]
) extends MVoucher {
  def designation: Designation = description.designation getOrElse Designation.empty
}
object MDomainVoucher {
  def apply(p: CreateObjectCommand): MDomainVoucher = {
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
