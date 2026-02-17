package org.simplemodeling.model

import org.goldenport.RAISE

/*
 * derived from ModelElement since Mar. 18, 2007.
 * derived from SObject and SMObject.
 *
 * @since   Sep. 10, 2008
 *  version Jul. 13, 2011
 *  version Sep. 18, 2011
 *  version Feb.  7, 2012
 *  version Apr.  8, 2012
 *  version Oct. 21, 2012
 *  version Nov. 25, 2012
 *  version Dec. 18, 2012
 *  version Jan. 10, 2013
 *  version Feb.  7, 2013
 *  version Aug.  8, 2019
 *  version Apr. 25, 2020
 *  version May. 17, 2020
 *  version Sep. 26, 2020
 * @version Feb. 14, 2026
 * @author  ASAMI, Tomoharu
 */
trait MObject extends MElement { // Classifier
  def affiliation: MPackageRef
  override def getAffiliation: Option[MPackageRef] = Some(affiliation)
  def packageName: String = affiliation.packageName
  def stereotypes: List[MStereotype]
  def base: Option[MObjectRef]
  def traits: List[MTraitRef]
  def powertypes: List[MPowertypeRef]
  def stateMachines: List[MStateMachineRef]
  def attributes: List[MAttribute]
  def associations: List[MAssociation]
  def operations: List[MOperation]
  def ports: List[MPort]
  def roles: List[MRoleRef]
  def services: List[MService]
  def rules: List[MRuleRef]
  def vouchers: List[MVoucherRef]
  // uses
  // participants
  // actions
  // displays

  def getBaseObject(implicit sm: SimpleModel): Option[MObject] = base.flatMap(sm.getObject)
  def derivedObjects(implicit sm: SimpleModel): List[MObject] = sm.takeDerivedObjects(this)

  /*
   * legacy
   */
  def typeName: String = ??? // TODO usage
  def kindName: String = ??? // TODO usage
  def powertypeName: String = ??? // TODO usage

  def toObjectRef: MObjectRef = MObjectRef(affiliation, name)
}

object MObject {
  case class Core(
    affiliation: MPackageRef,
    stereotypes: List[MStereotype] = Nil,
    base: Option[MObjectRef] = None,
    traits: List[MTraitRef] = Nil,
    powertypes: List[MPowertypeRef] = Nil,
    stateMachines: List[MStateMachineRef] = Nil,
    attributes: List[MAttribute] = Nil,
    associations: List[MAssociation] = Nil,
    operations: List[MOperation] = Nil,
    ports: List[MPort] = Nil,
    roles: List[MRoleRef] = Nil,
    services: List[MService] = Nil,
    rules: List[MRuleRef] = Nil,
    vouchers: List[MVoucherRef] = Nil
  )
  object Core {
    trait Holder {
      def objectCore: Core

      def affiliation: MPackageRef = objectCore.affiliation
      def stereotypes: List[MStereotype] = objectCore.stereotypes
      def base: Option[MObjectRef] = objectCore.base
      def traits: List[MTraitRef] = objectCore.traits
      def powertypes: List[MPowertypeRef] = objectCore.powertypes
      def stateMachines: List[MStateMachineRef] = objectCore.stateMachines
      def attributes: List[MAttribute] = objectCore.attributes
      def associations: List[MAssociation] = objectCore.associations
      def operations: List[MOperation] = objectCore.operations
      def ports: List[MPort] = objectCore.ports
      def roles: List[MRoleRef] = objectCore.roles
      def services: List[MService] = objectCore.services
      def rules: List[MRuleRef] = objectCore.rules
      def vouchers: List[MVoucherRef] = objectCore.vouchers
    }

    def create(
      pkg: MPackage,
      operations: Seq[MOperation] = Nil,
      services: Seq[MService] = Nil
    ): Core = Core(
      pkg.toPackageRef,
      operations = operations.toList,
      services = services.toList
    )
  }

  case class Instance(
    elementCore: MElement.Core,
    objectCore: MObject.Core
  ) extends MObject with MElement.Core.Holder with MObject.Core.Holder {
  }

  val entityId = MObject(MPackageRef.datatype, "EntityId")
  val record = MObject(MPackageRef.record, "Record")

  def apply(pkg: MPackageRef, name: String): MObject = Instance(
    MElement.Core(name),
    MObject.Core(pkg)
  )
}
