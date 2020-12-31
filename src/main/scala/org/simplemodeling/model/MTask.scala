package org.simplemodeling.model

import org.goldenport.value._
import org.simplemodeling.model._

/*
 * Derived from STask and SMTask.

 * @since   Nov.  8, 2008
 *  version Dec. 10, 2008
 *  version Nov. 12, 2010
 *  version Nov.  4, 2012
 *  version May. 10, 2020
 * @version Jul. 25, 2020
 * @author  ASAMI, Tomoharu
 */
trait MTask extends MStoryObject {
  def base: Option[MObjectRef] = None
  def traits: List[MTraitRef] = Nil
  def powertypes: List[MPowertypeRef] = Nil
  def stateMachines: List[MStateMachineRef] = Nil
  def attributes: List[MAttribute] = Nil
  def associations: List[MAssociation] = Nil
  def operations: List[MOperation] = Nil
  def ports: List[MPort] = Nil
  def roles: List[MRoleRef] = Nil
  def services: List[MServiceRef] = Nil
  def rules: List[MRuleRef] = Nil
  def vouchers: List[MVoucherRef] = Nil
}

object MTask {
  sealed trait Kind extends NamedValueInstance {
  }

  case object IndependentTask extends Kind {
    val name = "independent"
  }

  case object PartTask extends Kind {
    val name = "part"
  }

  case object WholeTask extends Kind {
    val name = "whole"
  }

  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(IndependentTask, PartTask, WholeTask)
  }
}
