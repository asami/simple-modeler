package org.simplemodeling.model

import org.smartdox._
import org.goldenport.values.Designation

/*
 * derived from SMRelationshipType.
 *
 * @since   Nov. 22, 2008
 *  version Sep. 27, 2020
 * @version Oct.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait MRelationshipType extends MElement {
  val getAffiliation = None
  def description: Description = Description.empty
  def target: MPointer
}

object MRelationshipType {
  case class Root(
    designation: Designation
  ) extends MRelationshipType {
    val target: MPointer = MPointer(name, "")
  }

  case class Instance(
    designation: Designation,
    packageRef: MPackageRef
  ) extends MRelationshipType {
    val target: MPointer = MPointer(name, packageRef.targetName)
  }

  def apply(name: String, pkg: MPackageRef): MRelationshipType = Instance(Designation(name), pkg)
  def apply(pathname: String): MRelationshipType = {
    val (pkg, name) = MPackageRef.createWithLeaf(pathname)
    apply(name, pkg)
  }

  def createInInit(pathname: String): MRelationshipType =
    if (pathname.isEmpty)
      Root(Designation(""))
    else
      apply(pathname)
}
