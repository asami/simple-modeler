package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.values.PathName
import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * derived from SPackage and SMPackage.
 *
 * @since   Sep. 15, 2008
 *  version Jul. 13, 2011
 *  version Nov. 25, 2012
 *  version Jan.  4, 2020
 *  version May. 19, 2020
 *  version Jun. 13, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MPackage(
  override val designation: Designation,
  affiliation: MPackageRef,
  elements: Vector[MElement],
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = Some(affiliation)

  def getPackage(p: MPackageRef): Option[MPackage] = getPackage(p.pathName)

  def getPackage(pathname: String): Option[MPackage] = getPackage(PathName(pathname))

  def getPackage(p: PathName): Option[MPackage] = p.headOption match {
    case Some(name) =>
      elements.find(_.name == name).flatMap {
        case m: MPackage => m.getPackage(p.tail)
        case m => RAISE.invalidArgumentFault(s"Not package: $name -> ${m.show}")
      }
    case None => Some(this)
  }

  def getObject(p: MObjectRef): Option[MObject] = getPackage(p.packageRef).flatMap(_.getObject(p.objectName))

  def getObject(name: String): Option[MObject] = elements.find(_.name == name).map {
    case m: MObject => m
    case m => RAISE.invalidArgumentFault(s"Not object: $name -> ${m.show}")
  }
}

object MPackage {
  def apply(name: String, affiliation: MPackageRef, ps: Seq[MElement]): MPackage = MPackage(
    Designation(name),
    affiliation,
    ps.toVector
  )

  def default(ps: Seq[MElement]): MPackage = MPackage(
    Designation(""),
    MPackageRef.default,
    ps.toVector
  )
}
