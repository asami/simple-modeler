package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.RAISE
import org.smartdox.specdoc._
import org.simplemodeling.model._
import org.simplemodeling.model.domain._
import org.simplemodeling.SimpleModeler.transformer.maker.mobject._

/*
 * @since   Apr. 30, 2020
 *  version May.  3, 2020
 *  version Jul. 19, 2020
 *  version Aug.  1, 2020
 * @version Sep. 14, 2020
 * @author  ASAMI, Tomoharu
 */
case class PModel(
  root: PPackage
) {
  def packageOfObject(p: PObject): PPackage = packageByRef(p.affiliation)

  def packageByRef(p: MPackageRef): PPackage = getPackage(p.packageName) getOrElse RAISE.noReachDefect

  def getPackage(p: String): Option[PPackage] = root.getPackage(p)

  def packages: Vector[PPackage] = root.packages

  def getObject(p: PObjectReferenceType): Option[PObject] = getObject(p.packageName, p.name)

  def getObject(p: String): Option[PObject] = root.getObject(p)

  def getObject(pkg: String, name: String): Option[PObject] = root.getObject(pkg, name)
}

object PModel {
  def parse(p: SimpleModel): PModel = {
    val root = _parse(p.root)
    PModel(root)
  }

  private def _parse(p: MPackage): PPackage = {
    val xs = p.elements.map(_parse)
    PPackage(p.designation, p.description, p.affiliation, SDFeatureSet.empty, xs)
  }

  private def _parse(p: MElement): PElement = p match {
    case m: MEntity => MPEntity(m)
  }
}
