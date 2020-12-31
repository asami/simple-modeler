package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.Strings
import org.goldenport.values.Designation
import org.smartdox.Description
import org.smartdox.specdoc.SDFeatureSet
import org.simplemodeling.model._

// Derived from PPackageEntity: Jun. 13, 2011 - Nov. 22, 2012
/*
 * @since   Jan.  4, 2020
 *  version Jan.  5, 2020
 *  version Mar.  1, 2020
 *  version Apr. 30, 2020
 *  version May.  3, 2020
 *  version Jun. 13, 2020
 *  version Jul. 24, 2020
 *  version Aug.  2, 2020
 * @version Sep. 14, 2020
 * @author  ASAMI, Tomoharu
 */
case class PPackage(
  designation: Designation,
  description: Description,
  affiliation: MPackageRef,
  featureSet: SDFeatureSet,
  elements: Vector[PElement]
) extends PElement {
  def model(p: SimpleModel): MPackage = p.getPackage(qualifiedName) getOrElse ???

  def getPackage(name: String): Option[PPackage] =
    getPackage(Strings.totokens(name, "."))

  def getPackage(path: List[String]): Option[PPackage] = path match {
    case Nil => Some(this)
    case x :: xs =>
      elements.find(_.name == x).flatMap {
        case pkg: PPackage => pkg.getPackage(xs)
        case _ => None
      }
  }

  def packages: Vector[PPackage] = this +: elements.flatMap {
    case m: PPackage => m.packages
    case _ => Vector.empty
  }

  def getObject(name: String): Option[PObject] =
    getObject(Strings.totokens(name, "."))

  def getObject(path: List[String]): Option[PObject] = path match {
    case Nil => None
    case x :: Nil => elements.find(_.name == x).collect {
      case m: PObject => m
    }
    case x :: xs =>
      elements.find(_.name == x).flatMap {
        case pkg: PPackage => pkg.getObject(xs)
        case _ => None
      }
  }

  def getObject(pkg: String, name: String): Option[PObject] =
    getObject(Strings.totokens(pkg, "."), name)

  def getObject(pkg: List[String], name: String): Option[PObject] = pkg match {
    case Nil => elements.find(_.name == name).collect {
      case m: PObject => m
    }
    case x :: xs =>
      elements.find(_.name == x).flatMap {
        case pkg: PPackage => pkg.getObject(xs, name)
        case _ => None
      }
  }
}
