package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * @since   Aug.  7, 2019
 *  version Nov.  4, 2019
 *  version May.  6, 2020
 *  version May. 25, 2020
 *  version Jun.  6, 2020
 * @version Sep. 27, 2020
 * @author  ASAMI, Tomoharu
 */
trait MObjectRef extends MReference {
  def packageRef: MPackageRef
  def packageName = packageRef.packageName
  def objectName: String
  lazy val pathName: String = s"${packageRef.pathName}.$objectName"

  lazy val target = MPointer(objectName, packageName)

  def toEntityRef: MEntityRef = this match {
    case m: MEntityRef => m
    case _ => MEntityRef(packageRef, objectName)
  }
}

object MObjectRef {
  case class PlainObjectRef(packageRef: MPackageRef, objectName: String) extends MObjectRef {
    val relationshipType: MRelationshipType = MRelationshipType(objectName, packageRef)
  }

  def apply(packageRef: MPackageRef, objectName: String): MObjectRef = PlainObjectRef(packageRef, objectName)

  def create(p: String): MObjectRef = {
    Strings.totokens(p, ".") match {
      case Nil => RAISE.syntaxErrorFault("empty object qualified name.")
      case x :: Nil => PlainObjectRef(MPackageRef.default, x)
      case xs => PlainObjectRef(MPackageRef(xs.init.mkString(".")), xs.last)
    }
  }
}
