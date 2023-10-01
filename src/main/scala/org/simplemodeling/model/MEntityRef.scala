package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * Derived from SMEntityType.
 * 
 * @since   Mar. 21, 2011
 *  version May.  6, 2020
 *  version May. 25, 2020
 *  version Sep. 27, 2020
 * @version Sep. 24, 2023
 * @author  ASAMI, Tomoharu
 */
case class MEntityRef(packageRef: MPackageRef, entityName: String) extends MObjectRef {
  val relationshipType = MRelationshipType(entityName, packageRef)
  def objectName = entityName
  def typeObject(implicit sm: SimpleModel): MEntity = sm.getEntity(this).getOrElse(
    RAISE.noReachDefect
  )
}

object MEntityRef {
  def create(p: String): MEntityRef = {
    Strings.totokens(p, ".") match {
      case Nil => RAISE.syntaxErrorFault("empty entity qualified name.")
      case x :: Nil => MEntityRef(MPackageRef.default, x)
      case xs => MEntityRef(MPackageRef(xs.init.mkString(".")), xs.last)
    }
  }

  def create(packagename: String, name: String): MEntityRef =
    MEntityRef(MPackageRef(packagename), name)
}
