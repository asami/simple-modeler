package org.simplemodeling.model

import org.goldenport.i18n.I18NString
import org.goldenport.values.PathName

/*
 * @since   Aug.  7, 2019
 *  version Nov.  3, 2019
 *  version Mar.  1, 2020
 *  version May. 18, 2020
 *  version Sep. 27, 2020
 *  version Oct.  4, 2020
 * @version Sep. 17, 2023
 * @author  ASAMI, Tomoharu
 */
case class MPackageRef(
  packageName: String
) extends MReference {
  lazy val relationshipType: MRelationshipType = MRelationshipType.createInInit(packageName)
  // override def targetName: String = packageName
  // override def targetPackageName: String = targetName // ???
  def isDefault: Boolean = this == MPackageRef.default

  def child(name: String): MPackageRef =
    if (isDefault)
      MPackageRef(name)
    else
      MPackageRef(s"${packageName}.${name}")

  lazy val pathName: PathName = PathName.createAbsolute(packageName, ".")
}

object MPackageRef {
  val default = MPackageRef("")

  def create(p: I18NString): MPackageRef = {
    val name = p.c
    if (name.isEmpty)
      default
    else
      MPackageRef(p.c)
  }

  def createWithLeaf(pathname: String): (MPackageRef, String) =
    pathname.lastIndexOf(".") match {
      case -1 => (default, pathname)
      case n => (MPackageRef(pathname.substring(0, n)), pathname.substring(n + 1))
    }
}
