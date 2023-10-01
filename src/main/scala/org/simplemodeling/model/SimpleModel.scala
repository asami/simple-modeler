package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.tree._

/*
 * @since   Aug.  8, 2019
 *  version May. 25, 2020
 *  version Jun.  6, 2020
 *  version Aug. 13, 2020
 *  version Sep. 21, 2020
 * @version Sep. 26, 2023
 * @author  ASAMI, Tomoharu
 */
case class SimpleModel(
  elements: Vector[MElement]
) extends GraphPart {
  lazy val tree: Tree[MElement] = {
    val r = new PlainTree[MElement]()
    for (x <- elements) {
      r.setContent(x.qualifiedPathName, x)
    }
    r
  }

  lazy val root: MPackage = {
    val pkg = MPackageRef.default
    val xs = tree.root.children.map(_build(pkg, _))
    MPackage.default(xs)
  }

  private def _build(pkg: MPackageRef, p: TreeNode[MElement]): MElement =
    if (p.isContainer)
      _build_package(pkg, p)
    else
      p.content match {
        case m => m // TODO
        case m => RAISE.noReachDefect(s"SimpleModel#_build: $m")
      }

  private def _build_package(pkg: MPackageRef, p: TreeNode[MElement]): MPackage = {
    val cpkg = pkg.child(p.name)
    val xs = p.children.map(_build(cpkg, _))
    MPackage(p.name, pkg, xs)
  }

  def getPackage(pathname: String): Option[MPackage] = root.getPackage(pathname)
  def getPackage(p: MPackageRef): Option[MPackage] = root.getPackage(p)
  def getObject(p: MObjectRef): Option[MObject] = root.getObject(p)
  def getEntity(p: MEntityRef): Option[MEntity] = getObject(p).collect {
    case m: MEntity => m
  }.orElse(RAISE.noReachDefect(s"SimpleModel#getEntity: ${p.pathName}"))

  def takeDerivedObjects(p: MObject): List[MObject] = {
    val qname = p.qualifiedName
    elements.flatMap {
      case m: MObject => m.getBaseObject(this).filter(_.qualifiedName == qname)
      case _ => None
    }.toList
  }
}
