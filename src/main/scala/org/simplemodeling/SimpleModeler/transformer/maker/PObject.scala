package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.RAISE
import org.simplemodeling.model.{MObject, MPackageRef}
import org.simplemodeling.model.MStereotype

/*
 * Derived from PObjectEntity: Apr. 21, 2011 - Feb. 7, 2013.
 *
 * @since   Dec. 22, 2019
 *  version Dec. 31, 2019
 *  version Jan.  1, 2020
 *  version Jan.  5, 2020
 *  version Mar.  2, 2020
 *  version Apr. 30, 2020
 *  version May.  3, 2020
 *  version Aug.  3, 2020
 * @version Dec. 20, 2020
 * @author  ASAMI, Tomoharu
 */
trait PObject extends PElement {
  def affiliation: MPackageRef
  def packageName: String = affiliation.packageName
  def xmlNamespace: String = packageName // TODO
  def attributes: List[PAttribute]
  def operations: List[POperation]
  def asciiName: String = name // TODO
  def classNameBase: String = name // TODO
  def modelObject: MObject = RAISE.notImplementedYetDefect
  def isId: Boolean = getIdAttr.isDefined
  def idAttr: PAttribute = getIdAttr.getOrElse(RAISE.noReachDefect)
  lazy val getIdAttr: Option[PAttribute] = attributes.find(_.isId)
  def getBaseObjectType: Option[PObjectReferenceType]
  def getTraitObjects: List[PObjectReferenceType]
  def associationEntityAttributes: List[PAttribute]
  //  def modelPackage: Option[MPackage]
  def stereotypes: List[MStereotype]
  def nameName: String = ???
  def getNameName: Option[String] = ???
  def getBaseObject(model: PModel): Option[PObject] = getBaseObjectType.flatMap(model.getObject)
  def wholeAttributes: List[PAttribute] = ???
  def wholeOperations: List[POperation] = ???
  def isImmutable: Boolean = false // TODO
  def getTraitNames: List[String] = getTraitObjects.map(_.name)
  def sqlTableName: String = ???
  def hasInheritancePowertypeExcludingBaseClasses: Boolean = ???
  def idAttrOption: Option[PAttribute] = ???
  def wholeAttributesWithoutId: List[PAttribute] = ???
  def isAncestor(p: PEntity): Boolean = ???
  def nameAttr: PAttribute = ???
}
