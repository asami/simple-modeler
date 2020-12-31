package org.simplemodeling.SimpleModeler.transformer.maker

import org.simplemodeling.model._

/*
 * @since   Dec. 31, 2019
 *  version Jan.  1, 2020
 *  version Jan.  5, 2020
 *  version Mar.  2, 2020
 *  version Apr. 28, 2020
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait PAttribute extends PElement {
  def attributeType: PObjectType
  def isComposition: Boolean = false // TODO
  def useOwnedProperty: Boolean = false // TODO
  def typeName: String = attributeType.objectTypeName // TODO container
  def elementTypeName: String = typeName
  def inject: Boolean = false // TODO
  def isDerive: Boolean = false // TODO
  def isMulti: Boolean = false // TODO
  def isHasMany: Boolean = isMulti
  def isQueryReference: Boolean = ???
  // GenericClassDefinition
    // attr.modelAssociation != null && 
    // attr.modelAssociation.isQueryReference
  def isAggregation: Boolean = ???
  def jpaTypeName: String = ???
  def jpaElementTypeName: String = ???
  def readonly: Boolean
  def attributeKind: MAttributeKind = NullAttributeKind
  def idPolicy: MIdPolicy = ???
  def isId: Boolean = name.equalsIgnoreCase("id") // TODO
  def jdoTypeName: String = ???
  def jdoElementTypeName: String = ???
  def isOwnedProperty: Boolean = ???
  def isSingle: Boolean = ???
  def deriveExpression: Option[PExpression] = ???
  def isEntity: Boolean = ???
  def platformParticipation: Option[PParticipation] = None
  def getEntity: Option[PEntity] = None
  def sqlColumnName: String = ???
  def getSqlDatatypeName: Option[String] = ???
  def multiplicity: PMultiplicity = ???
  def sqlAutoId: Boolean = ???
  def isParticipation: Boolean = ???
  def isEntityReference: Boolean = ???
  def isLogicalDelete: Boolean = ???
  def isDataType: Boolean = ???
  def associationKind: MAssociation.Kind = ???
  // GenericClassDefinition
    // attr.modelAssociation != null && 
    // attr.modelAssociation.isComposition &&
    // true
//    modelEntity != null &&
//    modelEntity.appEngine.use_owned_property
}

