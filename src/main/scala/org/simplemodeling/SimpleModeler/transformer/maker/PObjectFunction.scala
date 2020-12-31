package org.simplemodeling.SimpleModeler.transformer.maker

/*
 * Migrated from PObjectEntityFunction.
 * 
 * If target is only specific PObjectEntity class,
 * use GenericClassDefinition#package_children_entity.
 * If target are few specific PObjectEntity class,
 * use GenericClassDefinition#package_children_collect.
 * 
 * @since   Jul. 29, 2011
 *  version Aug. 21, 2011
 *  version Jan.  5, 2020
 * @version Mar.  1, 2020
 * @author  ASAMI, Tomoharu
 */
// XXX define all pobject classes
trait PObjectFunction[T] extends PartialFunction[PObject, T] {
  override def isDefinedAt(x: PObject) = true

  override def apply(objectentity: PObject): T = {
    objectentity match {
      case oe: PEntity => apply_EntityEntity(oe)
      case oe: PPackage => apply_PackageEntity(oe)
      case _ => apply_ObjectEntity(objectentity)
    }
  }

  protected def apply_EntityEntity(entity: PEntity): T
  protected def apply_PackageEntity(entity: PPackage): T
  protected def apply_ObjectEntity(entity: PObject): T
}
