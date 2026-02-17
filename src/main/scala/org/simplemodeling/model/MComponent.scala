package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from SComponent and SMComponent.
 * 
 * @since   Jan.  5, 2009
    version Aug.  7, 2009
 *  version Jul. 24, 2020
 * @version Feb.  9, 2026
 * @author  ASAMI, Tomoharu
 */
trait MComponent extends MObject {
  def entities: Vector[MEntity]
}

object MComponent {
  case class Core(
    entities: Vector[MEntity]
  )
  object Core {
    trait Holder {
      def componentCore: Core

      def entities = componentCore.entities
    }
  }
}
