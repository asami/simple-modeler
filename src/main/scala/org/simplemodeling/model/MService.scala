package org.simplemodeling.model

import org.simplemodeling.model._
import org.smartdox.Description

/*
 * Derived from SService and SMService.
 * 
 * @since   Sep. 11, 2008
 *  version Oct. 12, 2008
 *  version Jan. 18, 2009
 *  version Nov.  9, 2012
 *  version May. 10, 2020
 * @version Feb. 11, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class MService extends MObject
    with MElement.Core.Holder
    with MObject.Core.Holder
    with MService.Core.Holder {
}

object MService {
  case class Core() {
  }
  object Core {
    trait Holder {
      def serviceCore: Core
    }
  }

  case class Instance(
    elementCore: MElement.Core,
    objectCore: MObject.Core,
    serviceCore: Core
  ) extends MService {
  }

  def apply(pkg: MPackage, name: String, ops: Seq[MOperation]): MService =
    apply(pkg, name, ops, Description.name(name))

  def apply(pkg: MPackage, name: String, ops: Seq[MOperation], description: Description): MService =
    Instance(
      MElement.Core(description),
      MObject.Core.create(pkg, ops),
      Core()
    )
}
