package org.simplemodeling.model.domain

import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   Feb.  9, 2026
 * @version Feb.  9, 2026
 * @author  ASAMI, Tomoharu
 */
case class MDomainComponent(
  description: Description,
  objectCore: MObject.Core,
  componentCore: MComponent.Core
) extends MComponent
    with MObject.Core.Holder
    with MComponent.Core.Holder {
}
