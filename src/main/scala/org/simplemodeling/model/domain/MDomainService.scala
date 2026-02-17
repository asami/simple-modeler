package org.simplemodeling.model.domain

import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   Feb. 10, 2026
 * @version Feb. 10, 2026
 * @author  ASAMI, Tomoharu
 */
case class MDomainService(
  elementCore: MElement.Core,
  objectCore: MObject.Core,
  serviceCore: MService.Core
) extends MService
    with MObject.Core.Holder
    with MService.Core.Holder {
}
