package org.simplemodeling.model.domain

import org.simplemodeling.model._

/*
 * @since   Aug. 10, 2019
 * @version Aug. 10, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait DStereotype extends MStereotype {
}

case object DResourceStereotype extends DStereotype {
}

case object DVoucherStereotype extends DStereotype {
}
