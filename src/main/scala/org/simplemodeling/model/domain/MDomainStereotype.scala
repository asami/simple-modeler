package org.simplemodeling.model.domain

import org.simplemodeling.model._

/*
 * @since   Aug. 10, 2019
 *  version Nov.  2, 2019
 *  version May.  9, 2020
 * @version Jun.  6, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait MDomainStereotype extends MStereotype {
}

case object MDomainResourceStereotype extends MDomainStereotype {
  val name = "resource"
}
case object MDomainActorStereotype extends MDomainStereotype {
  val name = "actor"
}
case object MDomainEventStereotype extends MDomainStereotype {
  val name = "event"
}
case object MDomainTraitStereotype extends MDomainStereotype {
  val name = "trait"
}
case object MDomainVoucherStereotype extends MDomainStereotype {
  val name = "voucher"
}
