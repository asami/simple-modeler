package org.simplemodeling.SimpleModeler.generators.uml

import org.goldenport.value._

/*
 * @since   May. 17, 2020
 * @version May. 17, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait Perspective extends NamedValueInstance {
}

object Perspective extends EnumerationClass[Perspective] {
  val elements = Vector()
}

case object NonePerspective extends Perspective {
  val name = "none"
}

case object OverviewPerspective extends Perspective {
  val name = "overview"
}

case object HilightPerspective extends Perspective {
  val name = "hilight"
}

case object DetailPerspective extends Perspective {
  val name = "detail"
}
