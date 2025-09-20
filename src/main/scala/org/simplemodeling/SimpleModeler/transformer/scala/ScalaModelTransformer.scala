package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model.MObject
import org.simplemodeling.SimpleModeler.generator.scala.model.SClassBase

/*
 * @since   Sep. 19, 2025
 * @version Sep. 19, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class ScalaModelTransformer() extends PartialFunction[(MObject, ScalaModelTransformer.Purpose), Consequence[SClassBase]] {
}

object ScalaModelTransformer {
  sealed trait Purpose
  object Purpose {
    val elements = Vector(Create, Update, Delete)

    case object Create extends Purpose
    case object Update extends Purpose
    case object Delete extends Purpose
  }
}
