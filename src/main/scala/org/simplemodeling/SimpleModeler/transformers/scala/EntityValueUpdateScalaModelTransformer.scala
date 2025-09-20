package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 20, 2025
 * @version Sep. 20, 2025
 * @author  ASAMI, Tomoharu
 */
class EntityValueUpdateScalaModelTransformer() extends ScalaModelTransformer() {
  def isDefinedAt(p: (MObject, Purpose)): Boolean =
    p match {
      case (_: MEntity, Purpose.Update) => true
      case _ => false
    }

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MEntity, Purpose.Update) => _transform(m)
      case _ => Consequence.noReachDefect(s"EntityValueUpdateScalaModelTransformer#apply")
    }

  private def _transform(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
    Vector(_to_scala(p))
  }

  private def _to_scala(p: MEntity): SCaseClass = {
    val core = to_scala_core_subpackage(p, "update")
    SCaseClass(core)
  }
}
