package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 19, 2025
 *  version Sep. 23, 2025
 *  version Mar. 24, 2026
 *  version May. 22, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Plain)
  override protected def transform_entity(
    p: MEntity,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] =
    super.transform_entity(p, purpose).map(_.map(_normalize_plain_parameters))
  // def isDefinedAt(p: (MObject, Purpose)): Boolean =
  //   p match {
  //     case (_: MEntity, Purpose.Plain) => true
  //     case _ => false
  //   }

  // def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
  //   p match {
  //     case (m: MEntity, Purpose.Plain) => _transform(m)
  //     case _ => Consequence.noReachDefect(s"EntityValueScalaModelTransformer#apply")
  //   }

  // private def _transform(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
  //   Vector(_to_scala(p))
  // }

  // private def _to_scala(p: MEntity): SCaseClass = {
  //   val core = to_scala_core(p)
  //   SCaseClass(core)
  // }

  private def _normalize_plain_parameters(p: SClassBase): SClassBase =
    SimpleEntityScalaModelSupport.normalizeOutput(p)
}
