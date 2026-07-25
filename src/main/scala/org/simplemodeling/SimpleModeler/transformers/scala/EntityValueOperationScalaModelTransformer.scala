package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 20, 2025
 *  version Sep. 23, 2025
 *  version May. 22, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueOperationScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Operation)
  override protected def sub_package_name: Option[String] = Some("operation")

  override protected def transform_entity(
    p: MEntity,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] =
    super.transform_entity(p, purpose).map(_.map(_normalize_parameters))

  // def isDefinedAt(p: (MObject, Purpose)): Boolean =
  //   p match {
  //     case (_: MEntity, Purpose.Operation) => true
  //     case _ => false
  //   }

  // def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
  //   p match {
  //     case (m: MEntity, Purpose.Operation) => _transform(m)
  //     case _ => Consequence.noReachDefect(s"EntityValueOperationScalaModelTransformer#apply")
  //   }

  // private def _transform(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
  //   Vector(_to_scala(p))
  // }

  // private def _to_scala(p: MEntity): SCaseClass = {
  //   val core = to_scala_core_subpackage(p, "operation")
  //   SCaseClass(core)
  // }

  private def _normalize_parameters(p: SClassBase): SClassBase =
    SimpleEntityScalaModelSupport.normalizeOutput(p)
}
