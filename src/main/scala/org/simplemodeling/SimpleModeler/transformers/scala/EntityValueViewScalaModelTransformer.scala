package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 20, 2025
 *  version Mar. 17, 2026
 * @version Mar. 17, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueViewScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.View)
  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.View)

  // NOTE: View-specific DSL/model is not available yet.
  // Default: entity.view.<Entity>
  // Non-default: entity.view.<view-name>.<Entity>
  override protected def transform_entity(p: MEntity, purpose: Purpose): Consequence[Vector[SClassBase]] = Consequence {
    val subpkg = _view_package(_view_name(p))
    val core = to_entity_value_core(p, Some(subpkg))
    Vector(SCaseClass(core.withEntityValue.withPurpose(purpose)))
  }

  // Future: resolve view name from model metadata.
  private def _view_name(p: MEntity): Option[String] = None

  private def _view_package(name: Option[String]): String =
    name.flatMap(_token_opt).fold("view")(x => s"view.$x")

  private def _token_opt(name: String): Option[String] =
    Option(name).map(_.trim).filter(_.nonEmpty).map(_package_token)

  private def _package_token(name: String): String = {
    val b = new StringBuilder
    name.zipWithIndex.foreach { case (c, i) =>
      if (
        c.isUpper && i > 0 &&
        (name.charAt(i - 1).isLower || (i + 1 < name.length && name.charAt(i + 1).isLower))
      ) {
        b.append('_')
      }
      b.append(c.toLower)
    }
    b.toString
  }
}
