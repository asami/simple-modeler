package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Apr.  2, 2026
 *  version May. 22, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueProjectionScalaModelTransformer(
  projectionName: Option[String]
) extends EntityCaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.View)
  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.View)

  override protected def transform_entity(p: MEntity, purpose: Purpose): Consequence[Vector[SClassBase]] = Consequence {
    val subpkg = _view_package(projectionName)
    val ownerpkg = if (p.packageName.isEmpty) "entity" else s"${p.packageName}.entity"
    val owner = TypeName.Plain(PackageName(ownerpkg), p.name)
    val base = to_scala_core_subpackage(p, subpkg)
    val core = base.copy(directive = base.directive.withCanonicalSchemaOwner(owner))
    Vector(SCaseClass(core.withEntityValue.withPurpose(purpose))).map(_normalize_projection_parameters)
  }

  private def _normalize_projection_parameters(p: SClassBase): SClassBase =
    SimpleEntityScalaModelSupport.normalizeOutput(p, _value_package_name)

  protected final def _view_package(name: Option[String]): String =
    name.flatMap(_token_opt).fold("entity.view")(x => s"entity.view.$x")

  protected final def _token_opt(name: String): Option[String] =
    Option(name).map(_.trim).filter(_.nonEmpty).map(_package_token)

  protected final def _package_token(name: String): String = {
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

  private def _value_package_name(typename: String): String =
    typename match {
      case "NameAttributes" =>
        projectionName.flatMap(_token_opt).fold("org.simplemodeling.model.value") { x =>
          s"org.simplemodeling.model.value.$x"
        }
      case _ =>
        "org.simplemodeling.model.value"
    }

}
