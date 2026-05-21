package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose

/*
 * @since   Sep. 23, 2025
 *  version Sep. 25, 2025
 *  version Feb. 26, 2026
 * @version May. 22, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class EntityCaseClassScalaModelTransformer() extends CaseClassScalaModelTransformer {
  protected def is_accept_object(p: MObject): Boolean = p.isInstanceOf[MEntity]

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MEntity, purpose) if is_accept_purpose(purpose) => transform_entity(m, purpose)
      case _ => Consequence.noReachDefect(s"EntityValueCreateScalaModelTransformer#apply")
    }

  protected def transform_entity(p: MEntity, purpose: Purpose): Consequence[Vector[SClassBase]] = Consequence {
    Vector(_to_scala(p, purpose))
  }

  protected final def to_entity_value_core(
    p: MEntity,
    subpkg: Option[String]
  ): ClassCore = {
    val entitysubpkg = subpkg.fold("entity")(x => s"entity.$x")
    val ownerpkg = if (p.packageName.isEmpty) "entity" else s"${p.packageName}.entity"
    val core = to_scala_core_subpackage(p, entitysubpkg)
    val owner = TypeName.Plain(PackageName(ownerpkg), p.name)
    core.copy(directive = core.directive.withCanonicalSchemaOwner(owner))
  }

  private def _to_scala(p: MEntity, purpose: Purpose): SCaseClass = {
    val core = to_entity_value_core(p, sub_package_name)
    SCaseClass(core.withEntityValue.withPurpose(purpose))
  }
}
