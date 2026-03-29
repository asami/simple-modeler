package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 17, 2026
 * @version Mar. 30, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueAggregateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Aggregate)
  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Aggregate)

  override protected def effective_attributes(p: MObject): List[MAttribute] =
    p match {
      case m: MEntity =>
        val base = super.effective_attributes(m)
        val assocattrs = m.associations.map(_to_member_attribute)
        _merge_member_attributes(base, assocattrs)
      case _ =>
        super.effective_attributes(p)
    }

  // NOTE: Aggregate-specific DSL/model is not available yet.
  // Default: entity.aggregate.<Entity>
  // Non-default: entity.aggregate.<aggregate-name>.<Entity>
  override protected def transform_entity(p: MEntity, purpose: Purpose): Consequence[Vector[SClassBase]] = Consequence {
    val subpkg = _aggregate_package(_aggregate_name(p))
    val core = to_entity_value_core(p, Some(subpkg))
    Vector(SCaseClass(core.withEntityValue.withPurpose(purpose)))
  }

  // Future: resolve aggregate name from model metadata.
  private def _aggregate_name(p: MEntity): Option[String] = None

  private def _aggregate_package(name: Option[String]): String =
    name.flatMap(_token_opt).fold("aggregate")(x => s"aggregate.$x")

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

  private def _to_member_attribute(p: MAssociation): MAttribute =
    MAttribute(
      designation = p.designation,
      attributeType = MObjectAttributeType(p.objectRef),
      multiplicity = p.multiplicity,
      constraints = Nil,
      column = None,
      readonly = true,
      description = p.description
    )

  private def _merge_member_attributes(
    base: List[MAttribute],
    members: List[MAttribute]
  ): List[MAttribute] = {
    val m = scala.collection.mutable.LinkedHashMap[String, MAttribute]()
    base.foreach(x => m.update(x.name, x))
    members.foreach(x => m.update(x.name, x))
    m.values.toList
  }
}
