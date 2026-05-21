package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 17, 2026
 *  version Mar. 30, 2026
 *  version Apr.  2, 2026
 * @version May. 22, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueAggregateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Aggregate)
  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Aggregate)

  override protected def effective_attributes(p: MObject): List[MAttribute] =
    p match {
      case m: MEntity =>
        val base = super.effective_attributes(m)
        val assocattrs = m.associations.map(_to_member_attribute(_, _aggregate_package(_aggregate_name(m))))
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
    Vector(SCaseClass(core.withEntityValue.withPurpose(purpose))).map(_normalize_parameters)
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

  private def _to_member_attribute(
    p: MAssociation,
    aggregatepkg: String
  ): MAttribute =
    MAttribute(
      designation = p.designation,
      attributeType = MObjectAttributeType(_aggregate_member_ref(p.objectRef, aggregatepkg)),
      multiplicity = p.multiplicity,
      constraints = Nil,
      column = None,
      readonly = true,
      description = p.description
    )

  private def _aggregate_member_ref(
    p: MObjectRef,
    aggregatepkg: String
  ): MObjectRef = {
    val pkg =
      if (p.packageName.isEmpty)
        s"entity.${aggregatepkg}"
      else
        s"${p.packageName}.entity.${aggregatepkg}"
    MObjectRef(MPackageRef(pkg), p.objectName)
  }

  private def _merge_member_attributes(
    base: List[MAttribute],
    members: List[MAttribute]
  ): List[MAttribute] = {
    val m = scala.collection.mutable.LinkedHashMap[String, MAttribute]()
    base.foreach(x => m.update(x.name, x))
    members.foreach(x => m.update(x.name, x))
    m.values.toList
  }

  private def _normalize_parameters(p: SClassBase): SClassBase =
    p match {
      case m: SCaseClass if _is_simple_entity_parent(m.core.parentClass) =>
        val params = m.core.parameterSequence.parameters
        val idparam = params.find(_.name.name == "id").getOrElse(_id_parameter())
        val ownparams = params.filterNot(x => _inherited_simple_entity_keys.contains(x.name.name))
        val compositeparams = Vector(
          _simple_object_parameter("nameAttributes", "NameAttributes"),
          _simple_object_parameter("descriptiveAttributes", "DescriptiveAttributes"),
          _simple_object_parameter("contentAttributes", "ContentAttributes"),
          _simple_object_parameter("lifecycleAttributes", "LifecycleAttributes"),
          _simple_object_parameter("publicationAttributes", "PublicationAttributes"),
          _simple_object_parameter("securityAttributes", "SecurityAttributes"),
          _simple_object_parameter("resourceAttributes", "ResourceAttributes"),
          _simple_object_parameter("auditAttributes", "AuditAttributes"),
          _simple_object_parameter("mediaAttributes", "MediaAttributes"),
          _simple_object_parameter("contextualAttribute", "ContextualAttributes")
        )
        val normalized = ParameterSequence(idparam +: (compositeparams ++ ownparams))
        m.copy(core = m.core.copy(parameterSequence = normalized))
      case _ =>
        p
    }

  private def _is_simple_entity_parent(p: Option[TypeName]): Boolean =
    p.exists {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.simplemodeling.model" => true
      case _ => false
    }

  private def _simple_object_parameter(name: String, typename: String): Parameter =
    Parameter(
      ParameterName(name),
      TypeName.Plain(PackageName("org.simplemodeling.model.value"), typename),
      isAttribute = true,
      isDefault = false
    )

  private def _id_parameter(): Parameter =
    Parameter(
      ParameterName("id"),
      TypeName.Plain(PackageName("org.simplemodeling.model.datatype"), "EntityId"),
      isAttribute = true,
      isDefault = false
    )

  private val _inherited_simple_entity_keys: Set[String] = Set(
    "id",
    "name",
    "title",
    "contentAttributes",
    "content"
  )
}
