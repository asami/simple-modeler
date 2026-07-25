package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.goldenport.record.v2._
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 20, 2025
 *  version Sep. 23, 2025
 *  version Feb. 18, 2026
 *  version Mar. 24, 2026
 *  version May. 22, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueUpdateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Update)
  override protected def sub_package_name: Option[String] = Some("update")

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    super.to_scala_core_parent(p).map {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.simplemodeling.model" =>
        TypeName.Plain(PackageName("org.simplemodeling.model"), "SimpleEntityUpdate")
      case m => m
    }

  override protected def transform_entity(
    p: MEntity,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] =
    super.transform_entity(p, purpose).map(
      _.map(_normalize_update_parameters).
        map(x => SimpleEntityScalaModelSupport.normalizeInput(x, "SimpleEntityUpdate"))
    )

  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence = {
    val xs = ps.filterNot(_is_id_attribute).toVector.map(to_parameter)
    ParameterSequence(xs)
  }

  override protected def to_parameter(p: MAttribute): Parameter = {
    val t = _update_type(to_typename(p))
    super.to_parameter(p).copy(typeName = t)
  }

  private def _is_id_attribute(p: MAttribute): Boolean =
    p.name == "id" || (p.attributeType match {
      case m: MDataType => m.datatype == XEntityId
      case _ => false
    })

  private def _update_type(p: TypeName): TypeName = {
    val update = TypeName.Plain(PackageName("org.simplemodeling.model.directive"), "Update")
    TypeName.Container(update, _unwrap_option(p))
  }

  private def _unwrap_option(p: TypeName): TypeName =
    p match {
      case m: TypeName.Container if m.isOption => m.containee
      case m => m
    }

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Update)

  private def _normalize_update_parameters(p: SClassBase): SClassBase =
    p match {
      case m: SCaseClass if _is_simple_entity_update_parent(m.core.parentClass) =>
        val params = m.core.parameterSequence.parameters
        val idparam = params.find(_.name.name == "id").getOrElse(_id_parameter())
        val (inheritedparams, ownparams) =
          params.filterNot(_.name.name == "id").partition(x => _inherited_simple_entity_keys.contains(x.name.name))
        val schemaattrs = ParameterSequence(inheritedparams).distillAttributes.attributes
        val compositeparams = Vector(
          _simple_object_parameter("nameAttributes", "NameAttributesUpdate"),
          _simple_object_parameter("descriptiveAttributes", "DescriptiveAttributesUpdate"),
          _simple_object_parameter("contentAttributes", "ContentAttributesUpdate"),
          _simple_object_parameter("lifecycleAttributes", "LifecycleAttributesUpdate"),
          _simple_object_parameter("publicationAttributes", "PublicationAttributesUpdate"),
          _simple_object_parameter("securityAttributes", "SecurityAttributesUpdate"),
          _simple_object_parameter("resourceAttributes", "ResourceAttributesUpdate"),
          _simple_object_parameter("auditAttributes", "AuditAttributesUpdate"),
          _simple_object_parameter("mediaAttributes", "MediaAttributesUpdate"),
          _simple_object_parameter("contextualAttribute", "ContextualAttributesUpdate")
        )
        val normalized = ParameterSequence(idparam +: (compositeparams ++ ownparams))
        val directive = m.core.directive.withSchemaAttributes(schemaattrs)
        m.copy(core = m.core.copy(parameterSequence = normalized, directive = directive))
      case _ =>
        p
    }

  private def _is_simple_entity_update_parent(p: Option[TypeName]): Boolean =
    p.exists {
      case TypeName.Plain(pkg, "SimpleEntityUpdate", _) if pkg.name == "org.simplemodeling.model" => true
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
      TypeName.Container(
        TypeName.Plain(PackageName("org.simplemodeling.model.directive"), "Update"),
        TypeName.Plain(PackageName("org.simplemodeling.model.datatype"), "EntityId")
      ),
      isAttribute = true,
      isDefault = false
    )

  private val _inherited_simple_entity_keys: Set[String] = Set(
    "id",
    "name",
    "title",
    "headline",
    "brief",
    "summary",
    "description",
    "lead",
    "abstract",
    "remarks",
    "tooltip",
    "contentAttributes",
    "content"
  )

  // def isDefinedAt(p: (MObject, Purpose)): Boolean =
  //   p match {
  //     case (_: MEntity, Purpose.Update) => true
  //     case _ => false
  //   }

  // def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
  //   p match {
  //     case (m: MEntity, Purpose.Update) => _transform(m)
  //     case _ => Consequence.noReachDefect(s"EntityValueUpdateScalaModelTransformer#apply")
  //   }

  // private def _transform(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
  //   Vector(_to_scala(p))
  // }

  // private def _to_scala(p: MEntity): SCaseClass = {
  //   val core = to_scala_core_subpackage(p, "update")
  //   SCaseClass(core)
  // }
}
