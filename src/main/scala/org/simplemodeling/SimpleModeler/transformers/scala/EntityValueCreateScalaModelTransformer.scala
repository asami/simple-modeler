package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.goldenport.record.v2._
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 19, 2025
 *  version Sep. 23, 2025
 *  version Feb. 27, 2026
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueCreateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Create)
  override protected def sub_Package_Name: Option[String] = Some("create")

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    super.to_scala_core_parent(p).map {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.goldenport.model" =>
        TypeName.Plain(PackageName("org.goldenport.model"), "SimpleEntityCreate")
      case m => m
    }

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Create)

  override protected def transform_entity(
    p: MEntity,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] =
    super.transform_entity(p, purpose).map(_.map(_normalize_create_parameters))

  override protected def to_typename(p: MDataType): TypeName =
    p.datatype match {
      case XEntityId => TypeName.option(super.to_typename(p))
      case m => super.to_typename(p)
    }
//    TypeName.Primitive.createMarshalling(p)

  override protected def to_parameter(p: MAttribute): Parameter = {
    val base = super.to_parameter(p)
    if (_is_autocomplement_target(p))
      base.typeName match {
        case m: TypeName.Container if m.isOption => base
        case _ => base.copy(typeName = TypeName.option(base.typeName))
      }
    else
      base
  }

  private def _is_autocomplement_target(p: MAttribute): Boolean = {
    val key = p.name.toLowerCase(java.util.Locale.ROOT)
    p.isRequired && _autocomplement_target_keys.contains(key)
  }

  private val _autocomplement_target_keys: Set[String] = Set(
    "id",
    "name",
    "createdat",
    "updatedat",
    "createdby",
    "updatedby",
    "poststatus",
    "aliveness",
    "traceid",
    "correlationid"
  )

  private def _normalize_create_parameters(p: SClassBase): SClassBase =
    p match {
      case m: SCaseClass if _is_simple_entity_create_parent(m.core.parentClass) =>
        val params = m.core.parameterSequence.parameters
        val idparam = params.find(_.name.name == "id").getOrElse(_id_parameter())
        val ownparams = params.filterNot(x => _inherited_simple_entity_keys.contains(x.name.name))
        val compositeparams = Vector(
          _simple_object_parameter("name_Attributes", "NameAttributes"),
          _simple_object_parameter("descriptive_Attributes", "DescriptiveAttributes"),
          _simple_object_parameter("lifecycle_Attributes", "LifecycleAttributes"),
          _simple_object_parameter("publication_Attributes", "PublicationAttributes"),
          _simple_object_parameter("security_Attributes", "SecurityAttributes"),
          _simple_object_parameter("resource_Attributes", "ResourceAttributes"),
          _simple_object_parameter("audit_Attributes", "AuditAttributes"),
          _simple_object_parameter("media_Attributes", "MediaAttributes"),
          _simple_object_parameter("contextual_Attribute", "ContextualAttributes")
        )
        val normalized = ParameterSequence(idparam +: (compositeparams ++ ownparams))
        m.copy(core = m.core.copy(parameterSequence = normalized))
      case _ =>
        p
    }

  private def _is_simple_entity_create_parent(p: Option[TypeName]): Boolean =
    p.exists {
      case TypeName.Plain(pkg, "SimpleEntityCreate", _) if pkg.name == "org.goldenport.model" => true
      case _ => false
    }

  private def _simple_object_parameter(name: String, typename: String): Parameter =
    Parameter(
      ParameterName(name),
      TypeName.Plain(PackageName("org.goldenport.model.value"), typename),
      isAttribute = true,
      isDefault = false
    )

  private def _id_parameter(): Parameter =
    Parameter(
      ParameterName("id"),
      TypeName.option(TypeName.Plain(PackageName("org.goldenport.model.datatype"), "EntityId")),
      isAttribute = true,
      isDefault = false
    )

  private val _inherited_simple_entity_keys: Set[String] = Set(
    "id",
    "name",
    "title"
  )
}
