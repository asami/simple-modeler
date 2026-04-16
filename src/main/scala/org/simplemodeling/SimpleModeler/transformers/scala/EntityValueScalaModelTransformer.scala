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
 * @version Apr.  2, 2026
 *  version Sep. 23, 2025
 * @author  ASAMI, Tomoharu
 */
class EntityValueScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Plain)
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
    p match {
      case m: SCaseClass if _is_simple_entity_parent(m.core.parentClass) =>
        val params = m.core.parameterSequence.parameters
        val idparam = params.find(_.name.name == "id").getOrElse(_id_parameter())
        val ownparams = params.filterNot(x => _inherited_simple_entity_keys.contains(x.name.name))
        val compositeparams = Vector(
          _simple_object_parameter("nameAttributes", "NameAttributes"),
          _simple_object_parameter("descriptiveAttributes", "DescriptiveAttributes"),
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
    "content"
  )
}
