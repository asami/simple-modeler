package org.simplemodeling.SimpleModeler.transformers.scala

import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Jul. 25, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
private[scala] object SimpleEntityScalaModelSupport {
  def normalizeOutput(
    p: SClassBase,
    valuePackageName: String => String = _ => "org.simplemodeling.model.value"
  ): SClassBase =
    p match {
      case m: SCaseClass if _is_simple_entity_parent(m.core.parentClass) =>
        val parameters = m.core.parameterSequence.parameters
        val idparameter = parameters.find(_.name.name == "id").getOrElse(_id_parameter)
        val remaining = parameters.filterNot(x => _standard_parameter_names.contains(x.name.name))
        val (inheritedparameters, ownparameters) =
          remaining.partition(x => _inherited_simple_entity_keys.contains(x.name.name))
        val schemaattributes =
          ParameterSequence(inheritedparameters).distillAttributes.attributes
        val compositeparameters = Vector(
          _simple_object_parameter("nameAttributes", "NameAttributes", valuePackageName),
          _simple_object_parameter("descriptiveAttributes", "DescriptiveAttributes", valuePackageName),
          _simple_object_parameter("contentAttributes", "ContentAttributes", valuePackageName),
          _simple_object_parameter("lifecycleAttributes", "LifecycleAttributes", valuePackageName),
          _simple_object_parameter("publicationAttributes", "PublicationAttributes", valuePackageName),
          _simple_object_parameter("securityAttributes", "SecurityAttributes", valuePackageName),
          _simple_object_parameter("resourceAttributes", "ResourceAttributes", valuePackageName),
          _simple_object_parameter("auditAttributes", "AuditAttributes", valuePackageName),
          _simple_object_parameter("mediaAttributes", "MediaAttributes", valuePackageName),
          _simple_object_parameter("contextualAttribute", "ContextualAttributes", valuePackageName)
        )
        val normalized =
          ParameterSequence(idparameter +: (_revision_parameter +: (compositeparameters ++ ownparameters)))
        val directive = m.core.directive.withSchemaAttributes(schemaattributes)
        m.copy(core = m.core.copy(parameterSequence = normalized, directive = directive))
      case _ =>
        p
    }

  def normalizeInput(
    p: SClassBase,
    parentClassName: String
  ): SClassBase =
    p match {
      case m: SCaseClass if _is_model_parent(m.core.parentClass, parentClassName) =>
        val parameters = m.core.parameterSequence.parameters.
          filterNot(x => _managed_input_parameter_names.contains(x.name.name))
        m.copy(core = m.core.copy(parameterSequence = ParameterSequence(parameters)))
      case _ =>
        p
    }

  private def _is_simple_entity_parent(p: Option[TypeName]): Boolean =
    _is_model_parent(p, "SimpleEntity")

  private def _is_model_parent(
    p: Option[TypeName],
    classname: String
  ): Boolean =
    p.exists {
      case TypeName.Plain(pkg, name, _)
          if pkg.name == "org.simplemodeling.model" && name == classname => true
      case _ => false
    }

  private def _simple_object_parameter(
    name: String,
    typename: String,
    valuepackagename: String => String
  ): Parameter =
    Parameter(
      ParameterName(name),
      TypeName.Plain(PackageName(valuepackagename(typename)), typename),
      isAttribute = true,
      isDefault = false
    )

  private val _id_parameter: Parameter =
    Parameter(
      ParameterName("id"),
      TypeName.Plain(PackageName("org.simplemodeling.model.datatype"), "EntityId"),
      isAttribute = true,
      isDefault = false
    )

  private val _revision_parameter: Parameter =
    Parameter(
      ParameterName("revision"),
      TypeName.Plain(PackageName("org.simplemodeling.model.datatype"), "EntityRevision"),
      isAttribute = true,
      isDefault = false,
      web = WebAttribute(
        system = true,
        readonly = true,
        help = Some("Framework-managed persistence revision.")
      )
    )

  private val _standard_parameter_names: Set[String] =
    Set("id", "revision")

  private val _managed_input_parameter_names: Set[String] =
    Set("revision")

  private val _inherited_simple_entity_keys: Set[String] = Set(
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
}
