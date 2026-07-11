package org.simplemodeling.SimpleModeler.generator.componentapi

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Locale
import org.simplemodeling.model._

/*
 * @since   Jul. 12, 2026
 * @version Jul. 12, 2026
 * @author  ASAMI, Tomoharu
 */
object ComponentApiContractMetadata {
  final case class Operation(
    name: String,
    requestType: String,
    responseType: String
  )

  final case class PublicType(
    className: String,
    kind: String,
    artifactPatterns: Vector[String],
    attributes: Vector[PublicAttribute],
    baseTypes: Vector[String],
    traitTypes: Vector[String],
    powertypeValues: Vector[String]
  )

  final case class PublicAttribute(
    name: String,
    typeName: String,
    multiplicity: String
  )

  final case class ProvidedApi(
    componentName: String,
    contract: String,
    service: String,
    apiClass: String,
    operations: Vector[Operation],
    publicTypes: Vector[PublicType],
    packages: Vector[String],
    abiHash: String
  )

  final case class RequiredApi(
    componentName: String,
    service: String,
    apiClass: String,
    multiplicity: String,
    required: Boolean
  )

  final case class Document(
    provided: Vector[ProvidedApi],
    required: Vector[RequiredApi]
  ) {
    def isEmpty: Boolean = provided.isEmpty && required.isEmpty

    def toCanonicalJson: String =
      _object(Vector(
        "schemaVersion" -> _string("cncf.component-api-model.v1"),
        "provided" -> _array(provided.map(_provided_json)),
        "required" -> _array(required.map(_required_json))
      ))
  }

  def generate(model: SimpleModel): Either[String, Document] = {
    val elements = _all_elements(model.root)
    val components = elements.collect { case m: MComponent => m }.sortBy(_.qualifiedName)
    val objects = elements.collect { case m: MObject => m }
    val provided = components.flatMap(_provided_apis(_, objects))
    val required = components.flatMap(_required_apis)
    val errors = provided.collect { case Left(message) => message }
    errors.headOption match {
      case Some(message) => Left(message)
      case None => Right(Document(
        provided.collect { case Right(api) => api }.sortBy(x => (x.apiClass, x.contract)),
        required.sortBy(x => (x.apiClass, x.componentName, x.service))
      ))
    }
  }

  private def _provided_apis(
    component: MComponent,
    objects: Vector[MObject]
  ): Vector[Either[String, ProvidedApi]] =
    _component_core(component).componentDefinitions.flatMap(_.services).filter { definition =>
      definition.spiDirection.equalsIgnoreCase("provides") && definition.spiSocket
    }.map { definition =>
      val apiclass = s"${component.packageName}.api.${_api_name(definition)}"
      val operations = _service_operations(component, definition).sortBy(_.name)
      _public_type_closure(component, apiclass, operations, objects).map { publictypes =>
        val packages = publictypes.map(x => _package_name(x.className)).filter(_.nonEmpty).distinct.sorted
        val canonical = _provided_contract_json(
          component.name,
          s"${component.name}.${definition.name}",
          definition.name,
          apiclass,
          operations,
          publictypes,
          packages
        )
        ProvidedApi(
          component.name,
          s"${component.name}.${definition.name}",
          definition.name,
          apiclass,
          operations,
          publictypes,
          packages,
          s"sha256:${_sha256(canonical)}"
        )
      }
    }

  private def _required_apis(component: MComponent): Vector[RequiredApi] =
    _component_core(component).componentDefinitions.flatMap(_.services).filter { definition =>
      definition.spiDirection.equalsIgnoreCase("requires") && definition.spiComponentApi.exists(_.trim.nonEmpty)
    }.map { definition =>
      val multiplicity = definition.spiMultiplicity.map(_.trim).filter(_.nonEmpty).getOrElse("1")
      RequiredApi(
        component.name,
        definition.name,
        definition.spiComponentApi.get.trim,
        multiplicity,
        definition.spiRequired || multiplicity == "1"
      )
    }

  private def _service_operations(
    component: MComponent,
    definition: MComponent.ComponentServiceDefinition
  ): Vector[Operation] = {
    val operationnames = component.services.find(x => _normalize(x.name) == _normalize(definition.name))
      .map(_.operations.map(_.name).toSet).getOrElse(Set.empty)
    _component_core(component).operationDefinitions.filter(x => operationnames.exists(_normalize(_) == _normalize(x.name))).map { operation =>
      Operation(
        operation.name,
        _generated_value_type(component, operation.inputType),
        _generated_value_type(component, operation.outputType)
      )
    }
  }

  private def _public_type_closure(
    component: MComponent,
    apiclass: String,
    operations: Vector[Operation],
    objects: Vector[MObject]
  ): Either[String, Vector[PublicType]] = {
    val roots = operations.flatMap(x => Vector(x.requestType, x.responseType)).distinct
    val initial = _api_public_types(apiclass)
    roots.foldLeft[Either[String, Vector[PublicType]]](Right(initial)) { (result, typename) =>
      result.flatMap { accumulated =>
        _resolve_public_type(component, typename, objects, Set.empty).map(types =>
          (accumulated ++ types).groupBy(_.className).values.map(_.head).toVector.sortBy(_.className)
        )
      }
    }
  }

  private def _resolve_public_type(
    component: MComponent,
    typename: String,
    objects: Vector[MObject],
    visited: Set[String]
  ): Either[String, Vector[PublicType]] = {
    val normalized = _strip_container(typename)
    if (_is_forbidden_type(normalized))
      Left(s"Component API public signature exposes a forbidden implementation type: $normalized")
    else if (_is_external_type(normalized))
      Right(Vector.empty)
    else {
      _resolve_object(component, normalized, objects) match {
        case Left(message) => Left(message)
        case Right(None) => Right(Vector.empty)
        case Right(Some(entity: MEntity)) =>
          Left(s"Component API public signature exposes a persistence entity type: ${entity.qualifiedName}")
        case Right(Some(datatype: MStructuredDataType)) =>
          _resolve_object_closure(component, datatype, "datatype", objects, visited)
        case Right(Some(value: MValue)) =>
          _resolve_object_closure(component, value, "value", objects, visited)
        case Right(Some(powertype: MPowertype)) =>
          _resolve_object_closure(component, powertype, "powertype", objects, visited)
        case Right(Some(other)) =>
          Left(s"Component API public signature exposes an unsupported model type: ${other.qualifiedName}")
      }
    }
  }

  private def _api_public_types(apiclass: String): Vector[PublicType] =
    Vector(
      _api_public_type(apiclass, "api"),
      _api_public_type(apiclass + "$", "contract"),
      _api_public_type(apiclass + "$Proxy", "proxy"),
      _api_public_type(apiclass + "$Proxy$", "proxy-companion"),
      _api_public_type(apiclass + "$Provider$", "provider"),
      _api_public_type(apiclass + "$Socket", "socket"),
      _api_public_type(apiclass + "$SocketSet", "socket-set")
    )

  private def _api_public_type(classname: String, kind: String): PublicType =
    PublicType(classname, kind, _artifact_patterns(classname), Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  private def _resolve_object_closure(
    component: MComponent,
    obj: MObject,
    kind: String,
    objects: Vector[MObject],
    visited: Set[String]
  ): Either[String, Vector[PublicType]] = {
    val classname = _generated_object_type(component, obj)
    if (visited.contains(classname))
      Right(Vector.empty)
    else {
      val nextvisited = visited + classname
      val references = obj.attributes.flatMap(_public_attribute_reference(_, objects)) ++
        obj.base.map(_reference_name).toList ++
        obj.traits.map(_reference_name) ++
        obj.powertypes.map(_reference_name)
      val publictype = PublicType(
        classname,
        kind,
        _artifact_patterns(classname),
        obj.attributes.toVector.map(_public_attribute(component, _, objects)).sortBy(_.name),
        obj.base.map(_reference_name).toVector.sorted,
        obj.traits.map(_reference_name).toVector.sorted,
        obj match {
          case powertype: MPowertype => powertype.kinds.toVector.map(x => s"${x.name}:${x.value.getOrElse("")}:${x.label}")
          case _ => Vector.empty
        }
      )
      references.foldLeft[Either[String, Vector[PublicType]]](Right(Vector(publictype))) { (result, reference) =>
        result.flatMap { accumulated =>
          _resolve_public_type(component, reference, objects, nextvisited).map(accumulated ++ _)
        }
      }
    }
  }

  private def _resolve_object(
    component: MComponent,
    typename: String,
    objects: Vector[MObject]
  ): Either[String, Option[MObject]] = {
    val leaf = typename.split('.').lastOption.getOrElse(typename)
    val candidates = objects.filter { obj =>
      obj.name == leaf || obj.qualifiedName == typename || _generated_object_type(component, obj) == typename
    }
    val local = candidates.filter(_.packageName == component.packageName)
    val selected = if (local.nonEmpty) local else candidates
    selected.distinct match {
      case Vector() =>
        if (_is_primitive_name(typename)) Right(None)
        else Left(s"Component API public type is not resolvable from the CML model: $typename")
      case Vector(value) => Right(Some(value))
      case values => Left(s"Component API public type is ambiguous: $typename (${values.map(_.qualifiedName).sorted.mkString(", ")})")
    }
  }

  private def _public_attribute_reference(
    attribute: MAttribute,
    objects: Vector[MObject]
  ): Option[String] =
    attribute.attributeType match {
      case datatype: MDataType =>
        val name = datatype.name
        if (objects.exists(x => x.name == name || x.qualifiedName == name)) Some(name) else None
      case other => Some(other.name)
    }

  private def _public_attribute(
    component: MComponent,
    attribute: MAttribute,
    objects: Vector[MObject]
  ): PublicAttribute = {
    val typename = attribute.attributeType match {
      case datatype: MDataType =>
        _public_attribute_reference(attribute, objects).flatMap { reference =>
          _resolve_object(component, reference, objects) match {
            case Right(Some(obj)) => Some(_generated_object_type(component, obj))
            case _ => None
          }
        }.getOrElse(datatype.datatype.name)
      case other => other.name
    }
    PublicAttribute(attribute.name, typename, attribute.multiplicity.label)
  }

  private def _artifact_patterns(classname: String): Vector[String] = {
    val classpath = classname.replace('.', '/')
    val tastypath = classname.takeWhile(_ != '$').replace('.', '/') + ".tasty"
    Vector(s"$classpath.class", s"$classpath$$*.class", tastypath).distinct
  }

  private def _generated_value_type(component: MComponent, typename: String): String = {
    val value = Option(typename).getOrElse("").trim
    if (_is_record_type(value)) "org.goldenport.record.Record"
    else if (value.contains(".")) value
    else s"${component.packageName}.value.$value"
  }

  private def _component_core(component: MComponent): MComponent.Core =
    component match {
      case holder: MComponent.Core.Holder => holder.componentCore
      case _ => throw new IllegalArgumentException(s"MComponent does not expose component core metadata: ${component.qualifiedName}")
    }

  private def _generated_object_type(component: MComponent, obj: MObject): String =
    if (obj.packageName == component.packageName)
      obj match {
        case _: MStructuredDataType => s"${component.packageName}.datatype.${obj.name}"
        case _: MValue => s"${component.packageName}.value.${obj.name}"
        case _ => obj.qualifiedName
      }
    else obj.qualifiedName

  private def _reference_name(reference: MRelationship): String =
    if (reference.targetPackageName.isEmpty)
      reference.targetName
    else
      s"${reference.targetPackageName}.${reference.targetName}"

  private def _all_elements(pkg: MPackage): Vector[MElement] =
    pkg.elements.flatMap {
      case child: MPackage => Vector(child) ++ _all_elements(child)
      case element => Vector(element)
    }

  private def _api_name(definition: MComponent.ComponentServiceDefinition): String = {
    val stem = definition.spiApiName.map(_.trim).filter(_.nonEmpty).getOrElse(_title(definition.name))
    if (stem.endsWith("Api")) stem else s"${stem}Api"
  }

  private def _title(name: String): String =
    Option(name).getOrElse("").split("[^A-Za-z0-9]+").toVector.filter(_.nonEmpty).map { token =>
      token.head.toUpper + token.tail
    }.mkString

  private def _normalize(name: String): String =
    Option(name).getOrElse("").toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]", "")

  private def _strip_container(name: String): String =
    Option(name).getOrElse("").trim
      .replaceAll("^(Option|Vector|List|Seq|Set)\\[", "")
      .replaceAll("\\]$", "")

  private def _is_record_type(name: String): Boolean =
    name == "Record" || name == "org.goldenport.record.Record"

  private def _is_external_type(name: String): Boolean =
    _is_record_type(name) ||
      name.startsWith("java.") ||
      name.startsWith("scala.") ||
      name.startsWith("org.goldenport.") ||
      name.startsWith("org.simplemodeling.model.")

  private def _is_forbidden_type(name: String): Boolean = {
    val normalized = s".${name.toLowerCase(Locale.ROOT)}."
    Vector(".impl.", ".entity.", ".persistence.").exists(normalized.contains)
  }

  private def _is_primitive_name(name: String): Boolean =
    Set("string", "boolean", "byte", "short", "int", "integer", "long", "float", "double", "decimal", "date", "datetime", "time", "uuid", "uri", "url", "record", "unit").contains(name.toLowerCase(Locale.ROOT))

  private def _package_name(classname: String): String =
    classname.lastIndexOf('.') match {
      case -1 => ""
      case index => classname.substring(0, index)
    }

  private def _sha256(value: String): String =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8)).map(x => f"${x & 0xff}%02x").mkString

  private def _provided_contract_json(
    componentname: String,
    contract: String,
    service: String,
    apiclass: String,
    operations: Vector[Operation],
    publictypes: Vector[PublicType],
    packages: Vector[String]
  ): String =
    _object(Vector(
      "componentName" -> _string(componentname),
      "contract" -> _string(contract),
      "service" -> _string(service),
      "apiClass" -> _string(apiclass),
      "operations" -> _array(operations.map(_operation_json)),
      "publicTypes" -> _array(publictypes.map(_public_type_json)),
      "packages" -> _array(packages.map(_string))
    ))

  private def _provided_json(api: ProvidedApi): String =
    _object(Vector(
      "componentName" -> _string(api.componentName),
      "contract" -> _string(api.contract),
      "service" -> _string(api.service),
      "apiClass" -> _string(api.apiClass),
      "operations" -> _array(api.operations.map(_operation_json)),
      "publicTypes" -> _array(api.publicTypes.map(_public_type_json)),
      "packages" -> _array(api.packages.map(_string)),
      "abiHash" -> _string(api.abiHash)
    ))

  private def _required_json(api: RequiredApi): String =
    _object(Vector(
      "componentName" -> _string(api.componentName),
      "service" -> _string(api.service),
      "apiClass" -> _string(api.apiClass),
      "multiplicity" -> _string(api.multiplicity),
      "required" -> api.required.toString
    ))

  private def _operation_json(operation: Operation): String =
    _object(Vector(
      "name" -> _string(operation.name),
      "requestType" -> _string(operation.requestType),
      "responseType" -> _string(operation.responseType)
    ))

  private def _public_type_json(publictype: PublicType): String =
    _object(Vector(
      "className" -> _string(publictype.className),
      "kind" -> _string(publictype.kind),
      "artifactPatterns" -> _array(publictype.artifactPatterns.map(_string)),
      "attributes" -> _array(publictype.attributes.map(_public_attribute_json)),
      "baseTypes" -> _array(publictype.baseTypes.map(_string)),
      "traitTypes" -> _array(publictype.traitTypes.map(_string)),
      "powertypeValues" -> _array(publictype.powertypeValues.map(_string))
    ))

  private def _public_attribute_json(attribute: PublicAttribute): String =
    _object(Vector(
      "name" -> _string(attribute.name),
      "typeName" -> _string(attribute.typeName),
      "multiplicity" -> _string(attribute.multiplicity)
    ))

  private def _object(fields: Vector[(String, String)]): String =
    fields.map { case (key, value) => s"${_string(key)}:$value" }.mkString("{", ",", "}")

  private def _array(values: Vector[String]): String = values.mkString("[", ",", "]")

  private def _string(value: String): String = {
    val builder = new StringBuilder
    builder.append('"')
    Option(value).getOrElse("").foreach {
      case '"' => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\b' => builder.append("\\b")
      case '\f' => builder.append("\\f")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case c if c < ' ' => builder.append(f"\\u${c.toInt}%04x")
      case c => builder.append(c)
    }
    builder.append('"')
    builder.result()
  }
}
