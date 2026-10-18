package org.simplemodeling.SimpleModeler.generator.scala

import org.goldenport.RAISE
import org.goldenport.record.v2._
import org.simplemodeling.model.{MBuiltinStructuredDataType, MRange, MRanges}
import model._

/*
 * Explicit declaration projection into the accepted CNCF persistence ABI.
 *
 * @since   Oct.  5, 2026
 * @version Oct. 18, 2026
 * @author  ASAMI, Tomoharu
 */
object EntityStoreMappingProjection {
  final case class FieldPlan(
    logicalName: String,
    storageName: String,
    datatypeExpression: String,
    multiplicityExpression: String,
    bindingExpression: String
  )

  def project(
    attributes: Vector[Attribute],
    isCreate: Boolean,
    isUpdate: Boolean,
    schemaDatatype: TypeName => String
  ): Vector[FieldPlan] = {
    attributes.map { attribute =>
      attribute.storeDeclaration.foreach { declaration =>
        declaration.multiplicity match {
          case _: MRange | _: MRanges => {
            _unsupported(attribute, declaration.typeName, "bounded/disjoint multiplicity")
          }
          case _ => { () }
        }
      }
      val declaredtype = if (isCreate) {
        attribute.typeName
      } else if (isUpdate) {
        attribute.storeDeclaration.map(_.typeName).getOrElse(_peel_update(attribute.typeName))
      } else {
        attribute.storeDeclaration.map(_.typeName).getOrElse(attribute.typeName)
      }
      val (datatype, binding) = _binding(attribute, declaredtype, schemaDatatype)
      FieldPlan(attribute.name.name, attribute.dbColumnName.getOrElse(attribute.name.name),
        datatype, _multiplicity(attribute, declaredtype), binding)
    }
  }

  def stringLiteral(value: String): String = {
    "\"" + value.flatMap {
      case '"' => { "\\\"" }
      case '\\' => { "\\\\" }
      case '\n' => { "\\n" }
      case '\r' => { "\\r" }
      case '\t' => { "\\t" }
      case c if Character.isISOControl(c) => { "\\u%04x".format(c.toInt) }
      case c => { c.toString }
    } + "\""
  }

  private val _binding_prefix = "EntityStoreFieldBinding."
  private val _schema_prefix = "org.goldenport.schema."
  private val _ordered_containers = Set(
    "scala.Vector", "scala.collection.immutable.Vector",
    "scala.List", "scala.collection.immutable.List",
    "scala.Seq", "scala.collection.Seq", "scala.collection.immutable.Seq",
    "cats.data.NonEmptyVector"
  )
  private val _text_types = Set(
    "org.goldenport.datatype.Name", "org.goldenport.datatype.Identifier",
    "org.goldenport.datatype.Text", "org.goldenport.datatype.Token",
    "org.goldenport.datatype.Password", "org.goldenport.datatype.Url",
    "org.goldenport.datatype.Uri", "org.goldenport.datatype.Urn",
    "org.simplemodeling.model.datatype.EntityId", "org.simplemodeling.model.datatype.EntityCollectionId",
    "java.time.Instant", "java.time.ZonedDateTime", "java.time.OffsetDateTime",
    "java.time.LocalDateTime", "java.time.LocalDate", "java.time.LocalTime",
    "java.time.OffsetTime", "java.time.Year", "java.time.YearMonth",
    "java.time.MonthDay", "java.time.Duration", "java.time.Period",
    "java.net.URI", "java.net.URL", "java.nio.charset.Charset"
  )
  private val _numeric_types = Map(
    "Byte" -> "XInt", "Short" -> "XInt", "Int" -> "XInt", "Long" -> "XLong",
    "Float" -> "XFloat", "Double" -> "XDouble", "BigInt" -> "XInteger", "BigDecimal" -> "XDecimal"
  )

  private def _peel_update(typename: TypeName): TypeName = {
    typename match {
      case TypeName.Container(wrapper, child) if wrapper.fullName == "org.simplemodeling.model.directive.Update" => { child }
      case other => { other }
    }
  }

  private def _multiplicity(attribute: Attribute, typename: TypeName): String = {
    val name = typename match {
      case TypeName.Container(wrapper, _) if wrapper.fullName == "scala.Option" => { "ZeroOne" }
      case TypeName.Container(wrapper, _) if wrapper.fullName == "cats.data.NonEmptyVector" => { "OneMore" }
      case TypeName.Container(wrapper, _) if _ordered_containers.contains(wrapper.fullName) => { "ZeroMore" }
      case _: TypeName.Container => { _unsupported(attribute, typename, "unsupported container") }
      case _ => { "One" }
    }
    _schema_prefix + "Multiplicity." + name
  }

  private def _binding(
    attribute: Attribute,
    typename: TypeName,
    schemadatatype: TypeName => String
  ): (String, String) = {
    typename match {
      case TypeName.Container(wrapper, child) if wrapper.fullName == "scala.Option" => {
        child match {
          case _: TypeName.Container => { _unsupported(attribute, typename, "optional collection/container") }
          case _ => { _binding(attribute, child, schemadatatype) }
        }
      }
      case TypeName.Container(wrapper, child) if _ordered_containers.contains(wrapper.fullName) => {
        val (datatype, binding) = _binding(attribute, child, schemadatatype)
        (datatype, s"${_binding_prefix}array($binding)")
      }
      case _: TypeName.Container => { _unsupported(attribute, typename, "unsupported container/order") }
      case TypeName.Structured(MBuiltinStructuredDataType.Json) => { (_schema_prefix + "XJson", _binding_prefix + "json") }
      case TypeName.Structured(MBuiltinStructuredDataType.SemiStructuredData) => {
        (_schema_prefix + "XSemiStructuredData", _binding_prefix + "semiStructuredData")
      }
      case plain: TypeName.Plain if plain.fullName == "org.goldenport.record.Record" => {
        (_schema_prefix + "XSemiStructuredData", _binding_prefix + "semiStructuredData")
      }
      case primitive: TypeName.Primitive => {
        primitive.datatype match {
          case XString => { (_schema_prefix + "XString", _binding_prefix + "string") }
          case XBoolean => { _native_binding("Boolean", schemadatatype(primitive), "Boolean") }
          case datatype if datatype.name == "byte" => { _native_binding("Byte", schemadatatype(primitive), "Number") }
          case XShort => { _native_binding("Short", schemadatatype(primitive), "Number") }
          case XInt => { _native_binding("Int", schemadatatype(primitive), "Number") }
          case XLong => { _native_binding("Long", schemadatatype(primitive), "Number") }
          case XFloat => { _native_binding("Float", schemadatatype(primitive), "Number") }
          case XDouble => { _native_binding("Double", schemadatatype(primitive), "Number") }
          case XInteger => { _native_binding("BigInt", schemadatatype(primitive), "Number") }
          case XDecimal => { _native_binding("BigDecimal", schemadatatype(primitive), "Number") }
          case _ => { _unsupported(attribute, typename, "unsupported primitive") }
        }
      }
      case plain: TypeName.Plain if plain.packageName.name == "scala" && plain.name == "String" => {
        (_schema_prefix + "XString", _binding_prefix + "string")
      }
      case plain: TypeName.Plain if plain.packageName.name == "scala" && plain.name == "Boolean" => {
        _native_binding(plain.fullName, _schema_prefix + "XBoolean", "Boolean")
      }
      case plain: TypeName.Plain if plain.packageName.name == "scala" && _numeric_types.contains(plain.name) => {
        _native_binding(plain.fullName, _schema_prefix + _numeric_types(plain.name), "Number")
      }
      case plain: TypeName.Plain if plain.fullName == "org.simplemodeling.model.datatype.EntityRevision" => {
        _native_binding(plain.fullName, plain.fullName + ".DATA_TYPE", "Number")
      }
      case plain: TypeName.Plain if _text_types.contains(plain.fullName) => {
        _native_binding(plain.fullName, schemadatatype(plain), "Text")
      }
      case plain: TypeName.Plain => {
        val classname = plain.fullName
        (_schema_prefix + "DataType.Named(" + stringLiteral(classname) + ")",
          s"${_binding_prefix}application[$classname](${stringLiteral("declared:" + classname)})${_captures(classname)}")
      }
      case _ => { _unsupported(attribute, typename, "unsupported declaration") }
    }
  }

  private def _native_binding(classname: String, datatype: String, representation: String): (String, String) = {
    (datatype, s"${_binding_prefix}nativeScalar[$classname]($datatype, EntityStoreScalarRepresentation.$representation, " +
      stringLiteral("declared:" + classname) + ")" + _captures(classname))
  }

  private def _captures(classname: String): String = {
    s"(using summon[scala.reflect.ClassTag[$classname]], summon[io.circe.Encoder[$classname]], summon[io.circe.Decoder[$classname]])"
  }

  private def _unsupported(attribute: Attribute, typename: TypeName, reason: String): Nothing = {
    RAISE.syntaxErrorFault(s"Entity store declaration ${attribute.name.name} (${typename.fullName}): $reason")
  }
}
