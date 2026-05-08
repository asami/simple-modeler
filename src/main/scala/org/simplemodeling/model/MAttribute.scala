package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.goldenport.record.v3.{Table => _, _}
import org.goldenport.record.v2.{Column, SqlColumn, NullSqlColumn}
import org.goldenport.record.sql
import org.smartdox.Description
import org.simplemodeling.parser.SimpleModelParser

/*
 * derived from SAttribute and SMAttribute.
 *
 * @since   Sep. 10, 2008
 *  version Oct. 20, 2009
 *  version Nov. 13, 2010
 *  version Dec. 15, 2011
 *  version Feb.  9, 2012
 *  version Mar. 25, 2012
 *  version Oct. 30, 2012
 *  version Nov. 23, 2012
 *  version Dec.  9, 2012
 *  version Aug.  8, 2019
 *  version Nov.  4, 2019
 *  version Apr. 25, 2020
 *  version May. 16, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 *  version Sep. 23, 2025
 *  version Feb. 10, 2026
 *  version Mar. 19, 2026
 *  version Apr. 19, 2026
 * @version May.  8, 2026
 * @author  ASAMI, Tomoharu
 */
case class MAttribute(
  override val designation: Designation,
  attributeType: MAttributeType,
  multiplicity: MMultiplicity,
//  label: Option[I18NString],
  constraints: List[MConstraint],
  //
  column: Option[Column],
  //
  readonly: Boolean = false,
  derived: Option[String] = None,
  web: MAttribute.Web = MAttribute.Web.empty,
  confidentiality: Option[String] = None,
  description: Description = Description.empty
) extends MElement {
  def isRequired: Boolean = multiplicity.isRequired
  def isDerived: Boolean = derived.nonEmpty
}

object MAttribute {
  def apply(config: SimpleModelParser.Config, p: Record): MAttribute = {
    val kind = p.getStringCaseInsensitive(config.attributeKindNames) // XXX currently unused
    val name = p.getStringCaseInsensitive(config.nameNames) getOrElse {
      RAISE.syntaxErrorFault("Missing 'name' in attribute.")
    }
    val datatype = p.getStringCaseInsensitive(config.datatypeNames).
      map(MAttributeType.create).getOrElse(MDataType.string)
    val multiplicity = p.getStringCaseInsensitive(config.multiplicityNames).
      map(MMultiplicity.create).getOrElse(MOne)
    val label = p.getStringCaseInsensitive(config.labelNames).
      map(I18NString.parse)
    val web = Web.parse(p)
    val confidentiality = _string_value_flexible(p, Seq("confidentiality", "confidentiality-level", "confidentialityLevel", "security-level", "securityLevel"))
    val constraints = p.getStringCaseInsensitive(config.constraintNames).
      map(MConstraint.create).
      toList ++ web.validationConstraints
    val derived = _string_value_flexible(p, config.derivedNames.list).orElse(
      p.getStringCaseInsensitive(config.derivedNames).map(_.trim).filterNot(_.isEmpty)
    ).orElse(
      if (_has_web_field(p))
        None
      else
        p.getString("5").map(_.trim).filterNot(_.isEmpty)
    )
    val dbcolumnname = p.getStringCaseInsensitive(config.dbColumnNameNames).map(_.trim).filterNot(_.isEmpty)
    val dbcolumntype = p.getStringCaseInsensitive(config.dbColumnTypeNames).map(_.trim).filterNot(_.isEmpty)
    val externalname = p.getStringCaseInsensitive(config.externalNameNames).map(_.trim).filterNot(_.isEmpty)
    val column = Some(
      Column(
        name,
        datatype = datatype match {
          case m: MDataType => m.datatype
          case _ => org.goldenport.record.v2.XString
        },
        multiplicity = multiplicity.multiplicity,
        label = label.map(_.c),
        aliases = externalname.toList,
        sql = _sql_column(dbcolumnname, dbcolumntype)
      )
    )
    val designation = Designation.nameLabel(name, label)
    MAttribute(designation, datatype, multiplicity, constraints, column, derived = derived, web = web, confidentiality = confidentiality)
  }

  case class Web(
    label: Option[String] = None,
    controlType: Option[String] = None,
    placeholder: Option[String] = None,
    help: Option[String] = None,
    required: Option[Boolean] = None,
    hidden: Option[Boolean] = None,
    readonly: Option[Boolean] = None,
    minLength: Option[String] = None,
    maxLength: Option[String] = None,
    min: Option[String] = None,
    max: Option[String] = None,
    step: Option[String] = None,
    pattern: Option[String] = None
  ) {
    def validationConstraints: List[MConstraint] =
      List(
        minLength.map(LiteralConstraint("min_length", _)),
        maxLength.map(LiteralConstraint("max_length", _)),
        min.map(LiteralConstraint("min", _)),
        max.map(LiteralConstraint("max", _)),
        step.map(LiteralConstraint("step", _)),
        pattern.map(LiteralConstraint("pattern", _))
      ).flatten
  }
  object Web {
    val empty: Web = Web()

    def parse(p: Record): Web =
      Web(
        label = _string_value_flexible(p, Seq("web-label", "webLabel")).orElse(_string_value_flexible(p, Seq("label"))),
        controlType = _string_value_flexible(p, Seq("web-control-type", "web-controlType", "webControlType", "web-control", "webControl", "web-widget", "webWidget")),
        placeholder = _string_value_flexible(p, Seq("web-placeholder", "webPlaceholder")),
        help = _string_value_flexible(p, Seq("web-help", "webHelp")),
        required = _boolean_value_flexible(p, Seq("web-required", "webRequired")),
        hidden = _boolean_value_flexible(p, Seq("web-hidden", "webHidden")),
        readonly = _boolean_value_flexible(p, Seq("web-readonly", "webReadonly", "web-read-only", "webReadOnly")),
        minLength = _string_value_flexible(p, Seq("web-min-length", "webMinLength")),
        maxLength = _string_value_flexible(p, Seq("web-max-length", "webMaxLength")),
        min = _string_value_flexible(p, Seq("web-min", "webMin")),
        max = _string_value_flexible(p, Seq("web-max", "webMax")),
        step = _string_value_flexible(p, Seq("web-step", "webStep")),
        pattern = _string_value_flexible(p, Seq("web-pattern", "webPattern", "web-regex", "webRegex"))
      )
  }

  private def _string_value_flexible(p: Record, keys: Seq[String]): Option[String] = {
    val normalized = keys.map(_normalize_key).toSet
    p.fields.collectFirst {
      case field if normalized.contains(_normalize_key(field.name)) =>
        field.value.asString.trim
    }.filterNot(_.isEmpty)
  }

  private def _boolean_value_flexible(p: Record, keys: Seq[String]): Option[Boolean] =
    _string_value_flexible(p, keys).map(_.trim.toLowerCase(java.util.Locale.ROOT)).collect {
      case "true" | "yes" | "on" | "1" => true
      case "false" | "no" | "off" | "0" => false
    }

  private def _normalize_key(p: String): String =
    p.toLowerCase.replaceAll("[\\s_\\-　]+", "")

  private def _has_web_field(p: Record): Boolean =
    p.fields.exists(field => _normalize_key(field.name).startsWith("web"))

  private def _sql_column(
    dbcolumnname: Option[String],
    dbcolumntype: Option[String]
  ): SqlColumn = {
    val n = dbcolumnname.map(_.trim).filterNot(_.isEmpty)
    val t = dbcolumntype.flatMap(_to_sql_datatype)
    if (n.isEmpty && t.isEmpty)
      NullSqlColumn
    else
      SqlColumn(
        name = n.orNull,
        datatype = t
      )
  }

  private def _to_sql_datatype(p: String): Option[sql.SqlDatatype] = {
    val trimmed = p.trim
    if (trimmed.isEmpty)
      None
    else {
      val upper = trimmed.toUpperCase
      val onearg = """^([A-Z_]+)\((\d+)\)$""".r
      upper match {
        case onearg("VARCHAR", length) => Some(sql.VARCHAR(length.toInt))
        case onearg("NVARCHAR", length) => Some(sql.NVARCHAR(length.toInt))
        case onearg("CHAR", length) => Some(sql.CHAR(length.toInt))
        case "INT" => Some(sql.INT())
        case "INTEGER" => Some(sql.INTEGER())
        case "BIGINT" => Some(sql.BIGINT())
        case "REAL" => Some(sql.REAL())
        case "FLOAT" => Some(sql.FLOAT())
        case "DOUBLE" => Some(sql.DOUBLE())
        case "BOOLEAN" => Some(sql.BOOLEAN())
        case "TEXT" => Some(sql.CLOB())
        case "DATE" => Some(sql.DATE())
        case "TIME" => Some(sql.TIME())
        case "TIMESTAMP" => Some(sql.TIMESTAMP())
        case "NUMERIC" => Some(sql.NUMERIC())
        case "DECIMAL" => Some(sql.DECIMAL())
        case "BLOB" => Some(sql.BLOB())
        case "CLOB" => Some(sql.CLOB())
        case _ => None
      }
    }
  }
}
