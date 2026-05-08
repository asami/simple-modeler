package org.simplemodeling.SimpleModeler.transformer.scala

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 23, 2025
 *  version Sep. 30, 2025
 *  version Mar. 25, 2026
 *  version Apr. 19, 2026
 * @version May.  8, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class CaseClassScalaModelTransformer() extends ScalaModelTransformer() {
  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.filterNot(_.isDerived).map(to_parameter))

  override protected def to_parameter(p: MAttribute): Parameter = {
    val typename = to_typename(p)
    val dbcolumnname = p.column.flatMap(x => Option(x.sql.name).map(_.trim).filterNot(_.isEmpty))
    val dbcolumntype = p.column.flatMap(_.sql.datatype.map(_.fullName))
    val externalname = p.column.flatMap(_.aliases.headOption).map(_.trim).filterNot(_.isEmpty)
    val constraints = to_constraints(p.constraints)
    Parameter(
      ParameterName(p.name),
      typename,
      isAttribute = true,
      isDefault = false,
      constraints = constraints,
      label = p.web.label.orElse(p.designation.labelI18N.map(_.c)),
      dbColumnName = dbcolumnname,
      dbColumnType = dbcolumntype,
      externalName = externalname,
      derived = p.derived,
      web = to_web_attribute(p),
      confidentiality = p.confidentiality
    )
  }

  override protected def to_attributes(ps: List[MAttribute]) =
    AttributeSequence(ps.toVector.filter(_.isDerived).map(to_attribute))
}
