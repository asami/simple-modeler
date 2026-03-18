package org.simplemodeling.SimpleModeler.transformer.scala

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 23, 2025
 *  version Sep. 30, 2025
 * @version Mar. 19, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class CaseClassScalaModelTransformer() extends ScalaModelTransformer() {
  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.map(to_parameter))

  override protected def to_parameter(p: MAttribute): Parameter = {
    val typename = to_typename(p)
    val dbcolumnname = p.column.flatMap(x => Option(x.sql.name).map(_.trim).filterNot(_.isEmpty))
    val dbcolumntype = p.column.flatMap(_.sql.datatype.map(_.fullName))
    val externalname = p.column.flatMap(_.aliases.headOption).map(_.trim).filterNot(_.isEmpty)
    Parameter(
      ParameterName(p.name),
      typename,
      isAttribute = true,
      isDefault = false,
      dbColumnName = dbcolumnname,
      dbColumnType = dbcolumntype,
      externalName = externalname
    )
  }

  override protected def to_attributes(ps: List[MAttribute]) = AttributeSequence.empty
}
