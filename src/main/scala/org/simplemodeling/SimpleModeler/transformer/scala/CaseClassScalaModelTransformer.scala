package org.simplemodeling.SimpleModeler.transformer.scala

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 23, 2025
 * @version Sep. 30, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class CaseClassScalaModelTransformer() extends ScalaModelTransformer() {
  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.map(to_parameter))

  override protected def to_parameter(p: MAttribute): Parameter = {
    val typename = to_typename(p)
    Parameter(ParameterName(p.name), typename, true, false)
  }

  override protected def to_attributes(ps: List[MAttribute]) = AttributeSequence.empty
}
