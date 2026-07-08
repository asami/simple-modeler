package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainValue
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.CaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 25, 2026
 *  version May. 22, 2026
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
class ValueScalaModelTransformer() extends CaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_accept_object(p: MObject): Boolean =
    p.isInstanceOf[MDomainValue] || p.isInstanceOf[MStructuredDataType]

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MDomainValue, purpose) if is_accept_purpose(purpose) =>
        transform_value(m, purpose)
      case (m: MStructuredDataType, purpose) if is_accept_purpose(purpose) =>
        transform_value(m, purpose)
      case _ =>
        Consequence.noReachDefect("ValueScalaModelTransformer#apply")
    }

  protected def transform_value(
    p: MValue,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] = Consequence {
    val core = to_scala_core(p).withPurpose(purpose)
    Vector(SCaseClass(core))
  }
}
