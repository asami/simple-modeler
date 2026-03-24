package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.CaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 24, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class PowertypeScalaModelTransformer() extends CaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_Accept_Object(p: MObject): Boolean = p.isInstanceOf[MPowertype]

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MPowertype, purpose) if is_accept_purpose(purpose) =>
        transform_powertype(m, purpose)
      case _ =>
        Consequence.noReachDefect("PowertypeScalaModelTransformer#apply")
    }

  protected def transform_powertype(
    p: MPowertype,
    purpose: Purpose
  ): Consequence[Vector[SClassBase]] = Consequence {
    val core = to_scala_core_with_subpackage(p).withPurpose(purpose)
    Vector(SCaseClass(core))
  }
}
