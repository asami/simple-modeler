package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose

/*
 * @since   Sep. 23, 2025
 * @version Sep. 25, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class EntityCaseClassScalaModelTransformer() extends CaseClassScalaModelTransformer {
  protected def is_Accept_Object(p: MObject): Boolean = p.isInstanceOf[MEntity]

  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MEntity, purpose) if is_accept_purpose(purpose) => transform_entity(m)
      case _ => Consequence.noReachDefect(s"EntityValueCreateScalaModelTransformer#apply")
    }

  protected def transform_entity(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
    Vector(_to_scala(p))
  }

  private def _to_scala(p: MEntity): SCaseClass = {
    val core = sub_Package_Name match {
      case Some(s) => to_scala_core_subpackage(p, s)
      case None => to_scala_core(p)
    }
    SCaseClass(core)
  }
}
