package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 18, 2026
 * @version Feb. 19, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueQueryScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Query)
  override protected def sub_Package_Name: Option[String] = Some("query")

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Query)
}
