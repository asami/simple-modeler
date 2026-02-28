package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.goldenport.record.v2._
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 19, 2025
 *  version Sep. 23, 2025
 * @version Feb. 27, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueCreateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Create)
  override protected def sub_Package_Name: Option[String] = Some("create")

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Create)

  override protected def to_typename(p: MDataType): TypeName =
    p.datatype match {
      case XEntityId => TypeName.option(super.to_typename(p))
      case m => super.to_typename(p)
    }
//    TypeName.Primitive.createMarshalling(p)
}
