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
 *  version Feb. 27, 2026
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueCreateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Create)
  override protected def sub_Package_Name: Option[String] = Some("create")

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    super.to_scala_core_parent(p).map {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.goldenport.model" =>
        TypeName.Plain(PackageName("org.goldenport.model"), "SimpleEntityCreate")
      case m => m
    }

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Create)

  override protected def to_typename(p: MDataType): TypeName =
    p.datatype match {
      case XEntityId => TypeName.option(super.to_typename(p))
      case m => super.to_typename(p)
    }
//    TypeName.Primitive.createMarshalling(p)

  override protected def to_parameter(p: MAttribute): Parameter = {
    val base = super.to_parameter(p)
    if (_is_autocomplement_target(p))
      base.typeName match {
        case m: TypeName.Container if m.isOption => base
        case _ => base.copy(typeName = TypeName.option(base.typeName))
      }
    else
      base
  }

  private def _is_autocomplement_target(p: MAttribute): Boolean = {
    val key = p.name.toLowerCase(java.util.Locale.ROOT)
    p.isRequired && _autocomplement_target_keys.contains(key)
  }

  private val _autocomplement_target_keys: Set[String] = Set(
    "id",
    "name",
    "createdat",
    "updatedat",
    "createdby",
    "updatedby",
    "poststatus",
    "aliveness",
    "traceid",
    "correlationid"
  )
}
