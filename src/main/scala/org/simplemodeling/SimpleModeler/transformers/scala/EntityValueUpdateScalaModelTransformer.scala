package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.goldenport.record.v2._
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 20, 2025
 *  version Sep. 23, 2025
 *  version Feb. 18, 2026
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueUpdateScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Update)
  override protected def sub_Package_Name: Option[String] = Some("update")

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    super.to_scala_core_parent(p).map {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.goldenport.model" =>
        TypeName.Plain(PackageName("org.goldenport.model"), "SimpleEntityUpdate")
      case m => m
    }

  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence = {
    val xs = ps.filterNot(_is_id_attribute).toVector.map(to_parameter)
    ParameterSequence(xs)
  }

  override protected def to_parameter(p: MAttribute): Parameter = {
    val t = _update_type(to_typename(p))
    Parameter(ParameterName(p.name), t, true, false)
  }

  private def _is_id_attribute(p: MAttribute): Boolean =
    p.name == "id" || (p.attributeType match {
      case m: MDataType => m.datatype == XEntityId
      case _ => false
    })

  private def _update_type(p: TypeName): TypeName = {
    val update = TypeName.Plain(PackageName("org.goldenport.cncf.directive"), "Update")
    TypeName.Container(update, _unwrap_option(p))
  }

  private def _unwrap_option(p: TypeName): TypeName =
    p match {
      case m: TypeName.Container if m.isOption => m.containee
      case m => m
    }

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Update)

  // def isDefinedAt(p: (MObject, Purpose)): Boolean =
  //   p match {
  //     case (_: MEntity, Purpose.Update) => true
  //     case _ => false
  //   }

  // def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
  //   p match {
  //     case (m: MEntity, Purpose.Update) => _transform(m)
  //     case _ => Consequence.noReachDefect(s"EntityValueUpdateScalaModelTransformer#apply")
  //   }

  // private def _transform(p: MEntity): Consequence[Vector[SClassBase]] = Consequence {
  //   Vector(_to_scala(p))
  // }

  // private def _to_scala(p: MEntity): SCaseClass = {
  //   val core = to_scala_core_subpackage(p, "update")
  //   SCaseClass(core)
  // }
}
