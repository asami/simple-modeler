package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.EntityCaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 18, 2026
 *  version Feb. 19, 2026
 *  version Mar. 24, 2026
 * @version May.  8, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueQueryScalaModelTransformer() extends EntityCaseClassScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Query)
  override protected def sub_Package_Name: Option[String] = Some("query")

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    super.to_scala_core_parent(p).map {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.simplemodeling.model" =>
        TypeName.Plain(PackageName("org.simplemodeling.model"), "SimpleEntityQuery")
      case m => m
    }

  override protected def to_parameter(p: MAttribute): Parameter = {
    val t = _condition_type(to_typename(p))
    Parameter(ParameterName(p.name), t, true, false, web = to_web_attribute(p), confidentiality = p.confidentiality)
  }

  private def _condition_type(p: TypeName): TypeName = {
    val cond = TypeName.Plain(PackageName("org.simplemodeling.model.directive"), "Condition")
    TypeName.Container(cond, _unwrap_option(p))
  }

  private def _unwrap_option(p: TypeName): TypeName =
    p match {
      case m: TypeName.Container if m.isOption => m.containee
      case m => m
    }

  def apply(p: MObject): Consequence[Vector[SClassBase]] =
    apply(p, Purpose.Query)
}
