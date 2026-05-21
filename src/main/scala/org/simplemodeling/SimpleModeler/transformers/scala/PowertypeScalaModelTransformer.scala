package org.simplemodeling.SimpleModeler.transformers.scala

import scala.util.Try
import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformer.scala.CaseClassScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Mar. 24, 2026
 *  version Mar. 25, 2026
 *  version Apr.  3, 2026
 * @version May. 22, 2026
 * @author  ASAMI, Tomoharu
 */
class PowertypeScalaModelTransformer() extends CaseClassScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_accept_object(p: MObject): Boolean = p.isInstanceOf[MPowertype]

  override protected def to_scala_core_parent(p: MObject): Option[TypeName] =
    Some(TypeName(PackageName("org.simplemodeling.model.powertype"), "Powertype"))

  override protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(Vector(Parameter(
      ParameterName("value"),
      TypeName.Primitive.string,
      isAttribute = true
    )))

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
    val enumvalues = p.kinds.toVector.map { k =>
      val dbvalue = k.value.flatMap(x => Try(x.trim.toInt).toOption)
      Directive.EnumerationValue(k.name, k.name, dbvalue, k.label)
    }
    val base = to_scala_core_with_subpackage(p)
    val core = base.copy(
      directive = base.directive.withPurpose(purpose).withEnumerationValues(enumvalues)
    )
    Vector(SCaseClass(core))
  }
}
