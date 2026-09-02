package org.simplemodeling.SimpleModeler.generators.scala

import org.simplemodeling.model.MValue
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.ValueScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassFamilyGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel

/*
 * @since   Mar. 25, 2026
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3ValueFamilyGenerator(
  context: ScalaModel.Context = ScalaModel.Context.default
) extends Scala3ClassFamilyGeneratorBase[MValue] {
  override protected val scala_context = context

  protected def scala_model_transformers: Vector[ScalaModelTransformer] =
    Vector(
      new ValueScalaModelTransformer()
    )
}

object Scala3ValueFamilyGenerator {
}
