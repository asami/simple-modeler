package org.simplemodeling.SimpleModeler.generators.scala

import org.simplemodeling.model.MPowertype
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.PowertypeScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassFamilyGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel

/*
 * @since   Mar. 24, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3PowertypeFamilyGenerator(
  context: ScalaModel.Context = ScalaModel.Context.default
) extends Scala3ClassFamilyGeneratorBase[MPowertype] {
  override protected val scala_context = context

  protected def scala_model_transformers: Vector[ScalaModelTransformer] =
    Vector(
      new PowertypeScalaModelTransformer()
    )
}

object Scala3PowertypeFamilyGenerator {
}
