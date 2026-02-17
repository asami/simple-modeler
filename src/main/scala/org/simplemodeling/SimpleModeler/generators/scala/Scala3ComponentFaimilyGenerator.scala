package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model.SimpleModel
import org.simplemodeling.model.MComponent
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassFamilyGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.ComponentScalaModelTransformer

/*
 * @since   Feb.  8, 2026
 * @version Feb. 12, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3ComponentFamilyGenerator(
) extends Scala3ClassFamilyGeneratorBase[MComponent]{
  protected def scala_model_transformers: Vector[ScalaModelTransformer] =
    Vector(
      new ComponentScalaModelTransformer()
    )
}

object Scala3ComponentFamilyGenerator {
}
