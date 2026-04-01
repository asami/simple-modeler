package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model.MEntity
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala._
import org.simplemodeling.SimpleModeler.generator.scala.Generator
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassFamilyGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 18, 2025
 *  version Sep. 21, 2025
 *  version Feb. 19, 2026
 * @version Apr.  2, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3EntityFamilyGenerator(
) extends Scala3ClassFamilyGeneratorBase[MEntity] {
  import Scala3EntityFamilyGenerator._

  protected def scala_model_transformers: Vector[ScalaModelTransformer] =
    Vector(
      new EntityValueScalaModelTransformer(),
      new EntityValueCreateScalaModelTransformer(),
      new EntityValueReadScalaModelTransformer(),
      new EntityValueUpdateScalaModelTransformer(),
      new EntityValueAggregateScalaModelTransformer(),
      new EntityValueViewScalaModelTransformer(),
      new EntityValueSummaryScalaModelTransformer(),
      new EntityValueDetailScalaModelTransformer(),
      new EntityValueOperationScalaModelTransformer(),
      new EntityValueQueryScalaModelTransformer()
    )

  // protected def class_generators: Vector[Scala3ClassGeneratorBase[SEntityClass]] =
  //   Vector(
  //     new Scala3EntityGenerator()
  //   )
}

object Scala3EntityFamilyGenerator {
}
