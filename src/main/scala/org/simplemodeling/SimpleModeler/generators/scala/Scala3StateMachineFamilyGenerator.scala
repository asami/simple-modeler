package org.simplemodeling.SimpleModeler.generators.scala

import org.simplemodeling.model.MStateMachine
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformers.scala.StateMachineScalaModelTransformer
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassFamilyGeneratorBase

/*
 * @since   Mar. 24, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3StateMachineFamilyGenerator(
) extends Scala3ClassFamilyGeneratorBase[MStateMachine] {
  protected def scala_model_transformers: Vector[ScalaModelTransformer] =
    Vector(
      new StateMachineScalaModelTransformer()
    )
}

object Scala3StateMachineFamilyGenerator {
}
