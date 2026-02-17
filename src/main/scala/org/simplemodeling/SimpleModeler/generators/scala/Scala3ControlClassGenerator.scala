package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 16, 2026
 * @version Feb. 16, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3ControlClassGenerator(
  context: ScalaModel.Context
) extends Scala3ClassGeneratorBase[SControlClass](context) {
  val classkind = ClassKind.Control
}

object Scala3ControlClassGenerator {
}
