package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 12, 2026
 * @version Feb. 12, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3ComponentGenerator(
  context: ScalaModel.Context
) extends Scala3ClassGeneratorBase[SComponent](context) {
  val classkind = ClassKind.Component
}

object Scala3ComponentGenerator {
}
