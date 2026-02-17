package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 19, 2025
 *  version Nov.  8, 2025
 * @version Feb. 12, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3CaseClassGenerator(
  context: ScalaModel.Context
) extends Scala3ClassGeneratorBase[SCaseClass](context) {
  val classkind = ClassKind.Value
}

object Scala3CaseClassGenerator {
}
