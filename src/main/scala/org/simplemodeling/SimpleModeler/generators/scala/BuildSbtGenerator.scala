package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.scala.Generator
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel

/*
 * @since   May. 19, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class BuildSbtGenerator(
) extends Generator[ScalaModel, String] {
  def run(p: ScalaModel): Generator.GenM[String] = ???
}

object Scala3SbtGenerator {
}
