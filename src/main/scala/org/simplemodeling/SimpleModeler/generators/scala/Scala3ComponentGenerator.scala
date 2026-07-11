package org.simplemodeling.SimpleModeler.generators.scala

import org.goldenport.context.Consequence
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generator.SourceArtifacts

/*
 * @since   Feb. 12, 2026
 * @version Jul. 11, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3ComponentGenerator(
  context: ScalaModel.Context
) extends Scala3ClassGeneratorBase[SComponent](context) {
  val classkind = ClassKind.Component

  override def generate(component: SComponent): Consequence[SourceArtifacts] =
    super.generate(component).map(_ + ComponentApiSourceGenerator.generate(component))
}

object Scala3ComponentGenerator {
}
