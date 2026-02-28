package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.scalaz.rwscr
import org.simplemodeling.model.MObject
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator._
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM
import org.simplemodeling.SimpleModeler.generators.scala._
import model._
import Generator.{State => GState, _}

/*
 * @since   Sep. 18, 2025
 *  version Sep. 21, 2025
 *  version Nov.  8, 2025
 * @version Feb. 26, 2026
 * @author  ASAMI, Tomoharu
 */
trait Scala3ClassFamilyGeneratorBase[T <: MObject] extends SourceArtifactsGenerator[T] {
  import SourceArtifactsGenerator.ArtifactsPipeline

  protected val scala_context = ScalaModel.Context.default

  def run(p: T): ArtifactsPipeline = {
    scala_model_transformers.foldMap(_generate_class(_, p))
  }

  protected def scala_model_transformers: Vector[ScalaModelTransformer]

  private def _generate_class(g: ScalaModelTransformer, p: T): ArtifactsPipeline = {
    val a00 = Purpose.elements.map(purpose =>
      if (g.isDefinedAt((p, purpose)))
        g.apply((p, purpose))
      else
        Consequence.success(Vector.empty))
    val a0 = Purpose.elements.traverse(purpose =>
      if (g.isDefinedAt((p, purpose)))
        g.apply((p, purpose))
      else
        Consequence.success(Vector.empty))
    val a: Consequence[Vector[SClassBase]] = a0.map(_.flatten)

    val r = for {
      xs <- a
      rs <- _generate_classes(xs)
    } yield rs
    rwscr.lift(r)
  }

  private def _generate_classes(ps: Vector[SClassBase]): Consequence[SourceArtifacts] =
    ps.traverse(_generate_class).map(_.suml)

  private def _generate_class(p: SClassBase): Consequence[SourceArtifacts] =
    p match {
      case m: SComponent => new Scala3ComponentGenerator(scala_context).generate(m)
      case m: STrait => ??? // Consequence.success(new Scala3TraitGenerator())
      case m: SCaseClass => new Scala3CaseClassGenerator(scala_context).generate(m)
      case m: SEnum => ??? // Consequence.success(new Scala3EnumGenerator())
      case m: SControlClass => new Scala3ControlClassGenerator(scala_context).generate(m)
      case m: SEntityClass => new Scala3EntityGenerator(scala_context).generate(m)
    }

  // private def _generate_classx(g: Scala3ClassGeneratorBase[SCaseClass], p: SClassBase): Consequence[SourceArtifacts] =
  //   for {
  //     s <- g.generate(p)
  //   } yield SourceArtifacts.create(???, s)

  // protected def class_generators: Vector[Scala3ClassGeneratorBase[T]]

  // def run(p: T): ArtifactsPipeline =
  //   class_generators.foldMap(_generate_class(_, p))

  // private def _generate_class(g: Scala3ClassGeneratorBase[T], p: T): ArtifactsPipeline = {
  //   val r = for {
  //     a <- g.generate(p)
  //   } yield SourceArtifacts()
  //   rwscr.lift(r)
  // }
}
