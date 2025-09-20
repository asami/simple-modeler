package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz.{modify => _, _}
import org.goldenport.context.Consequence
import org.goldenport.scalaz.rwscr._
import org.goldenport.scalaz.Recorder
import org.simplemodeling.SimpleModeler.generator._

/*
 * @since   Sep. 18, 2025
 * @version Sep. 19, 2025
 * @author  ASAMI, Tomoharu
 */
trait SourceArtifactsGenerator[T] {
  import SourceArtifactsGenerator._

  def generate(p: T): Consequence[SourceArtifacts] = {
    val r = run(p)
    val config = Config()
    val init = State()
    r.run(config, init).map { x =>
      val (output, s, state) = x
      s
    }
  }

  def run(p: T): ArtifactsPipeline
}

object SourceArtifactsGenerator {
  type Pipeline[A] = RWSCR[Config, State, A]
  type ArtifactsPipeline = Pipeline[SourceArtifacts]

  implicit def pipelineMonoid[A](implicit M: Monoid[A]): Monoid[Pipeline[A]] =
  new Monoid[Pipeline[A]] {
    def zero: Pipeline[A] =
      ReaderWriterStateT { (r: Config, s: State) =>
        Consequence.success((Recorder.empty, M.zero, s))
      }

    def append(f1: Pipeline[A], f2: => Pipeline[A]): Pipeline[A] =
      for {
        a <- f1
        b <- f2
      } yield a |+| b
  }  

  trait Config {
  }
  object Config {
    case class Instance() extends Config

    def apply(): Config = Instance()
  }

  trait State {
  }
  object State {
    case class Instance() extends State

    def apply(): State = Instance()
  }
}
