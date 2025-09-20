package org.simplemodeling.SimpleModeler.generator

import scalaz._, Scalaz._

/*
 * @since   Sep. 18, 2025
 * @version Sep. 19, 2025
 * @author  ASAMI, Tomoharu
 */
case class SourceArtifacts(
  slots: Vector[SourceArtifacts.Slot] = Vector.empty
) {
  def +(rhs: SourceArtifacts): SourceArtifacts =
    SourceArtifacts(slots ++ rhs.slots)
}

object SourceArtifacts {
  val empty = SourceArtifacts()

  implicit object SourceArtifactsMonoid extends Monoid[SourceArtifacts] {
    def zero = empty
    def append(lhs: SourceArtifacts, rhs: => SourceArtifacts) = lhs + rhs
  }

  case class Slot(path: String, content: String)

  def create(path: String, content: String): SourceArtifacts =
    SourceArtifacts(Vector(Slot(path, content)))
}
