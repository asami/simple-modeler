package org.simplemodeling.model

import org.goldenport.RAISE

/*
 * @since   Aug. 13, 2020
 * @version Oct.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait GraphPart { self: SimpleModel =>
  def ownUses(p: MObject): Seq[MUse] = Nil // TODO
  def derivedUses(p: MObject): Seq[MUse] = Nil // TODO
  def ownParticipations(p: MObject): Seq[MParticipation] = Nil // TODO
  def derivedParticipations(p: MObject): Seq[MParticipation] = Nil // TODO
}
