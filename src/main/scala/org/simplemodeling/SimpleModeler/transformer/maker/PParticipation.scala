package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
import org.simplemodeling.model._

/*
 * @since   Nov. 25, 2012
 *  version Dec. 22, 2012
 * @version Mar.  2, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait PParticipation {
  val source: PObject
}

case class BaseParticipation(source: PObject) extends PParticipation
case class TraitParticipation(source: PObject) extends PParticipation
/**
 * SimpleModel2ProgramRealmTransformerBase sets AttributeParticipation.
 */
case class AttributeParticipation(source: PObject, attribute: PAttribute) extends PParticipation
