package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model.MObject
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.model.SClassBase

/*
 * @since   Sep. 19, 2025
 * @version Sep. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class EntityValueCreateScalaModelTransformer() extends ScalaModelTransformer() {
  def isDefinedAt(p: (MObject, Purpose)): Boolean = ???

  def apply(p: (MObject, Purpose)): Consequence[SClassBase] = ???
}
