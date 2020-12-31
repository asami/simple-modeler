package org.simplemodeling.SimpleModeler.generator

import org.goldenport.recorder.Recordable
import org.simplemodeling.SimpleModeler._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Jun. 21, 2020
 * @version Jun. 21, 2020
 * @author  ASAMI, Tomoharu
 */
trait GeneratorBase extends Recordable {
  def context: Context

  set_Recorder(context)
}

