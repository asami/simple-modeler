package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 25, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MPOperation(model: MOperation) extends MPElement with POperation {
  def description: Description = model.description
}
