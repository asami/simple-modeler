package org.simplemodeling.SimpleModeler.transformer.maker.mobject

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Apr. 25, 2020
 *  version Apr. 26, 2020
 *  version May.  3, 2020
 * @version Dec. 20, 2020
 * @author  ASAMI, Tomoharu
 */
trait MPObject extends PObject with MPElement {
  def model: MObject
  def affiliation = model.affiliation
  def stereotypes = model.stereotypes
}
