package org.simplemodeling.SimpleModeler.transformer.maker

/*
 * @since   Aug. 19, 2011
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class ScalaAspect extends GenericAspect with ScalaMakerHolder {
  def open(m: ScalaMaker) {
    sm_open(m)
  }
}
