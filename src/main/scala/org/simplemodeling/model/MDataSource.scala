package org.simplemodeling.model

import org.simplemodeling.model._

/*
 * Derived from DataSource and SMDataSource.
 * 
 * @since   Jan. 17, 2011
 *  since   Mar. 26, 2011
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
trait MDataSource extends MObject {
  def entities: List[MEntityRef]
}
