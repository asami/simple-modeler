package org.simplemodeling.model

import org.smartdox.Description

/*
 * Derived from SStep and SMStep.
 * 
 * @since   Nov.  8, 2008
 *  version Dec.  8, 2008
 *  version Nov. 12, 2010
 *  version Nov.  4, 2011
 *  version Nov. 18, 2012
 *  version May.  6, 2020
 *  version Jul. 25, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
trait MStep extends MElement {
  def description: Description = Description.empty
  def sequenceNumber: Int = ???
}
