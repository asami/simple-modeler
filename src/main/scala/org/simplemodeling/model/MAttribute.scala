package org.simplemodeling.model

import org.goldenport.record.v2.Column

/*
 * derived from SAttribute and SMAttribute.
 *
 * @since   Sep. 10, 2008
 *  version Nov. 13, 2010
 *  version Dec. 15, 2011
 *  version Feb.  9, 2012
 *  version Mar. 25, 2012
 *  version Oct. 30, 2012
 *  version Dec.  9, 2012
 *  version Dec.  9, 2012
 * @version Aug.  8, 2019
 * @author  ASAMI, Tomoharu
 */
case class MAttribute(
  attributeType: MAttributeType,
  multiplicity: MMultiplicity,
  constraints: List[MConstraint],
  //
  column: Option[Column]
) {
}
