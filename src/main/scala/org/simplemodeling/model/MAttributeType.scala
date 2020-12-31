package org.simplemodeling.model

/*
 * derived from SAttributeType and SMAttributeType.
 *
 * @since   Sep. 10, 2008
 *  version Oct. 22, 2008
 *  version Jun. 24, 2009
 *  version Aug.  7, 2019
 *  version Nov.  4, 2019
 *  version Jan.  5, 2020
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
trait MAttributeType extends MElement {
  // def name = dslAttributeType.name
  // def packageName = dslAttributeType.packageName
  // def qualifiedName = dslAttributeType.qualifiedName
  // def summary = dslAttributeType.summary
  // var typeObject: SMObject = SMNullObject
  // var constraints = new ArrayBuffer[SMConstraint]
  def constraints: Seq[MConstraint] = ???
}

object MAttributeType {
  def create(p: String): MAttributeType = MDatatype.create(p)
}
