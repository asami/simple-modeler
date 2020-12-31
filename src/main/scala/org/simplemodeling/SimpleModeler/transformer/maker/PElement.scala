package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * @since   Apr. 25, 2020
 *  version May.  3, 2020
 *  version Jun. 13, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
trait PElement extends Designation.Holder {
  def designation: Designation
  def description: Description
  def getAffiliation: Option[MPackageRef] = None
  def qualifiedName: String = getAffiliation.map(x =>
    if (x.isDefault)
      name
    else
      s"${x.packageName}.${name}"
  ).getOrElse(name)
}
