package org.simplemodeling.model

import org.goldenport.extension.Showable
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.smartdox.Description

/*
 * derived from ModelElement since Mar. 18, 2007
 * derived from SElement and SMElement.
 *
 * @since   Sep. 15, 2008
 *  version Jul. 19, 2009
 *  version Dec. 18, 2010
 *  version Feb. 22, 2012
 *  version Nov. 26, 2012
 *  version Dec. 26, 2012
 *  version Jan. 17, 2013
 *  version Feb.  3, 2013
 *  version Aug.  8, 2019
 *  version Dec. 15, 2019
 *  version Apr. 25, 2020
 *  version May. 18, 2020
 *  version Jun. 13, 2020
 * @version Aug. 15, 2020
 * @author  ASAMI, Tomoharu
 */
trait MElement extends Designation.Holder with Description.Holder with Showable {
  def designation: Designation
  def description: Description
  final def resume: org.smartdox.Resume = description.resume
  def getAffiliation: Option[MPackageRef]
  def qualifiedName: String = getAffiliation.map(x =>
    if (x.isDefault)
      name
    else
      s"${x.packageName}.${name}"
  ).getOrElse(name)

  def features: List[MFeature] = Nil

  def print = name
  def display = name
  def show = name
  def embed = name
}
