package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.record.v3.{Table => _, _}

/*
 * Derived from SPowertypeRelationship and SMPowertypeRelationship.
 * 
 * @since   Dec. 21, 2008
 *  version Jan. 20, 2009
 *  version Nov. 24, 2012
 *  version Jan. 13, 2013
 *  since   Nov.  3, 2019
 *  version May.  9, 2020
 *  version Sep. 22, 2020
 *  version Oct.  3, 2020
 * @version Oct. 12, 2023
 * @author  ASAMI, Tomoharu
 */
case class MPowertypeRef(
  packageRef: MPackageRef,
  powertypeName: String,
  multiplicity: MMultiplicity = MOne
) extends MReference {
  def relationshipType = MRelationshipType(powertypeName, packageRef)
  // def targetName: String = powertypeName
  // def targetPackageName: String = packageRef.packageName
  def powertype(implicit sm: SimpleModel): MPowertype = sm.getPowertype(this)
    .getOrElse(RAISE.syntaxErrorFault(s"No powertype: $powertypeName"))
}

object MPowertypeRef {
  def apply(packageRef: MPackageRef, p: Record): MPowertypeRef = ???
}
