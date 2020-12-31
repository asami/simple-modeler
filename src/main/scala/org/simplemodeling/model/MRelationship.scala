package org.simplemodeling.model

import org.smartdox._

/*
 * derived from SRelationship and SMRelationship.
 *
 * @since   Sep. 10, 2008
 *  version Nov. 21, 2008
 *  version Aug.  7, 2019
 *  version Apr. 25, 2020
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 *  version Aug. 15, 2020
 *  version Sep. 26, 2020
 * @version Oct.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait MRelationship extends MElement {
  def relationshipType: MRelationshipType
  def targetName: String = relationshipType.target.name
  def targetPackageName: String = relationshipType.target.packageName

  final def targetTypeLiteral: Dox = {
    UnresolvedLink(targetName, relationshipType.target)
  }

  override def features = List(MFeature(MFeature.Type, this, targetTypeLiteral))
}
