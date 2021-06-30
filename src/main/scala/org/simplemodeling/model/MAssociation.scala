package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.value._
import org.goldenport.values.Designation
import org.goldenport.record.v3.{Table => _, _}
import org.smartdox.Description
import org.simplemodeling.parser.SimpleModelParser

/*
 * derived from SAssociation and SMAssociation.
 *
 * @since   Sep. 11, 2008
 *  version Nov. 11, 2009
 *  version Nov. 25, 2012
 *  version Feb.  7, 2013
 *  version Aug.  7, 2019
 *  version Nov.  4, 2019
 *  version Dec. 15, 2019
 *  version Mar.  2, 2020
 *  version Apr. 25, 2020
 *  version May. 24, 2020
 *  version Jun. 17, 2020
 *  version Sep. 26, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MAssociation(
  override val designation: Designation,
  description: Description,
  getAffiliation: Option[MPackageRef],
  objectRef: MObjectRef,
  kind: MAssociation.Kind,
  multiplicity: MMultiplicity,
  collaborations: List[CombiCollaboration]
) extends MRelationship {
//  def targetName: String = objectRef.objectName
//  def targetPackageName: String = objectRef.packageName
  def associationType: MAssociationType = MAssociationType(designation, objectRef)
  def relationshipType: MRelationshipType = associationType

  def isComposition: Boolean = false // TODO
  def isAggregation: Boolean = false // TODO
}

object MAssociation {
  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Association, Aggregation, Composition)
  }

  case object Association extends Kind {
    val name = "association"
  }
  case object Aggregation extends Kind { // implicit whole/part
    val name = "aggregation"
  }
  case object Composition extends Kind { // implicit whole/part
    val name = "composition"
  }

  case object SuperAssociationKind extends Kind { // XXX See MAssociationKind
    val name = "super"
  }

  def apply(config: SimpleModelParser.Config, p: Record): MAssociation = {
    val kind = Association // TODO
    val name = p.getStringCaseInsensitive(config.nameNames) getOrElse {
      RAISE.syntaxErrorFault("Missing 'name' in association.")
    }
    val label = p.getStringCaseInsensitive(config.labelNames).
      map(I18NString.parse)
    val description = Description.empty
    val affiliation = None
    val obj = p.getStringCaseInsensitive(config.typeNames).
      map(MObjectRef.create).getOrElse {
        RAISE.syntaxErrorFault("Missing 'type' in association.")
      }
    val multiplicity = p.getStringCaseInsensitive(config.multiplicityNames).
      map(MMultiplicity.create).getOrElse(MOne)
    val collaborations = Nil // TODO
    val designation = Designation.nameLabel(name, label)
    MAssociation(designation, description, affiliation, obj, kind, multiplicity, collaborations)
  }
}
