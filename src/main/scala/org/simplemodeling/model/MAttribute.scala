package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.goldenport.record.v3.{Table => _, _}
import org.goldenport.record.v2.Column
import org.smartdox.Description
import org.simplemodeling.parser.SimpleModelParser

/*
 * derived from SAttribute and SMAttribute.
 *
 * @since   Sep. 10, 2008
 *  version Oct. 20, 2009
 *  version Nov. 13, 2010
 *  version Dec. 15, 2011
 *  version Feb.  9, 2012
 *  version Mar. 25, 2012
 *  version Oct. 30, 2012
 *  version Nov. 23, 2012
 *  version Dec.  9, 2012
 *  version Aug.  8, 2019
 *  version Nov.  4, 2019
 *  version Apr. 25, 2020
 *  version May. 16, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MAttribute(
  override val designation: Designation,
  attributeType: MAttributeType,
  multiplicity: MMultiplicity,
//  label: Option[I18NString],
  constraints: List[MConstraint],
  //
  column: Option[Column],
  //
  readonly: Boolean = false,
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = None
}

object MAttribute {
  def apply(config: SimpleModelParser.Config, p: Record): MAttribute = {
    val kind = p.getStringCaseInsensitive(config.attributeKindNames) // XXX currently unused
    val name = p.getStringCaseInsensitive(config.nameNames) getOrElse {
      RAISE.syntaxErrorFault("Missing 'name' in attribute.")
    }
    val datatype = p.getStringCaseInsensitive(config.datatypeNames).
      map(MAttributeType.create).getOrElse(MDatatype.string)
    val multiplicity = p.getStringCaseInsensitive(config.multiplicityNames).
      map(MMultiplicity.create).getOrElse(MOne)
    val label = p.getStringCaseInsensitive(config.labelNames).
      map(I18NString.parse)
    val constraints = p.getStringCaseInsensitive(config.constraintNames).
      map(MConstraint.create).
      toList
    val column = None // XXX
    val designation = Designation.nameLabel(name, label)
    MAttribute(designation, datatype, multiplicity, constraints, column)
  }
}
