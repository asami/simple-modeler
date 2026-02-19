package org.simplemodeling.model

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation
import org.goldenport.record.v3.{Table => _, _}
import org.goldenport.record.v2.Column
import org.smartdox.Description
import org.simplemodeling.parser.SimpleModelParser

/*
 * @since   Feb. 10, 2026
 * @version Feb. 19, 2026
 * @author  ASAMI, Tomoharu
 */
case class MParameter(
  description: Description,
  parameterType: MParameter.MParameterType,
  multiplicity: MMultiplicity = MOne,
  constraints: List[MConstraint] = Nil
) extends MElement {
  def isRequired: Boolean = multiplicity.isRequired
}

object MParameter {
  sealed trait MParameterType
  case class MDataTypeParameterType(datatype: MDataType) extends MParameterType
  case class MObjectParameterType(o: MObject) extends MParameterType
  case class MObjectRefParameterType(ref: MObjectRef) extends MParameterType

  val entityId = apply("id", MObjectRef.entityId)

  def apply(name: String, o: MDataType): MParameter = MParameter(
    Description.name(name),
    MDataTypeParameterType(o)
  )

  def apply(name: String, o: MObject): MParameter = MParameter(
    Description.name(name),
    MObjectParameterType(o)
  )

  def apply(name: String, o: MObjectRef): MParameter = MParameter(
    Description.name(name),
    MObjectRefParameterType(o)
  )

//  def apply(name: String, o: MObjectRef): MParameter = ???
  def option(name: String, o: MDataType): MParameter = ???
  def option(name: String, o: MObject): MParameter = ???
//  def option(name: String, o: MObjectRef): MParameter = ???
  def list(name: String, o: MDataType): MParameter = ???
  def list(name: String, o: MObject): MParameter = ???
//  def list(name: String, o: MObjectRef): MParameter = ???

  def vector(name: String, o: MDataType): MParameter = ???

  def vector(name: String, o: MObject): MParameter = apply(name, MTypedObject.vector(o))
//  def vector(name: String, o: MObjectRef): MParameter = ???

  def etityId: MParameter = apply("id", MObjectRef.entityId)

  def record: MParameter = record("record")

  def record(name: String): MParameter = apply(name, MObjectRef.record)

  def query(name: String, o: MObject): MParameter =
    apply(name, MTypedObject.query(o))

  def query(name: String, ref: MObjectRef): MParameter =
    apply(name, MTypedObject.query(ref))

  // def apply(config: SimpleModelParser.Config, p: Record): MParameter = {
  //   val kind = p.getStringCaseInsensitive(config.attributeKindNames) // XXX currently unused
  //   val name = p.getStringCaseInsensitive(config.nameNames) getOrElse {
  //     RAISE.syntaxErrorFault("Missing 'name' in attribute.")
  //   }
  //   val parametertype = {
  //     val d = p.getStringCaseInsensitive(config.datatypeNames).
  //       map(MAttributeType.create).getOrElse(MDataType.string)
  //     Left(d)
  //   }
  //   val multiplicity = p.getStringCaseInsensitive(config.multiplicityNames).
  //     map(MMultiplicity.create).getOrElse(MOne)
  //   val label = p.getStringCaseInsensitive(config.labelNames).
  //     map(I18NString.parse)
  //   val constraints = p.getStringCaseInsensitive(config.constraintNames).
  //     map(MConstraint.create).
  //     toList
  //   val designation = Designation.nameLabel(name, label)
  //   MParameter(designation, parametertype, multiplicity, constraints)
  // }
}
