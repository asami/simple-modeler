package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose

/*
 * @since   Sep. 19, 2025
 * @version Sep. 29, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class ScalaModelTransformer() extends PartialFunction[(MObject, ScalaModelTransformer.Purpose), Consequence[Vector[SClassBase]]] {
  protected def is_Accept_Object(p: MObject): Boolean
  protected def accept_Purposes: Vector[Purpose]
  protected def sub_Package_Name: Option[String] = None

  protected final def is_accept_purpose(p: Purpose): Boolean = 
    accept_Purposes.contains(p)

  def isDefinedAt(p: (MObject, Purpose)): Boolean =
    is_Accept_Object(p._1) && is_accept_purpose(p._2)

  protected final def to_scala_core_subpackage(
    p: MObject,
    subpkg: String
  ): ClassCore = to_scala_core(p).moveToSubPackage(subpkg)

  protected final def to_scala_core(p: MObject): ClassCore = {
    val packagename = PackageName(p.packageName)
    val declaration = ClassDeclaration.CaseClass
    val classname = ClassName(p.name)
    val parentclass = p.base.map(_to_type)
    val traits = p.traits.map(_to_type)
    val parameters = to_parameters(p.attributes)
    val fields = to_fields(p.attributes)
    val methods = _to_methods(p.operations)
    val receptions = ReceptionCompartment.empty // TODO
    ClassCore(
      packagename,
      declaration,
      classname,
      parentclass,
      traits,
      parameters,
      fields,
      methods,
      receptions
    )
  }

  private def _to_type(p: MObjectRef): TypeName =
    TypeName(PackageName(p.packageName), p.name)

  private def _to_type(p: MTraitRef): TypeName =
    TypeName(PackageName(p.packageRef.packageName), p.name)

  protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.flatMap(to_parameter_required))

  protected def to_parameter_required(p: MAttribute): Option[Parameter] =
    if (p.isRequired)
      Some(to_parameter(p))
    else
      None

  protected def to_parameter(p: MAttribute): Parameter = {
    val typename = to_typename(p)
    Parameter(ParameterName(p.name), typename, false, false)
  }

  protected def to_fields(ps: List[MAttribute]): FieldCompartment =
    FieldCompartment(ps.toVector.flatMap(x => Vector(to_field(x))))

  protected def to_field(p: MAttribute): Field = {
    val typename = to_typename(p)
    Field(FieldName(p.name), typename)
  }

  protected def to_attributes(ps: List[MAttribute]): AttributeSequence =
    AttributeSequence(ps.toVector.map(to_attribute))

  protected def to_attribute(p: MAttribute): Attribute = {
    val typename = to_typename(p)
    Attribute(AttributeName(p.name), typename)
  }

  def to_typename(p: MAttribute): TypeName = p.multiplicity match {
    case MOne => _typename_one(p.attributeType)
    case MZeroOne => _typename_zeroone(p.attributeType)
    case MOneMore => _typename_zeromore(p.attributeType)
    case MZeroMore => _typename_onemore(p.attributeType)
    case m: MRange => _typename_range(p.attributeType)
    case m: MRanges => _typename_ranges(p.attributeType)
  }

  private def _typename_one(p: MAttributeType): TypeName =
    _to_typename(p)

  private def _typename_zeroone(p: MAttributeType): TypeName =
    TypeName.Container(
      TypeName.Plain(PackageName("scala"), "Option"),
      _to_typename(p)
    )

  protected def container_type_zeromore: TypeName =
    TypeName.Plain(PackageName("scala.collection.immutable"), "Vector")

  protected def container_type_onemore: TypeName =
    TypeName.Plain(PackageName("cats.data"), "NonEmptyVector")

  private def _typename_zeromore(p: MAttributeType): TypeName =
    TypeName.Container(container_type_zeromore, _to_typename(p))

  private def _typename_onemore(p: MAttributeType): TypeName =
    TypeName.Container(container_type_onemore, _to_typename(p))

  private def _typename_range(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _typename_ranges(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _to_typename(p: MAttributeType): TypeName =
    p match {
      case m: MDatatype => to_typename(m)
    }

  protected def to_typename(p: MDatatype): TypeName =
    TypeName.Primitive.create(p.datatype)

  private def _to_methods(ps: List[MOperation]): MethodCompartment = {
    MethodCompartment.empty // TODO
  }

  protected def project_dir = "scala.d"

  protected def src_main = s"${project_dir}/src/main"

  protected def src_main_scala = s"$src_main/scala"

  protected def package_To_Pathname(p: MObject): String = {
    s"${src_main_scala}/${p.packageName.replace('.', '/')}"
  }

  protected def object_To_Pathname(p: MObject): String = {
    s"${package_To_Pathname(p)}/${p.name}.scala"
  }
}

object ScalaModelTransformer {
  sealed trait Purpose
  object Purpose {
    val elements = Vector(Plain, Create, Read, Update, Delete, Operation, View)

    case object Plain extends Purpose
    case object Create extends Purpose
    case object Read extends Purpose
    case object Update extends Purpose
    case object Delete extends Purpose
    case object Operation extends Purpose
    case object View extends Purpose
  }
}
