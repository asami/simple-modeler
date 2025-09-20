package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Sep. 19, 2025
 * @version Sep. 20, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class ScalaModelTransformer() extends PartialFunction[(MObject, ScalaModelTransformer.Purpose), Consequence[Vector[SClassBase]]] {
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
    val parameters = _to_parameters(p.attributes)
    val methods = _to_methods(p.operations)
    val receptions = ReceptionCompartment.empty // TODO
    ClassCore(
      packagename,
      declaration,
      classname,
      parentclass,
      traits,
      parameters,
      methods,
      receptions
    )
  }

  private def _to_type(p: MObjectRef): TypeName =
    TypeName(PackageName(p.packageName), p.name)

  private def _to_type(p: MTraitRef): TypeName =
    TypeName(PackageName(p.packageRef.packageName), p.name)

  private def _to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.map(_to_parameter))

  private def _to_parameter(p: MAttribute): Parameter = {
    val typename = p.multiplicity match {
      case MOne => _typename_one(p.attributeType)
      case MZeroOne => _typename_zeroone(p.attributeType)
      case MOneMore => _typename_zeromore(p.attributeType)
      case MZeroMore => _typename_onemore(p.attributeType)
      case m: MRange => _typename_range(p.attributeType)
      case m: MRanges => _typename_ranges(p.attributeType)
    }
        
    Parameter(ParameterName(p.name), typename)
  }

  private def _typename_one(p: MAttributeType): TypeName =
    _to_typename(p)

  private def _typename_zeroone(p: MAttributeType): TypeName =
    TypeName.Container(
      TypeName.Plain(PackageName("scala"), "Option"),
      _to_typename(p)
    )

  private def _typename_zeromore(p: MAttributeType): TypeName =
    TypeName.Container(
      TypeName.Plain(PackageName("scala.collection.immutable"), "Vector"),
      _to_typename(p)
    )

  private def _typename_onemore(p: MAttributeType): TypeName =
    TypeName.Container(
      TypeName.Plain(PackageName("cats.data"), "NonEmptyVector"),
      _to_typename(p)
    )

  private def _typename_range(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _typename_ranges(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _to_typename(p: MAttributeType): TypeName =
    p match {
      case m: MDatatype => _to_typename(m)
    }

  private def _to_typename(p: MDatatype): TypeName =
    TypeName.Primitive(p.datatype)

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
    val elements = Vector(Create, Update, Delete)

    case object Create extends Purpose
    case object Read extends Purpose
    case object Update extends Purpose
    case object Delete extends Purpose
    case object Operation extends Purpose
    case object View extends Purpose
  }
}
