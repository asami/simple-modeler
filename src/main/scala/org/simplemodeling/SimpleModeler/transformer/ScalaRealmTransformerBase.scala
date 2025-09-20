package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.Strings
import org.goldenport.RAISE
import org.goldenport.values.PathName
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import org.goldenport.record.v2.{MOne =>_, MZeroOne => _, MOneMore => _, MZeroMore => _, MRange => _, MRanges => _, _}
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker.ScalaClassDefinition
import org.simplemodeling.SimpleModeler.transformer.maker._
import org.simplemodeling.SimpleModeler.transformer.maker.mobject.MPEntity
import org.simplemodeling.SimpleModeler.generators.scala.Scala3EntityGenerator

/*
 * Derived from SimpleModel2ScalaRealmTransformerBase (Nov. 19, 2012)
 * 
 * @since   Dec.  8, 2019
 *  version Dec.  8, 2019
 *  version May. 16, 2020
 *  version May. 18, 2025
 * @version Sep. 18, 2025
 * @author  ASAMI, Tomoharu
 */
trait ScalaRealmTransformerBase extends ProgramRealmTransformerBase {
  val fileSuffix = "scala"

  protected def make_Entity(model: PModel, p: PEntity): String = {
    val aspects = Nil
    val maker = new ScalaClassDefinition(context, model, aspects, p)
    maker.build()
    maker.toText
  }

  protected def package_File_Pathname(p: PObject): PathName = {
    val prjdir = "src" // TODO
    PathName(prjdir) :+ p.affiliation.packageName.replace('.', '/')
  }

  override protected def build_Entity(b: Realm.Builder, p: MEntity): Realm.Builder = {
    RAISE.notImplementedYetDefect
  }

  // /*
  //  * Lagacy
  //  */
  // protected def make_Entity(p: MEntity): String = {
  //   // val aspects = Nil
  //   // val model = p
  //   // val po = MPEntity(p)
  //   // val maker = new ScalaClassDefinition(context, model, aspects, po)
  //   // maker.build()
  //   // maker.toText
  //   val smodel = _to_scala(p)
  //   val g = new Scala3EntityGenerator()
  //   g.generate(smodel).take
  // }

  import org.simplemodeling.SimpleModeler.generator.scala.model._

  private def _to_scala(p: MEntity): SEntityClass = {
    val packagename = PackageName(p.packageName)
    val declaration = ClassDeclaration.CaseClass
    val classname = ClassName(p.name)
    val parentclass = p.base.map(_to_type)
    val traits = p.traits.map(_to_type)
    val parameters = _to_parameters(p.attributes)
    val methods = _to_methods(p.operations)
    val receptions = ReceptionCompartment.empty // TODO
    val core = ClassCore(
      packagename,
      declaration,
      classname,
      parentclass,
      traits,
      parameters,
      methods,
      receptions
    )
    SEntityClass(core)
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
