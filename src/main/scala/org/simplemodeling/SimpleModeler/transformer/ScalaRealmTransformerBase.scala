package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.Strings
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.values.PathName
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import org.goldenport.record.v2.{MOne =>_, MZeroOne => _, MOneMore => _, MZeroMore => _, MRange => _, MRanges => _, _}
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainValue
import org.simplemodeling.SimpleModeler.transformer.maker.ScalaClassDefinition
import org.simplemodeling.SimpleModeler.transformer.maker._
import org.simplemodeling.SimpleModeler.transformer.maker.mobject.MPEntity
import org.simplemodeling.SimpleModeler.generators.scala._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer

/*
 * Derived from SimpleModel2ScalaRealmTransformerBase (Nov. 19, 2012)
 * 
 * @since   Dec.  8, 2019
 *  version May. 16, 2020
 *  version May. 18, 2025
 *  version Sep. 21, 2025
 *  version Feb. 28, 2026
 *  version Mar. 25, 2026
 *  version Apr.  5, 2026
 *  version May. 20, 2026
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
trait ScalaRealmTransformerBase extends ProgramRealmTransformerBase {
  val fileSuffix = "scala"

  override def transform(model: SimpleModel): TransformResult = {
    ScalaModelTransformer.clearObjectRegistry()
    model.elements.foreach {
      case m: MObject => ScalaModelTransformer.registerObject(m)
      case _ =>
    }
    super.transform(model)
  }

  protected def make_entity_legacy(model: PModel, p: PEntity): String = {
    val aspects = Nil
    val maker = new ScalaClassDefinition(context, model, aspects, p)
    maker.build()
    maker.toText
  }

  protected def package_file_pathname_legacy(p: PObject): PathName = {
    val prjdir = "src" // TODO
    PathName(prjdir) :+ p.affiliation.packageName.replace('.', '/')
  }

  override protected def build_entity(b: Realm.Builder, p: MEntity): Realm.Builder = {
    val g = new Scala3EntityFamilyGenerator()
    g.generate(p) match {
      case Consequence.Success(r, _) => r.build(b)
      case Consequence.Error(c) => c.RAISE
    }
  }

  override protected def build_value(b: Realm.Builder, p: MValue): Realm.Builder = {
    p match {
      case m: MDomainValue =>
        val g = new Scala3ValueFamilyGenerator()
        g.generate(m) match {
          case Consequence.Success(r, _) => r.build(b)
          case Consequence.Error(c) => c.RAISE
        }
      case m: MStructuredDataType =>
        val g = new Scala3ValueFamilyGenerator()
        g.generate(m) match {
          case Consequence.Success(r, _) => r.build(b)
          case Consequence.Error(c) => c.RAISE
        }
      case _ =>
        b
    }
  }

  override protected def build_powertype(b: Realm.Builder, p: MPowertype): Realm.Builder = {
    val g = new Scala3PowertypeFamilyGenerator()
    g.generate(p) match {
      case Consequence.Success(r, _) => r.build(b)
      case Consequence.Error(c) => c.RAISE
    }
  }

  override protected def build_state_machine(b: Realm.Builder, p: MStateMachine): Realm.Builder = {
    val g = new Scala3StateMachineFamilyGenerator()
    g.generate(p) match {
      case Consequence.Success(r, _) => r.build(b)
      case Consequence.Error(c) => c.RAISE
    }
  }

  // /*
  //  * Lagacy
  //  */
  // protected def make_entity_legacy(p: MEntity): String = {
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

  // import org.simplemodeling.SimpleModeler.generator.scala.model._

  // private def _to_scala(p: MEntity): SEntityClass = {
  //   val packagename = PackageName(p.packageName)
  //   val declaration = ClassDeclaration.CaseClass
  //   val classname = ClassName(p.name)
  //   val parentclass = p.base.map(_to_type)
  //   val traits = p.traits.map(_to_type)
  //   val parameters = _to_parameters(p.attributes)
  //   val methods = _to_methods(p.operations)
  //   val receptions = ReceptionCompartment.empty // TODO
  //   val core = ClassCore(
  //     packagename,
  //     declaration,
  //     classname,
  //     parentclass,
  //     traits,
  //     parameters,
  //     methods,
  //     receptions
  //   )
  //   SEntityClass(core)
  // }

  // private def _to_type(p: MObjectRef): TypeName =
  //   TypeName(PackageName(p.packageName), p.name)

  // private def _to_type(p: MTraitRef): TypeName =
  //   TypeName(PackageName(p.packageRef.packageName), p.name)

  // private def _to_parameters(ps: List[MAttribute]): ParameterSequence =
  //   ParameterSequence(ps.toVector.map(_to_parameter))

  // private def _to_parameter(p: MAttribute): Parameter = {
  //   val typename = p.multiplicity match {
  //     case MOne => _typename_one(p.attributeType)
  //     case MZeroOne => _typename_zeroone(p.attributeType)
  //     case MOneMore => _typename_zeromore(p.attributeType)
  //     case MZeroMore => _typename_onemore(p.attributeType)
  //     case m: MRange => _typename_range(p.attributeType)
  //     case m: MRanges => _typename_ranges(p.attributeType)
  //   }
        
  //   Parameter(ParameterName(p.name), typename)
  // }

  // private def _typename_one(p: MAttributeType): TypeName =
  //   _to_typename(p)

  // private def _typename_zeroone(p: MAttributeType): TypeName =
  //   TypeName.Container(
  //     TypeName.Plain(PackageName("scala"), "Option"),
  //     _to_typename(p)
  //   )

  // private def _typename_zeromore(p: MAttributeType): TypeName =
  //   TypeName.Container(
  //     TypeName.Plain(PackageName("scala.collection.immutable"), "Vector"),
  //     _to_typename(p)
  //   )

  // private def _typename_onemore(p: MAttributeType): TypeName =
  //   TypeName.Container(
  //     TypeName.Plain(PackageName("cats.data"), "NonEmptyVector"),
  //     _to_typename(p)
  //   )

  // private def _typename_range(p: MAttributeType): TypeName =
  //   _typename_zeromore(p) // TODO

  // private def _typename_ranges(p: MAttributeType): TypeName =
  //   _typename_zeromore(p) // TODO

  // private def _to_typename(p: MAttributeType): TypeName =
  //   p match {
  //     case m: MDatatype => _to_typename(m)
  //   }

  // private def _to_typename(p: MDatatype): TypeName =
  //   TypeName.Primitive(p.datatype)

  // private def _to_methods(ps: List[MOperation]): MethodCompartment = {
  //   MethodCompartment.empty // TODO
  // }

  protected def source_main_pathname = "src/main/scala"

  protected def project_dir = "scala.d"

  // protected def src_main = s"${project_dir}/src/main"

  // protected def src_main_scala = s"$src_main/scala"

  protected def src_main_scala = s"${project_dir}/${source_managed_main_pathname}"

  protected def package_pathname(p: MObject): String = {
    s"${src_main_scala}/${p.packageName.replace('.', '/')}"
  }

  protected def object_pathname(p: MObject): String = {
    s"${package_pathname(p)}/${p.name}.scala"
  }

  override protected def build_makefile(b: Realm): Realm =
    b.setContent("build.sbt", buildsbtcontent)

  val buildsbtcontent = """val scala3Version = "3.3.7"

def sampleVersion(envname: String, filename: String, fallback: String): String =
  sys.env.get(envname)
    .orElse {
      sys.env.get("CNCF_SAMPLES_ROOT").flatMap { root =>
        val versionfile = file(root) / "versions" / filename
        if (versionfile.isFile)
          Some(IO.read(versionfile).trim).filter(_.nonEmpty)
        else
          None
      }
    }
    .getOrElse(fallback)

val cncfversion = sampleVersion("CNCF_VERSION", "cncf-version.conf", "0.4.8")

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.example",
    name := "sample",
    version := "0.0.1-SNAPSHOT",

    scalaVersion := scala3Version,

    resolvers += Resolver.defaultLocal,
    resolvers += Resolver.file("Local Ivy", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns),
    resolvers += "Local Maven Repository" at ("file://" + Path.userHome.absolutePath + "/.m2/repository"),
    resolvers += "SimpleModeling.org" at "https://www.simplemodeling.org/maven",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test",
    libraryDependencies += "org.goldenport" %% "goldenport-cncf" % cncfversion,

    Compile / unmanagedSourceDirectories += (Compile / sourceManaged).value
  )
"""

  override protected def build_component(b: Realm.Builder, model: MComponent): Realm.Builder = {
    val g = new Scala3ComponentFamilyGenerator()
    g.generate(model) match {
      case Consequence.Success(r, _) => r.build(b)
      case Consequence.Error(c) => c.RAISE
    }
  }
}
