package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.util.StringUtils
import org.simplemodeling.SimpleModeler.generator.SourceArtifacts
import model._
import Generator.{State => GState, _}

/*
 * @since   May. 16, 2025
 *  version May. 19, 2025
 * @version Sep. 25, 2025
 * @author  ASAMI, Tomoharu
 */
trait Scala3ClassGeneratorBase[T <: SClassBase] extends Generator[T, SourceArtifacts] {
  def generate(p: T): Consequence[SourceArtifacts] = {
    val r = run(p)
    val config = Config()
    val init = GState()
    r.run(config, init).map { x =>
      val (output, s, state) = x
      s
    }
  }

  def run(ast: T): GenM[SourceArtifacts] =
    for {
      _ <- section_package(ast)
      _ <- separator
      _ <- section_import(ast)
      _ <- separator
      _ <- section_class(ast)
      _ <- separator
      _ <- section_object(ast)
      s <- build
    } yield {
      val pkgpath = ast.packageName.toPathName
      val path = s"${pkgpath}/${ast.className.name}.scala"
      SourceArtifacts.create(path, s)
    }

  protected def section_package(p: T): GenM[Unit] =
    for {
      _ <- print("package ")
      _ <- println(p.packageName)
    } yield ()

  // protected def section_import_bak(p: T): GenM[Unit] =
  //   for {
  //     _ <- {
  //       val o = p.importNames.foldLeft(Output.empty)((z, x) =>
  //         z.print("import ").println(x.fullName))
  //       add(o)
  //     }
  //   } yield ()

  // protected def section_import(p: T): GenM[Unit] =
  //   p.importNames.traverse_(x => add(Output().print("import ").println(x.fullName)))

  protected def section_import(p: T): GenM[Unit] =
    p.importNames.traverse_(x =>
      println(s"import ${x.fullName}")
    )

  protected def section_class(p: T): GenM[Unit] =
    for {
      _ <- printws(p.declaration)
      _ <- print(p.className)
      _ <- {
        val ps = p.parameterSequence
        val s = ps.parameters.map(x => x.name.name + ": " + x.typeName.name).mkString("(", ", ", ")")
        print(s)
      }
      _ <- {
        def _extends_(c: String, ts: List[String]) =
          s" extends ${c}" + ts.mkString(" with", " with ", " ")
        val s = (p.parentClass, p.traitList) match {
          case (Some(s), Nil) => s" extends ${s.name} "
          case (Some(s), xs) => _extends_(s.name, xs.map(_.name))
          case (None, Nil) => " "
          case (None, x :: xs) => _extends_(x.name, xs.map(_.name))
        }
        print(s)
      }
      _ <- println("{")
      _ <- indent
      _ <- section_variables(p)
      _ <- separator
      _ <- section_methods(p)
      _ <- separator
      _ <- section_reception(p)
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def section_variables(p: T): GenM[Unit] = unit

  protected def section_methods(p: T): GenM[Unit] = unit

  protected def section_methods(p: SMethod): GenM[Unit] = unit

  protected def section_reception(p: T): GenM[Unit] = unit

  protected def section_object(p: T): GenM[Unit] =
    for {
      _ <- print("object ")
      _ <- print(p.className)
      _ <- println(" {")
      _ <- indent
      _ <- property_name_definitions(p)
      _ <- builder(p)
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def property_name_definitions(p: T): GenM[Unit] = {
    val names = p.effectiveAttributeSequence.attributes.map(_.name.name)
    names.traverse_(property_name_definition)
  }

  // protected def property_name_definitions(p: T): GenM[Unit] = {
  //   val names = p.effectiveAttributeSequence.attributes.map(_.name.name)
  //   names.foldLeftM(()) { case (_, x) => property_name_definition(x) }
  // }

  // protected def property_name_definitions(p: T): GenM[Vector[Unit]] = {
  //   val names = p.effectiveAttributeSequence.attributes.map(_.name.name)
  //   names.traverse_ { x =>
  //     val output = Output().
  //     property_name_definition(x)
  //   }
  // }

  protected def property_name(p: String): String =
    s"ATTR_${StringUtils.camelToUnderscore(p).toUpperCase}"

  protected def property_name_definition(p: String): GenM[Unit] =
    println(s"""final val ${property_name(p)} = "${p}"""")

  protected def builder(p: T): GenM[Unit] =
    for {
      _ <- println("case class Builder(")
      _ <- println(") {")
      _ <- indent
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def create_method(p: T): GenM[Unit] = {
    for {
      _ <- print("def create(")
      _ <- println(s"): ${p.className.name} = {")
      _ <- println(") {")
      _ <- indent
      _ <- print("createC(")
      _ <- println(").take")
      _ <- outdent
      _ <- println("}")
    } yield ()
  }

  protected def createc_method(p: T): GenM[Unit] = {
    for {
      _ <- print("def createC(")
      _ <- println(s"): Consequence[${p.className.name}] = {")
      _ <- println(") {")
      _ <- indent
      _ <- outdent
      _ <- println("}")
    } yield ()
  }
}
