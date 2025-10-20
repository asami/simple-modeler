package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.record.v2._
import org.goldenport.util.StringUtils
import org.goldenport.scalaz.FoldTraverseUtil
import org.simplemodeling.SimpleModeler.generator.SourceArtifacts
import model._
import Generator.{State => GState, _}

/*
 * @since   May. 16, 2025
 *  version May. 19, 2025
 *  version Sep. 30, 2025
 * @version Oct. 17, 2025
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
    for {
      _ <- println("import cats.*")
      _ <- println("import cats.implicits.*")
      _ <- println("import cats.syntax.all.*")
      _ <- println("import org.simplemodeling.Consequence")
      _ <- println("import org.simplemodeling.record.Record")
      _ <- p.importNames.traverse_(x =>
        println(s"import ${x.fullName}")
      )
    } yield ()

  protected def section_class(p: T): GenM[Unit] =
    for {
      _ <- printws(p.declaration)
      _ <- print(p.className)
      _ <- parameter_list(p.parameterSequence)
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
      _ <- separator
      _ <- section_utility(p)
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def parameter_list(p: ParameterSequence): GenM[Unit] = {
    val s = p.parameters.map(parameter_item).mkString("(", ", ", ")")
    print(s)
  }

  protected def parameter_item(p: Parameter): String = {
    val s = p.name.name + ": " + p.typeName.name
    val d = if (p.isDefault) {
      val a = p.typeName match {
        case m: TypeName.Container if m.isOption => "None"
        case m: TypeName.Container if m.isList => "Nil"
        case m: TypeName.Container if m.isVector => "Vector.empty"
        case m: TypeName.Container if m.isSet => "Set.empty"
        case m: TypeName.Container => s"${m.container.name}.empty"
        case m: TypeName.Plain => s"${m.name}.empty"
        case m: TypeName.Primitive => m.datatype match {
          case XString => "\"\""
          case XInt => "0"
          case m => "0" // TODO
        }
      }
      s" = $a"
    } else {
      ""
    }
    s + d
  }

  protected def section_variables(p: T): GenM[Unit] = unit

  protected def section_methods(p: T): GenM[Unit] = unit

  protected def section_methods(p: SMethod): GenM[Unit] = unit

  protected def section_reception(p: T): GenM[Unit] = unit

  protected def section_utility(p: T): GenM[Unit] =
    for {
      _ <- with_methods(p)
      _ <- to_record_method(p)
    } yield ()

  protected def with_methods(p: T): GenM[Unit] = {
    val params = p.parameterSequence.parameters
    intercalateTraverse_(params, separator)(with_method(p, _))
  }

  protected def with_method(c: T, p: Parameter): GenM[Unit] = {
    val name = s"with${StringUtils.makeTitle(p.name.name)}"
    val param = p
    val rtype = TypeName.create(c)
    val m = SMethod.create(name, rtype, param) {
      val paramname = p.name.name
      println(s"copy($paramname = $paramname)")
    }
    define_method(m)
  }

  protected def to_record_method(c: T): GenM[Unit] = {
    val m = SMethod.create("toRecord", TypeName.create("org.simplemodeling.record", "Record")) {
      for {
        _ <- println("Record.data(")
        _ <- println(")")
      } yield ()
    }
    define_method(m)
  }

  // protected final def traverse_with_separator[T](ps: Vector[T]): GenM[Unit] =
  //   ???

  // protected final def traverse_with_separator[T](ps: Vector[T], f: T => GenM[Unit]): GenM[Unit] =
  //   ps.lastOption match {
  //     case Some(s) => ps.init.traverse_(_x(f)).flatMap(_ => f(s))
  //     case None => unit
  //   }

  // private def _x[T](f: T => GenM[Unit])(p: T): GenM[Unit] =
  //   kleisli_with_post(f, p)(separator)

  // protected final def kleisli_with_post[T](f: T => GenM[Unit], p: T)(post: => GenM[Unit]): GenM[Unit] =
  //   f(p).flatMap(_ => post)

  protected def section_object(p: T): GenM[Unit] =
    for {
      _ <- print("object ")
      _ <- print(p.className)
      _ <- println(" {")
      _ <- indent
      _ <- property_name_definitions(p)
      _ <- separator
      _ <- builder(p)
      _ <- separator
      _ <- createc_method(p)
      _ <- separator
      _ <- create_method(p)
      _ <- separator
      _ <- create_recordc_method(p)
      _ <- separator
      _ <- create_record_method(p)
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def property_name_definitions(p: T): GenM[Unit] = {
    val names = p.attributeSequence.attributes.map(_.name.name)
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
    s"PROP_${StringUtils.camelToUnderscore(p).toUpperCase}"

  protected def property_name_definition(p: String): GenM[Unit] =
    println(s"""final val ${property_name(p)} = "${p}"""")

  protected def builder(p: T): GenM[Unit] =
    for {
      _ <- print("case class Builder")
      _ <- parameter_list(to_builder_parameters(p.attributeSequence))
      _ <- println(" {")
      _ <- indent
      _ <- builder_buildc_method(p)
      _ <- separator
      _ <- builder_build_method(p)
      _ <- separator
      _ <- builder_build_recordc_method(p)
      _ <- separator
      _ <- builder_build_record_method(p)
      _ <- outdent
      _ <- println("}")
      _ <- separator
      _ <- println("object Builder {")
      _ <- indent
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def to_builder_parameters(p: AttributeSequence): ParameterSequence =
    ParameterSequence(p.attributes.map(to_builder_parameter))

  protected def to_builder_parameter(p: Attribute): Parameter =
    Parameter(ParameterName(p.name.name), to_optionable_type(p.typeName), true, true)

  protected def to_optionable_type(p: TypeName): TypeName =
    p match {
      case m: TypeName.Primitive => TypeName.option(m)
      case m: TypeName.Plain => TypeName.option(m)
      case m: TypeName.Container => m
    }

  protected def define_case_class(
    name: ClassName,
    params: ParameterSequence,
    methods: MethodCompartment
  ): GenM[Unit] =
    for {
      _ <- print("case class ")
      _ <- println(name.name)
      _ <- parameter_list(params)
      _ <- println(" {")
      _ <- indent
      _ <- define_methods(methods)
      _ <- outdent
      _ <- println("}")
      _ <- separator
      _ <- print("object ")
      _ <- print(name.name)
      _ <- println(" {")
      _ <- indent
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def define_methods(p: MethodCompartment): GenM[Unit] =
    p.methods.traverse_(define_method)

  protected def define_method(p: SMethod): GenM[Unit] =
    for {
      _ <- separator
      _ <- print("def ")
      _ <- print(p.name.name)
      _ <- parameter_list(p.parameters)
      _ <- print(": ")
      _ <- print(p.returnType.name)
      _ <- p.body match {
        case Some(s) => for {
          _ <- println(" = {")
          _ <- indent
          _ <- s()
          _ <- outdent
          _ <- println("}")
        } yield ()
        case None => unit
      }
    } yield ()

  protected def define_method_consequence_take(
    name: String,
    rtype: SClassBase,
    param: Parameter,
    params: Parameter*
  ): GenM[Unit] = define_method_consequence_take(
    name,
    TypeName.create(rtype),
    (param +: params)
  )

  protected def define_method_consequence_take(
    name: String,
    rtype: TypeName,
    param: Parameter,
    params: Parameter*
  ): GenM[Unit] = define_method_consequence_take(name, rtype, param +: params)

  protected def define_method_consequence_take(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  ): GenM[Unit] = {
    val m = SMethod.create(s"${name}", rtype, params) {
      println(s"""${name}C(${params.map(_.name.name).mkString(" ,")}).take""")
    }
    define_method(m)
  }

  protected def builder_buildc_method(p: T): GenM[Unit] =
    for {
      _ <- print("def buildC(): Consequence[")
      _ <- print(p.className.name)
      _ <- println("] = {")
      _ <- indent
      _ <- println("(")
      _ <- indent
      _ <- builder_parameters(p)
      _ <- outdent
      _ <- print(").mapN(")
      _ <- print(p.className.name)
      _ <- print(".apply")
      _ <- println(")")
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def builder_parameters(p: T): GenM[Unit] = {
    val a = p.parameterSequence
    a.parameters.lastOption match {
      case Some(s) => for {
        _ <- a.parameters.init.traverse_(builder_parameter)
        _ <- builder_parameter_last(s)
      } yield ()
      case None => unit
    }
  }

  protected def builder_parameter(p: Parameter): GenM[Unit] =
    for {
      _ <- print(builder_parameter_line(p))
      _ <- println(",")
    } yield ()

  protected def builder_parameter_last(p: Parameter): GenM[Unit] =
    println(builder_parameter_line(p))

  protected def builder_parameter_line(p: Parameter): String =
    builder_parameter_line(p.name.name)

  protected def builder_parameter_line(p: String): String =
    s"Consequence.takeOrMissingPropertyFault(${property_name(p)}, $p)"

  protected def builder_build_method(p: T): GenM[Unit] =
    for {
      _ <- print("def build(): ")
      _ <- print(p.className.name)
      _ <- println(" = {")
      _ <- indent
      _ <- println("buildC().take")
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def builder_build_recordc_method(p: T): GenM[Unit] = {
    val m = SMethod.create("buildC", TypeName.consequence(p), Parameter.record) {
      val attrs = p.attributeSequence.attributes
      for {
        _ <- println("copy(")
        _ <- indent
        _ <- FoldTraverseUtil.intercalateTraverseWithEnd_(
          attrs,
          println(", "),
          println()
        )(_build_attr_record)
        _ <- outdent
        _ <- println(")")
      } yield ()
    }
    define_method(m)
  }

  private def _build_attr_record(p: Attribute): GenM[Unit] = {
    for {
      _ <- print(p.name.name, " = record.getAs[", p.typeName.name, "](", property_name(p.name.name), ")")
    } yield ()
  }

  protected def builder_build_record_method(p: T): GenM[Unit] = {
    val param = Parameter.record
    define_method_consequence_take("build", p, param)
  }

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

  protected def create_record_method(p: T): GenM[Unit] = {
    val param = Parameter.record
    define_method_consequence_take("create", p, param)
  }

  protected def create_recordc_method(p: T): GenM[Unit] = {
    val m = SMethod.create("createC", TypeName.consequence(p), Parameter.record) {
      for {
        _ <- println("val builder = Builder()")
        _ <- println("builder.buildC(record)")
      } yield ()
    }
    define_method(m)
  }
}
