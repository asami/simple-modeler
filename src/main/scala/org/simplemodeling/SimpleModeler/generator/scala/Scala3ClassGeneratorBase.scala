package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import org.goldenport.RAISE
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
 *  version Oct. 17, 2025
 *  version Nov. 18, 2025
 * @version Feb. 17, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class Scala3ClassGeneratorBase[T <: SClassBase](
  context: ScalaModel.Context
) extends Generator[T, SourceArtifacts] {
  import Scala3ClassGeneratorBase._

  protected final def scala_context: ScalaModel.Context = context

  def classkind: ClassKind

  def generate(p: T): Consequence[SourceArtifacts] = {
    val r = run(p)
    val config = Config()
    val init = GState()
    r.run(config, init).map { x =>
      val (output, s, state) = x
      s
    }
  }

  def run(p: T): GenM[SourceArtifacts] = new Scala3ClassGeneratorExecutor(scala_context, classkind, p).run()
}

object Scala3ClassGeneratorBase {
  sealed trait ClassKind {
    def isValue: Boolean = false
  }
  object ClassKind {
    case object Value extends ClassKind {
      override def isValue = true
    }
    case object EntityValue extends ClassKind {
      override def isValue = true
    }
    case object Control extends ClassKind
    case object Component extends ClassKind
  }
}

class Scala3ClassGeneratorExecutor[T <: SClassBase](
  context: ScalaModel.Context,
  classKind: Scala3ClassGeneratorBase.ClassKind,
  val clazz: T
) extends ComponentPart[T] {
  import Scala3ClassGeneratorBase._

  protected final def scala_context = context

  // protected final def is_entity = classKind match {
  //   case ClassKind.Entity => true
  //   case _ => false
  // }

  protected final def is_value = classKind.isValue

  protected final def class_type_name: TypeName = TypeName.create(clazz)

  protected final def parameters_vector: Vector[Parameter] =
    clazz.parameterSequence.parameters

  protected final def attributes_vector: Vector[Attribute] =
    clazz.attributeSequence.attributes

  def run(): GenM[SourceArtifacts] =
    for {
      _ <- section_package
      _ <- separator
      _ <- section_import
      _ <- separator
      _ <- section_class
      _ <- separator
      _ <- section_object
      s <- build
    } yield {
      val pkgpath = clazz.packageName.toPathName
      val path = s"${pkgpath}/${clazz.className.name}.scala"
      SourceArtifacts.create(path, s)
    }

  protected def section_package: GenM[Unit] =
    for {
      _ <- print("package ")
      _ <- println(clazz.packageName)
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

  protected def section_import: GenM[Unit] =
    for {
      _ <- println("import scala.language.strictEquality")
//      _ <- println("import scala.language.dynamics")
      _ <- println("import cats.*")
      _ <- println("import cats.implicits.*")
      _ <- println("import cats.syntax.all.*")
      _ <- println("import cats.derived.*")
      _ <- println("import io.circe.Codec")
      _ <- println("import io.circe.generic.semiauto.*")
      _ <- println("import org.goldenport.Consequence")
      _ <- println("import org.goldenport.record.Record")
      _ <- println("import org.goldenport.protocol.*")
      _ <- println("import org.goldenport.protocol.spec.*")
      _ <- println("import org.goldenport.protocol.operation.*")
      _ <- println("import org.goldenport.cncf.action.*")
      _ <- println("import org.goldenport.cncf.component.*")
      _ <- clazz.importNames.traverse_(x =>
        println(s"import ${x.fullName}")
      )
    } yield ()

  protected def section_class: GenM[Unit] =
    for {
      _ <- printws(clazz.declaration)
      _ <- print(clazz.className)
      _ <- parameter_list(clazz.parameterSequence)
      _ <- {
        def _extends_(c: String, ts: List[String]) =
          s" extends ${c}" + ts.mkString(" with", " with ", " ")
        val s = (clazz.parentClass, clazz.traitList) match {
          case (Some(s), Nil) => s" extends ${s.name} "
          case (Some(s), xs) => _extends_(s.name, xs.map(_.name))
          case (None, Nil) => " "
          case (None, x :: xs) => _extends_(x.name, xs.map(_.name))
        }
        print(s)
      }
      _ <- declare_derives
      _ <- println("{")
      _ <- indent
      _ <- section_import_in_class
      _ <- section_variables
      _ <- separator
      _ <- section_methods
      _ <- separator
      _ <- section_reception
      _ <- separator
      _ <- section_utility
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def declare_derives: GenM[Unit] =
    classKind match {
      case ClassKind.EntityValue => print("derives Codec.AsObject ") // Case class
      case ClassKind.Value => print("derives Eq, Codec.AsObject ") // Case class
      case _ => unit
    }

  protected def section_import_in_class: GenM[Unit] =
    println(s"import ${clazz.className}.*")

  protected def section_variables: GenM[Unit] = unit

  protected def section_methods: GenM[Unit] = unit

  protected def section_methods(p: SMethod): GenM[Unit] = unit

  protected def section_reception: GenM[Unit] = unit

  protected def section_utility: GenM[Unit] =
    for {
      _ <- with_methods
      _ <- lenslikeupdate_methods
      _ <- validate_method
      _ <- iri_method
      _ <- properties_method
      _ <- to_record_method
    } yield ()

  protected def with_methods: GenM[Unit] = {
    val params = clazz.parameterSequence.parameters
    intercalateTraverse_(params, separator)(with_method)
  }

  protected def with_method(p: Parameter): GenM[Unit] = {
    val name = s"with${StringUtils.makeTitle(p.name.name)}"
    val param = p
    val rtype = TypeName.create(clazz)
    val m = SMethod.create(name, rtype, param) {
      val paramname = p.name.name
      println(s"copy($paramname = $paramname)")
    }
    define_method(m)
  }

  protected def lenslikeupdate_methods: GenM[Unit] =
    println("// lenslikeupdate_methods")
    // intercalateTraverse_(parameters_vector, separator) { p =>
    //   val name = s"update${p.titleName}"
    //   val param = Parameter.create("f", TypeName.Function(p.typeName, p.typeName))
    //   val pname = p.name.name
    //   val m = SMethod.create(name, class_type_name, param) {
    //     println(s"copy($pname = f($pname))")
    //   }
    //   define_method(m)
    // }

  protected def validate_method: GenM[Unit] =
    println("// validate_method")

  protected def iri_method: GenM[Unit] =
    println("// iri_method")

  protected def properties_method: GenM[Unit] =
    println("// properties_method")

  protected def to_record_method: GenM[Unit] =
    if (is_value) {
      val m = SMethod.create("toRecord", TypeName.create("org.goldenport.record", "Record")) {
        for {
          _ <- println("Record.data(")
          _ <- _to_record
          _ <- println(")")
        } yield ()
      }
      define_method(m)
    } else {
      unit
    }

  private def _to_record: GenM[Unit] =
    FoldTraverseUtil.intercalateTraverseWithEnd_(
      attributes_vector,
      println(", "),
      println()
    )(_to_record)

  private def _to_record(p: Attribute): GenM[Unit] =
    println(property_name(p.name.name), " -> ", p.name.name)

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

  protected def section_object: GenM[Unit] =
    for {
      _ <- print("object ")
      _ <- print(clazz.className)
      _ <- println(" {")
      _ <- indent
      _ <- property_name_definitions
      _ <- separator
      _ <- schema
      _ <- separator
      _ <- given_typeclasses
      _ <- builder_part
      _ <- component_object_part
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def property_name_definitions: GenM[Unit] = {
    val names = clazz.attributeSequence.attributes.map(_.name.name)
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

  protected def schema: GenM[Unit] =
    println("// Schema")

  protected def builder_part: GenM[Unit] =
    if (is_value)
      for {
        _ <- separator
        _ <- builder
        _ <- separator
        _ <- createc_method_required
        _ <- separator
        _ <- create_method
        _ <- separator
        _ <- create_recordc_method
        _ <- separator
        _ <- create_record_method
      } yield {}
    else
      unit

  protected def builder: GenM[Unit] =
    for {
      _ <- print("case class Builder")
      _ <- parameter_list(to_builder_parameters(clazz.attributeSequence))
      _ <- println(" {")
      _ <- indent
      _ <- builder_with_methods
      _ <- separator
      _ <- builder_buildc_method
      _ <- separator
      _ <- builder_build_method
      _ <- separator
      _ <- builder_build_recordc_method
      _ <- separator
      _ <- builder_build_record_method
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

  protected def define_method(
    name: String,
    rtype: SClassBase,
    param: Parameter,
    params: Parameter*
  )(body: => GenM[Unit]): GenM[Unit] =
    define_method(name, rtype, (param +: params))(body)

  protected def define_method(
    name: String,
    rtype: TypeName,
    param: Parameter,
    params: Parameter*
  )(body: => GenM[Unit]): GenM[Unit] =
    define_method(name, rtype, (param +: params))(body)

  protected def define_method(
    name: String,
    rtype: SClassBase,
    params: Seq[Parameter]
  )(body: => GenM[Unit]): GenM[Unit] =
    define_method(name, TypeName.create(rtype), params)(body)

  protected def define_method(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  )(body: => GenM[Unit]): GenM[Unit] = {
    val m = SMethod.create(name, rtype, params)(body)
    define_method(m)
  }

  // protected def define_cmethods_force(
  //   name: String,
  //   rtype: TypeName,
  //   param: Parameter,
  //   params: Parameter*
  // )(body: => GenM[Unit]): GenM[Unit] =
  //   define_cmethods_force(name, rtype, param +: params)(body)

  // protected def define_cmethods_force(
  //   name: String,
  //   rtype: TypeName,
  //   params: Seq[Parameter]
  // )(body: => GenM[Unit]): GenM[Unit] = RAISE.notImplementedYetDefect

  protected def define_cmethods(
    name: String,
    rtype: TypeName,
    param: Parameter,
    params: Parameter*
  )(body: => GenM[Unit]): GenM[Unit] =
    define_cmethods(name, rtype, param +: params)(body)

  protected def define_cmethods(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  )(body: => GenM[Unit]): GenM[Unit] =
    define_cmethods_safe(name, rtype, params)(body)

  protected def define_cmethods_safe(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  )(body: => GenM[Unit]): GenM[Unit] = {
    val cname = name + "C"
    for {
      _ <- define_method_delegate(name, cname, type_consequence(rtype), params)
      _ <- define_method(cname, type_consequence(rtype), params)(body)
      _ <- define_method_unsafe_consequence_take(name, rtype, params)
    } yield ()
  }

  protected def define_cmethods_unsafe(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  )(body: => GenM[Unit]): GenM[Unit] = {
    val cname = name + "C"
    val uname = name + "U"
    for {
      _ <- define_method(cname, type_consequence(rtype), params)(body)
      _ <- define_method_consequence_take(name, rtype, params)
      _ <- define_method_consequence_take(uname, rtype, params)
    } yield ()
  }

  protected def define_method_delegate(
    name: String,
    target: String,
    rtype: SClassBase,
    param: Parameter,
    params: Parameter*
  ): GenM[Unit] = define_method_delegate(
    name,
    target,
    TypeName.create(rtype),
    param +: params
  )

  protected def define_method_delegate(
    name: String,
    target: String,
    rtype: TypeName,
    param: Parameter,
    params: Parameter*
  ): GenM[Unit] = define_method_delegate(name, target, rtype, param +: params)

  protected def define_method_delegate(
    name: String,
    target: String,
    rtype: TypeName,
    params: Seq[Parameter]
  ): GenM[Unit] = {
    val m = SMethod.create(s"${name}", rtype, params) {
      println(s"""${target}(${params.map(_.name.name).mkString(" ,")})""")
    }
    define_method(m)
  }

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

  protected def define_method_unsafe_consequence_take(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  ): GenM[Unit] = {
    val m = SMethod.create(s"${name}U", rtype, params) {
      println(s"""${name}C(${params.map(_.name.name).mkString(" ,")}).take""")
    }
    define_method(m)
  }

  protected def builder_with_methods: GenM[Unit] = {
    val params = clazz.parameterSequence.parameters
    FoldTraverseUtil.intercalateTraverse_(
      params,
      separator
    )(_builder_with_methods)
  }

  protected val package_type = clazz.packageName

  protected val companion_package = PackageName(clazz.packageName, clazz.className.name)

  protected def consequence_type(p: TypeName): TypeName = TypeName.consequence(p)

  protected val builder_type = TypeName(companion_package, "Builder")

  protected val consequence_builder_type = consequence_type(builder_type)

  protected final def option_type(p: TypeName): TypeName = scala_context.optionType(p)

  protected final def string_type: TypeName = scala_context.stringType

  protected final def option_parameter(p: Parameter): Parameter = scala_context.optionParameter(p)

  protected final def string_parameter(p: Parameter): Parameter = scala_context.stringParameter(p)

  protected final def short_parameter(p: Parameter): Parameter = scala_context.shortParameter(p)

  protected final def int_parameter(p: Parameter): Parameter = scala_context.intParameter(p)

  protected final def long_parameter(p: Parameter): Parameter = scala_context.longParameter(p)

  protected final def float_parameter(p: Parameter): Parameter = scala_context.floatParameter(p)

  protected final def double_parameter(p: Parameter): Parameter = scala_context.doubleParameter(p)

  protected final def bigint_parameter(p: Parameter): Parameter = scala_context.bigintParameter(p)

  protected final def bigdecimal_parameter(p: Parameter): Parameter = scala_context.bigdecimalParameter(p)


  protected final def type_consequence(p: TypeName): TypeName = TypeName.consequence(p)

  private def _builder_with_methods(p: Parameter): GenM[Unit] = {
    val propname = p.name.name
    def methodname = "with" + StringUtils.makeTitle(propname)
    val optionp = Parameter(p.name, option_type(p.typeName))

    for {
      _ <- define_method(methodname, builder_type, p) {
        println("copy(", propname, " = Some(", propname, "))")
      }
      _ <- define_method(methodname, builder_type, option_parameter(p)) {
        println("copy(", propname, " = ", propname, ")")
      }
      _ <- _builder_with_methods_parse(p)
    } yield ()
  }

  private def _builder_with_methods_parse(p: Parameter): GenM[Unit] =
    if (p.typeName.isString) {
      unit
    } else {
      for {
        _ <- _builder_with_method_parse(p.name.name, p.typeName, string_parameter(p))
        _ <- _builder_with_methods_parse_number(p)
      } yield ()
    }

  private def _builder_with_methods_parse_number(p: Parameter): GenM[Unit] =
    if (p.typeName.isNumberOrigin) {
      for {
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, short_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, int_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, long_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, float_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, double_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, bigint_parameter(p))
        _ <- _builder_with_method_parse_if_required(p.name.name, p.typeName, bigdecimal_parameter(p))
      } yield ()
    } else {
      unit
    }

  private def _builder_with_method_parse_if_required(name: String, rtype: TypeName, param: Parameter): GenM[Unit] =
    if (rtype.contentType == param.typeName)
      unit
    else
      _builder_with_method_parse(name, rtype, param)

  private def _builder_with_method_parse(name: String, rtype: TypeName, param: Parameter): GenM[Unit] = 
    rtype match {
      case m: TypeName.Primitive =>
        _builder_with_method_parse_primitive(name, m, param)
      case m => _builder_with_method_parse_plain(name, m, param)
    }

  private def _builder_with_method_parse_primitive(name: String, rtype: TypeName.Primitive, param: Parameter): GenM[Unit] =
    if (param.typeName.isPrimitive)
      _builder_with_method_parse_primitive_primitive(name, rtype, param)
    else
      _builder_with_method_parse_primitive_plain(name, rtype, param)

  private def _builder_with_method_parse_primitive_primitive(name: String, rtype: TypeName.Primitive, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val propname = name
    val primitivename = rtype.name
    define_cmethods(methodname, builder_type, param) {
      println("Consequence.to", primitivename, "(", propname, ").map(x => copy(", propname, " = Some(x)))")
    }
  }

  private def _builder_with_method_parse_primitive_plain(name: String, rtype: TypeName.Primitive, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val propname = name
    val primitivename = rtype.name
    define_cmethods(methodname, builder_type, param) {
      println(propname, ".to" + primitivename, ".map(x => copy(", propname, " = Some(x)))")
    }
  }

  private def _builder_with_method_parse_plain(name: String, rtype: TypeName, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val propname = name
    define_cmethods(methodname, builder_type, param) {
      println(rtype.name, ".parse(", propname, ").map(x => copy(", propname, " = Some(x)))")
    }
  }

  protected final def with_method_name(name: String): String =
    s"with${StringUtils.makeTitle(name)}"

  protected def builder_buildc_method: GenM[Unit] = {
    val m = SMethod.create("buildC", TypeName.consequence(clazz)) {
      _builder_buildc_method_body(builder_parameters)
    }
    define_method(m)
  }

  private def _builder_buildc_method_body(body: => GenM[Unit]): GenM[Unit] = {
    for {
      _ <- println("(")
      _ <- indent
      _ <- body
      _ <- outdent
      _ <- print(").mapN(")
      _ <- print(clazz.className.name)
      _ <- print(".apply")
      _ <- println(")")
    } yield ()
  }

  protected def builder_parameters: GenM[Unit] = {
    val a = clazz.parameterSequence
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

  protected def builder_build_method: GenM[Unit] =
    for {
      _ <- print("def build(): ")
      _ <- print(clazz.className.name)
      _ <- println(" = {")
      _ <- indent
      _ <- println("buildC().take")
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def builder_build_recordc_method: GenM[Unit] = {
    val m = SMethod.create("buildC", TypeName.consequence(clazz), Parameter.record) {
      _builder_buildc_method_body(_builder_record_parameters)
    }
    define_method(m)
  }

  private def _builder_record_parameters: GenM[Unit] = {
    val params = clazz.parameterSequence.parameters
    for {
      _ <- FoldTraverseUtil.intercalateTraverseWithEnd_(
        params,
        println(", "),
        println()
      )(_build_record_param)
    } yield ()
  }

  private def _build_record_param(p: Parameter): GenM[Unit] =
    _build_param_or_var(p)(_get_record_param(p))

  private def _build_param_or_var(p: Parameter)(param: => GenM[Unit]): GenM[Unit] =
    for {
      _ <- print("Consequence.takeOrMissingPropertyFault(", property_name(p.name.name), ", ")
      _ <- param
      _ <- print(" orElse ", p.name.name, ")")
    } yield ()

  private def _get_record_param(p: Parameter): GenM[Unit] =
    print("record.getAs[", p.typeName.name, "](", property_name(p.name.name), ")")

  // protected def builder_build_recordc_method(p: T): GenM[Unit] = {
  //   val m = SMethod.create("buildC", TypeName.consequence(p), Parameter.record) {
  //     val attrs = p.attributeSequence.attributes
  //     for {
  //       _ <- println("copy(")
  //       _ <- indent
  //       _ <- FoldTraverseUtil.intercalateTraverseWithEnd_(
  //         attrs,
  //         println(", "),
  //         println()
  //       )(_build_attr_record)
  //       _ <- outdent
  //       _ <- println(")")
  //     } yield ()
  //   }
  //   define_method(m)
  // }

  // private def _build_attr_record(p: Attribute): GenM[Unit] = {
  //   for {
  //     _ <- print(p.name.name, " = record.getAs[", p.typeName.name, "](", property_name(p.name.name), ")")
  //   } yield ()
  // }

  protected def builder_build_record_method: GenM[Unit] = {
    val param = Parameter.record
    define_method_consequence_take("build", clazz, param)
  }

  protected def create_method: GenM[Unit] = {
    for {
      _ <- print("def create(")
      _ <- required_parameter_list(clazz.parameterSequence)
      _ <- println(s"): ${clazz.className.name} = {")
      _ <- indent
      _ <- print("createC(")
      _ <- required_invoke_parameters(clazz.parameterSequence)
      _ <- println(").take")
      _ <- outdent
      _ <- println("}")
    } yield ()
  }

  protected def createc_method_required: GenM[Unit] = {
    createc_method(clazz.parameterSequence.requiredPatameters)
  }

  protected def createc_method(params: Seq[Parameter]): GenM[Unit] = {
    for {
      _ <- print("def createC")
      _ <- parameter_list(params)
      _ <- println(s": Consequence[${clazz.className.name}] = {")
      _ <- indent
      _ <- builder_build(params)
      _ <- outdent
      _ <- println("}")
    } yield ()
  }

  protected def builder_build(params: Seq[Parameter]): GenM[Unit] = {
    def _with_(p: Parameter) = s".with${StringUtils.makeTitle(p.name.name)}(${p.name})"
    val withs = params.map(_with_).mkString
    for {
      _ <- println("val builder = Builder()")
      _ <- indent
      _ <- print("builder")
      _ <- println(withs)
      _ <- outdent
      _ <- println("builder.buildC()")
    } yield ()
  }

  protected def create_record_method: GenM[Unit] = {
    val param = Parameter.record
    define_method_consequence_take("create", clazz, param)
  }

  protected def create_recordc_method: GenM[Unit] = {
    val m = SMethod.create("createC", TypeName.consequence(clazz), Parameter.record) {
      for {
        _ <- println("val builder = Builder()")
        _ <- println("builder.buildC(record)")
      } yield ()
    }
    define_method(m)
  }

  // object Age:
  //     given Eql[Age, Age] = Eql.derived
  //     given Eq[Age]       = Eq.by(_.value)
  //     given Hash[Age]     = Hash.by(_.value)
  //     given Ordering[Age] = Ordering.by(_.value)
  //     given Show[Age]     = Show.show(a => s"Age(${a.value})")
  //     given Encoder[Age]  = Encoder.encodeInt.contramap(_.value)
  //     given Decoder[Age]  = Decoder.decodeInt.map(Age(_))

  protected def given_typeclasses: GenM[Unit] = {
    val name = clazz.className.name
    // println(s"given Eql[", name, ",", name, "]")
    for {
      _ <- _can_equal(name)
      _ <- _eq(name)
    } yield ()
  }

  private def _can_equal(name: String): GenM[Unit] =
    if (is_value)
      println(s"given CanEqual[", name, ",", name, "] = CanEqual.derived")
    else
      unit

  private def _eq(name: String): GenM[Unit] =
    if (is_value) {
      println("// Domain semantic equality = equality of identity")
      println(s"given Eq[", name, "] = Eq.by(_.id)")
    } else {
      unit
    }

  //
  protected def parameter_list(p: ParameterSequence): GenM[Unit] =
    parameter_list(p.parameters)

  protected def parameter_list(ps: Seq[Parameter]): GenM[Unit] = {
    val s = ps.map(parameter_item).mkString("(", ", ", ")")
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

  protected final def required_parameter_list(p: ParameterSequence): GenM[Unit] = {
    val xs = p.requiredPatameters.map(parameter_item)
    print_list(xs)
  }

  protected final def required_invoke_parameters(p: ParameterSequence): GenM[Unit] = {
    val xs = p.requiredPatameters.map(_.name.name)
    print_list(xs)
  }

  protected final def print_list(ps: Seq[String]): GenM[Unit] =
    print(ps.mkString(", "))

  protected final def print_parenthes_list(ps: Seq[String]): GenM[Unit] =
    print(ps.mkString("(", ", ", ")"))
}
