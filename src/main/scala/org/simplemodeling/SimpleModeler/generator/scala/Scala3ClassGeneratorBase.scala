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
 *  version Feb. 28, 2026
 * @version Mar. 24, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class Scala3ClassGeneratorBase[T <: SClassBase](
  context: ScalaModel.Context
) extends Generator[T, SourceArtifacts] {
  import Scala3ClassGeneratorBase._

  protected final def scala_context: ScalaModel.Context = context

  // default class kind
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

  def run(p: T): GenM[SourceArtifacts] = {
    val ck = p.directive.classKind getOrElse classkind
    new Scala3ClassGeneratorExecutor(scala_context, ck, p).run()
  }
}

object Scala3ClassGeneratorBase {
  sealed trait ClassKind {
    def isValue: Boolean = false
    def isEntityValue: Boolean = false
  }
  object ClassKind {
    case object Value extends ClassKind {
      override def isValue = true
    }
    case object EntityValue extends ClassKind {
      override def isValue = true
      override def isEntityValue: Boolean = true
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

  protected final def is_entity_value_create: Boolean =
    is_entity_value && clazz.directive.isCreate

  protected final def is_value = classKind.isValue
  protected final def is_entity_value = classKind.isEntityValue
  protected final def is_query = clazz.directive.isQuery
  protected final def is_update = clazz.directive.isUpdate

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
      _ <- println(clazz.packageName.name)
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
      _ <- println("import org.goldenport.ConsequenceT")
      _ <- println("import org.goldenport.datatype.*")
      _ <- println("import org.goldenport.schema.Schema")
//      _ <- println("import org.goldenport.value.*")
      _ <- println("import org.goldenport.record.Record")
      _ <- println("import org.goldenport.protocol.*")
      _ <- println("import org.goldenport.protocol.spec.*")
      _ <- println("import org.goldenport.protocol.operation.*")
      _ <- println("import org.simplemodeling.model.datatype.*")
      _ <- println("import org.simplemodeling.model.value.*")
      _ <- println("import org.simplemodeling.model.directive.*")
      _ <- println("import org.goldenport.cncf.directive.*")
      _ <- println("import org.goldenport.cncf.action.*")
      _ <- println("import org.goldenport.cncf.component.*")
      _ <- println("import org.goldenport.cncf.statemachine.*")
      _ <- println("import org.goldenport.cncf.unitofwork.ExecUowM")
      _ <- println("import org.goldenport.cncf.unitofwork.UnitOfWork.uowmNotImplemented")
      _ <- println("import org.goldenport.cncf.entity.*")
      _ <- clazz.importNames.traverse_(x =>
        println(s"import ${x.fullName}")
      )
    } yield ()

  protected def section_class: GenM[Unit] =
    for {
      _ <- printws(clazz.declaration)
      _ <- print(clazz.className)
      _ <- class_parameter_list(clazz.parameterSequence)
      _ <- {
        def _extends_(c: String, ts: List[String]) =
          s" extends ${c}" + (
            ts match {
              case Nil => ""
              case xs => " with " + xs.mkString(" with ")
            }
          )
        val ts = clazz.traitList.map(_typename_for_extends) ++ _augument_traits
        val s = (clazz.parentClass, ts) match {
          case (Some(s), Nil) => s" extends ${_typename_for_extends(s)} "
          case (Some(s), xs) => _extends_(_typename_for_extends(s), xs)
          case (None, Nil) => " "
          case (None, x :: xs) => _extends_(x, xs)
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

  private def _augument_traits: List[String] = {
    if (is_query)
      List("EntityPersistableQuery")
    else if (is_update)
      List("EntityPersistableUpdate")
    else if (is_entity_value_create)
      List("EntityPersistableCreate")
    else if (classKind == ClassKind.Component)
      List("CollectionTransitionRuleProvider")
    else if (is_entity_value)
      List("EntityPersistable")
    else
      Nil
  }

  private def _typename_for_extends(p: TypeName): String = p match {
    case TypeName.Plain(pkg, name, _) if pkg.name == "org.simplemodeling.model" && name == "SimpleEntity" =>
      p.fullName
    case TypeName.Plain(pkg, name, _) if pkg.name == "org.simplemodeling.model" &&
      (name == "SimpleEntityCreate" || name == "SimpleEntityUpdate" || name == "SimpleEntityQuery") =>
      p.fullName
    case _ =>
      p.name
  }

  protected def declare_derives: GenM[Unit] =
    classKind match { // TODO Eq
      case ClassKind.EntityValue if _is_codec_derives_supported =>
        print(" derives Codec.AsObject ") // Case class
      case ClassKind.Value if _is_codec_derives_supported =>
        print(" derives Eq, Codec.AsObject ") // Case class
      case ClassKind.Value =>
        print(" derives Eq ") // Case class
      case _ => unit
    }

  private def _is_codec_derives_supported: Boolean =
    !clazz.parameterSequence.parameters.exists(p => _is_simple_object_attribute_type(p.typeName))

  protected def section_import_in_class: GenM[Unit] =
    println(s"import ${clazz.className}.*")

  protected def section_variables: GenM[Unit] =
    _simple_object_attribute_alias_definitions

  protected def section_methods: GenM[Unit] = unit

  protected def section_methods(p: SMethod): GenM[Unit] = unit

  protected def section_reception: GenM[Unit] = unit

  protected def section_utility: GenM[Unit] =
    for {
      _ <- schema_accessor_method
      _ <- separator
      _ <- with_methods
      _ <- lenslikeupdate_methods
      _ <- validate_method
      _ <- iri_method
      _ <- properties_method
      _ <- component_class_part()
      _ <- to_record_method
      _ <- to_data_store_method
      _ <- value_convert_methods
    } yield ()

  protected def schema_accessor_method: GenM[Unit] =
    if (is_entity_value) {
      val m = SMethod.query("schema", TypeName.create("org.goldenport.schema", "Schema")) {
        println(s"${clazz.className.name}.schema")
      }
      define_method(m)
    } else {
      unit
    }

  protected def with_methods: GenM[Unit] = {
    val params = clazz.parameterSequence.parameters
    intercalateTraverse_(params, separator)(with_method)
  }

  protected def with_method(p: Parameter): GenM[Unit] = {
    val name = s"with${StringUtils.makeTitle(p.name.name)}"
    val param = p
    val rtype = TypeName.create(clazz)
    val m = SMethod.query(name, rtype, param) {
      val paramname = p.name.name
      val targetname = _class_constructor_parameter_name(paramname)
      println(s"copy($targetname = $paramname)")
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
      val m = SMethod.query("toRecord", TypeName.create("org.goldenport.record", "Record")) {
        for {
          _ <- println("Record.dataAuto(")
          _ <- indent
          _ <- _to_record
          _ <- outdent
          _ <- println(")")
        } yield ()
      }
      define_method(m)
    } else {
      unit
    }

  protected def to_data_store_method: GenM[Unit] =
    if (is_entity_value) {
      val m = SMethod.query("toDataStore", TypeName.create("org.goldenport.record", "Record")) {
        for {
          _ <- println("Record.dataAuto(")
          _ <- indent
          _ <- _to_data_store
          _ <- outdent
          _ <- println(")")
        } yield ()
      }
      define_method(m)
    } else {
      unit
    }

  protected def value_convert_methods: GenM[Unit] =
    if (is_entity_value || is_value) {
      for {
        _ <- println("private def _to_external_value(v: Any): Any = v match {")
        _ <- indent
        _ <- println("case m if java.util.Objects.isNull(m) => null")
        _ <- println("case m: String => m")
        _ <- println("case m: java.lang.Number => m")
        _ <- println("case m: java.lang.Boolean => m")
        _ <- println("case m: java.lang.Character => m.toString")
        _ <- println("case m: Record => m")
        _ <- println("case m: Option[?] => m.map(_to_external_value)")
        _ <- println("case m: Seq[?] => m.map(_to_external_value)")
        _ <- println("case m: Set[?] => m.toVector.map(_to_external_value)")
        _ <- println("case m: Array[?] => m.toVector.map(_to_external_value)")
        _ <- println("""case m: Map[?, ?] => m.iterator.map { case (k, value) => k.toString -> _to_external_value(value) }.toMap""")
        _ <- println("case m: org.goldenport.text.Presentable => m.print")
        _ <- println("case other => other.toString")
        _ <- outdent
        _ <- println("}")
        _ <- println()
        _ <- println("private def _to_data_store_value(v: Any): Any = v match {")
        _ <- println("  case m: org.simplemodeling.model.directive.Update[?] => m")
        _ <- println("  case other => _to_external_value(other)")
        _ <- println("}")
      } yield ()
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
    print("\"", _record_external_key(p), "\" -> _to_external_value(", p.name.name, ")")

  private def _to_data_store: GenM[Unit] =
    FoldTraverseUtil.intercalateTraverseWithEnd_(
      attributes_vector,
      println(", "),
      println()
    )(_to_data_store)

  private def _to_data_store(p: Attribute): GenM[Unit] = {
    val key = p.dbColumnName.getOrElse(StringUtils.camelToUnderscore(p.name.name))
    print("\"", key, "\" -> _to_data_store_value(", p.name.name, ")")
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
      _ <- separator
      _ <- record_reader_methods
      _ <- builder_part
      _ <- component_object_part
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def property_name_definitions: GenM[Unit] = {
    val attrs = clazz.attributeSequence.attributes
    attrs.traverse_(property_name_definition)
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

  protected def property_name_definition(p: Attribute): GenM[Unit] =
    for {
      _ <- println(s"""final val ${property_name(p.name.name)} = "${p.name.name}"""")
      _ <- property_input_keys_definition(p)
    } yield ()

  protected def input_keys_name(p: String): String =
    s"INPUT_KEYS_${StringUtils.camelToUnderscore(p).toUpperCase}"

  protected def property_input_keys_definition(p: Attribute): GenM[Unit] = {
    val xs = _record_input_keys(p).map(x => s""""$x"""").mkString(", ")
    println(s"final val ${input_keys_name(p.name.name)}: List[String] = List($xs).distinct")
  }

  private def _record_input_keys(p: Attribute): Vector[String] = {
    val external = p.externalName.toVector
    val camel = Vector(p.name.name)
    val snake = Vector(StringUtils.camelToUnderscore(p.name.name))
    (external ++ camel ++ snake).filterNot(_.isEmpty).distinct
  }

  private def _record_external_key(p: Attribute): String =
    p.externalName.getOrElse(StringUtils.camelToUnderscore(p.name.name))

  protected def schema: GenM[Unit] =
    if (is_entity_value)
      if (clazz.directive.isPlain)
        schema_canonical
      else
        schema_delegate
    else
      println("// Schema")

  protected def schema_canonical: GenM[Unit] =
    for {
      _ <- println("val schema: org.goldenport.schema.Schema = org.goldenport.schema.Schema(")
      _ <- indent
      _ <- println("columns = Vector(")
      _ <- indent
      _ <- _schema_columns
      _ <- outdent
      _ <- println(")")
      _ <- outdent
      _ <- println(")")
    } yield ()

  protected def schema_delegate: GenM[Unit] =
    println(s"val schema: org.goldenport.schema.Schema = ${_canonical_schema_owner}.schema")

  private def _canonical_schema_owner: String =
    clazz.directive.canonicalSchemaOwner.map(_.fullName).getOrElse(clazz.className.name)

  private def _schema_columns: GenM[Unit] = {
    val n = attributes_vector.length
    attributes_vector.zipWithIndex.traverse_ { case (a, i) =>
      for {
        _ <- _schema_column(a)
        _ <- if (i + 1 < n) println(",") else println()
      } yield ()
    }
  }

  private def _schema_column(p: Attribute): GenM[Unit] =
    for {
      _ <- println("org.goldenport.schema.Column(")
      _ <- indent
      _ <- println(s"""baseContent = org.simplemodeling.model.value.BaseContent.simple("${p.name.name}"),""")
      _ <- println("domain = org.goldenport.schema.ValueDomain(")
      _ <- indent
      _ <- println(s"datatype = ${_schema_datatype_expr(p.typeName)},")
      _ <- println(s"multiplicity = ${_schema_multiplicity_expr(p.typeName)}")
      _ <- outdent
      _ <- println(")")
      _ <- outdent
      _ <- print(")")
    } yield ()

  private def _schema_datatype_expr(p: TypeName): String = {
    val base = _schema_base_type(p)
    base match {
      case m: TypeName.Primitive => _schema_datatype_expr_by_name(m.datatype.name)
      case m: TypeName.Plain => _schema_datatype_expr_by_name(m.name)
      case _ => "org.goldenport.schema.XString"
    }
  }

  private def _schema_datatype_expr_by_name(name: String): String = {
    val key = Option(name).getOrElse("").trim.toLowerCase(java.util.Locale.ROOT)
    val schema = "org.goldenport.schema."
    key match {
      case "string" => s"${schema}XString"
      case "boolean" => s"${schema}XBoolean"
      case "byte" => s"${schema}XInt"
      case "short" => s"${schema}XInt"
      case "int" => s"${schema}XInt"
      case "long" => s"${schema}XLong"
      case "float" => s"${schema}XFloat"
      case "double" => s"${schema}XDouble"
      case "integer" => s"${schema}XInteger"
      case "nonnegativeinteger" => s"${schema}XNonNegativeInteger"
      case "positiveinteger" => s"${schema}XPositiveInteger"
      case "decimal" => s"${schema}XDecimal"
      case "datetime" => s"${schema}XDateTime"
      case "localdatetime" => s"${schema}XLocalDateTime"
      case "yearmonth" => s"${schema}XYearMonth"
      case "age" => s"${schema}XInt"
      case _ => s"${schema}XString"
    }
  }

  private def _schema_base_type(p: TypeName): TypeName = p match {
    case m: TypeName.Container => _schema_base_type(m.containee)
    case m => m
  }

  private def _schema_multiplicity_expr(p: TypeName): String = p match {
    case m: TypeName.Container if m.isOption =>
      "org.goldenport.schema.Multiplicity.ZeroOne"
    case m: TypeName.Container if _is_non_empty_collection(m.container) =>
      "org.goldenport.schema.Multiplicity.OneMore"
    case m: TypeName.Container if _is_collection(m.container) =>
      "org.goldenport.schema.Multiplicity.ZeroMore"
    case _ =>
      "org.goldenport.schema.Multiplicity.One"
  }

  private def _is_collection(p: TypeName): Boolean = p match {
    case m: TypeName.Plain =>
      m.fullName match {
        case "scala.List" => true
        case "scala.collection.immutable.List" => true
        case "scala.Vector" => true
        case "scala.collection.immutable.Vector" => true
        case "scala.Seq" => true
        case "scala.collection.immutable.Seq" => true
        case "scala.Set" => true
        case "scala.collection.immutable.Set" => true
        case "cats.data.NonEmptyVector" => true
        case _ => false
      }
    case _ => false
  }

  private def _is_non_empty_collection(p: TypeName): Boolean = p match {
    case m: TypeName.Plain =>
      m.fullName match {
        case "cats.data.NonEmptyVector" => true
        case _ => false
      }
    case _ => false
  }

  protected def record_reader_methods: GenM[Unit] =
    for {
      _ <- println("private def _record_get_as_c[A](")
      _ <- indent
      _ <- println("record: Record,")
      _ <- println("keys: List[String]")
      _ <- outdent
      _ <- println(")(using vr: org.goldenport.convert.ValueReader[A]): Consequence[Option[A]] = {")
      _ <- indent
      _ <- println("keys.foldLeft(Consequence.success(Option.empty[A])) { (z, key) =>")
      _ <- indent
      _ <- println("z.flatMap {")
      _ <- indent
      _ <- println("case s @ Some(_) => Consequence.success(s)")
      _ <- println("case None => record.getAsC[A](key)")
      _ <- outdent
      _ <- println("}")
      _ <- outdent
      _ <- println("}")
      _ <- outdent
      _ <- println("}")
    } yield ()

  protected def builder_part: GenM[Unit] =
    if (is_value)
      for {
        _ <- separator
        _ <- builder
        _ <- separator
        _ <- createc_method_required
        _ <- separator
        _ <- createc_method_required_with_execution_context
        _ <- separator
        _ <- create_method
        _ <- separator
        _ <- create_method_with_execution_context
        _ <- separator
        _ <- create_recordc_method
        _ <- separator
        _ <- create_recordc_method_with_execution_context
        _ <- separator
        _ <- create_record_method
        _ <- separator
        _ <- create_record_method_with_execution_context
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
      _ <- builder_buildc_with_execution_context_method
      _ <- separator
      _ <- builder_build_method
      _ <- separator
      _ <- builder_build_recordc_method
      _ <- separator
      _ <- builder_build_recordc_with_execution_context_method
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
    ParameterSequence(p.attributes.map(to_builder_parameter) :+ Parameter(ParameterName("_failures"), TypeName.failureVector, true, true))

  protected def to_builder_parameter(p: Attribute): Parameter =
    Parameter(ParameterName(p.name.name), to_optionable_type(p.typeName), true, true)

  protected def to_optionable_type(p: TypeName): TypeName =
    p match {
      case m: TypeName.Primitive => TypeName.option(m)
      case m: TypeName.Plain => TypeName.option(m)
      case m: TypeName.Container if is_condition_type(m) => TypeName.option(m)
      case m: TypeName.Container if is_update_type(m) => TypeName.option(m)
      case m: TypeName.Container => m
      case m: TypeName.Function => RAISE.notImplementedYetDefect("Function")
      case m: TypeName.Unit => RAISE.notImplementedYetDefect("Unit")
    }

  protected final def is_condition_type(p: TypeName): Boolean = p match {
    case m: TypeName.Container =>
      m.container.name == "Condition" ||
      m.container.fullName == "org.simplemodeling.model.directive.Condition"
    case _ => false
  }

  protected final def is_option_condition_type(p: TypeName): Boolean = p match {
    case m: TypeName.Container if m.isOption => is_condition_type(m.containee)
    case _ => false
  }

  protected final def is_update_type(p: TypeName): Boolean = p match {
    case m: TypeName.Container =>
      m.container.name == "Update" ||
      m.container.fullName == "org.simplemodeling.model.directive.Update"
    case _ => false
  }

  protected final def is_option_update_type(p: TypeName): Boolean = p match {
    case m: TypeName.Container if m.isOption => is_update_type(m.containee)
    case _ => false
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
    val m = SMethod.query(name, rtype, params)(body)
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
    val m = SMethod.query(s"${name}", rtype, params) {
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
    val m = SMethod.query(s"${name}", rtype, params) {
      println(s"""${name}C(${params.map(_.name.name).mkString(" ,")}).take""")
    }
    define_method(m)
  }

  protected def define_method_unsafe_consequence_take(
    name: String,
    rtype: TypeName,
    params: Seq[Parameter]
  ): GenM[Unit] = {
    val m = SMethod.query(s"${name}U", rtype, params) {
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

  protected def consequence_type(p: TypeName): TypeName = TypeName.consequence(p)

  protected val builder_type = TypeName(clazz.packageName, s"${clazz.className.name}.Builder")

  protected val consequence_builder_type = consequence_type(builder_type)

  protected final def option_type(p: TypeName): TypeName = scala_context.optionType(p)

  protected final def string_type: TypeName = scala_context.stringType

  protected final def raw_parameter(p: Parameter): Parameter = {
    p.typeName match {
      case TypeName.Container(_, containee) => p.copy(typeName = containee)
      case _ => p
    }
  }

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

    if (is_condition_type(p.typeName)) {
      for {
        _ <- define_method(methodname, builder_type, raw_parameter(p)) {
          println("copy(", propname, " = Some(Condition.is(", propname, ")))")
        }
        _ <- define_method(methodname, builder_type, p) {
          println("copy(", propname, " = Some(", propname, "))")
        }
      } yield ()
    } else if (is_update_type(p.typeName)) {
      for {
        _ <- define_method(methodname, builder_type, raw_parameter(p)) {
          println("copy(", propname, " = Some(Update.set(", propname, ")))")
        }
        _ <- define_method(methodname, builder_type, p) {
          println("copy(", propname, " = Some(", propname, "))")
        }
      } yield ()
    } else {
      for {
        _ <- define_method(methodname, builder_type, raw_parameter(p)) {
          println("copy(", propname, " = Some(", propname, "))")
        }
        _ <- define_method(methodname, builder_type, option_parameter(p)) {
          println("copy(", propname, " = ", propname, ")")
        }
        _ <- _builder_with_methods_parse(p)
      } yield ()
    }
  }

  private def _builder_with_methods_parse(p: Parameter): GenM[Unit] =
    if (
      p.typeName.isString ||
      _is_simple_object_attribute_type(p.typeName) ||
      is_condition_type(p.typeName) ||
      is_option_condition_type(p.typeName) ||
      is_update_type(p.typeName) ||
      is_option_update_type(p.typeName)
    ) {
      unit
    } else {
      for {
        _ <- _builder_with_method_parse(p.name.name, p.typeName, string_parameter(p))
        _ <- _builder_with_methods_parse_number(p)
      } yield ()
    }

  private val _simple_object_attribute_type_names: Set[String] = Set(
    "NameAttributes",
    "DescriptiveAttributes",
    "LifecycleAttributes",
    "PublicationAttributes",
    "SecurityAttributes",
    "ResourceAttributes",
    "AuditAttributes",
    "MediaAttributes",
    "ContextualAttributes",
    "NameAttributesUpdate",
    "DescriptiveAttributesUpdate",
    "LifecycleAttributesUpdate",
    "PublicationAttributesUpdate",
    "SecurityAttributesUpdate",
    "ResourceAttributesUpdate",
    "AuditAttributesUpdate",
    "MediaAttributesUpdate",
    "ContextualAttributesUpdate",
    "NameAttributesQuery",
    "DescriptiveAttributesQuery",
    "LifecycleAttributesQuery",
    "PublicationAttributesQuery",
    "SecurityAttributesQuery",
    "ResourceAttributesQuery",
    "AuditAttributesQuery",
    "MediaAttributesQuery",
    "ContextualAttributesQuery"
  )

  private def _is_simple_object_attribute_type(p: TypeName): Boolean =
    p.contentType match {
      case TypeName.Plain(pkg, name, _) =>
        pkg.name == "org.simplemodeling.model.value" && _simple_object_attribute_type_names.contains(name)
      case _ =>
        false
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

  private def _builder_with_method_parse_if_required(name: String, proptype: TypeName, param: Parameter): GenM[Unit] =
    if (proptype.contentType == param.typeName)
      unit
    else
      _builder_with_method_parse(name, proptype, param)

  private def _builder_with_method_parse(name: String, proptype: TypeName, param: Parameter): GenM[Unit] = 
    proptype match {
      case m: TypeName.Primitive =>
        _builder_with_method_parse_primitive(name, m, param)
      case m => _builder_with_method_parse_plain(name, m, param)
    }

  private def _builder_with_method_parse_primitive(name: String, proptype: TypeName.Primitive, param: Parameter): GenM[Unit] =
    if (param.typeName.isPrimitive)
      _builder_with_method_parse_primitive_primitive(name, proptype, param)
    else
      _builder_with_method_parse_primitive_plain(name, proptype, param)

  private def _builder_with_method_parse_primitive_primitive(name: String, proptype: TypeName.Primitive, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val varname = name
    val propname = name
    val primitivename = proptype.name
    define_cmethods(methodname, builder_type, param) {
      println("Consequence.to", primitivename, "(", varname, ").map(x => copy(", propname, " = Some(x)))")
    }
  }

  private def _builder_with_method_parse_primitive_plain(name: String, proptype: TypeName.Primitive, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val varname = name
    val propname = name
    val primitivename = proptype.name
    define_cmethods(methodname, builder_type, param) {
      println(varname, ".to" + primitivename, ".map(x => copy(", propname, " = Some(x)))")
    }
  }

  private def _builder_with_method_parse_plain(name: String, proptype: TypeName, param: Parameter): GenM[Unit] = {
    proptype match {
      case m: TypeName.Container =>
        if (m.isOption)
          _builder_with_method_parse_plain_option(name, m.containee, param)
        else
          println("???")
      case m => _builder_with_method_parse_plain_nooption(name, proptype, param)
    }
  }

  private def _builder_with_method_parse_plain_nooption(name: String, proptype: TypeName, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val propname = name
    // define_cmethods(methodname, builder_type, param) {
    //   println(proptype.name, ".parse(", varname, ").map(x => copy(", propname, " = Some(x)))")
    // }
    _builder_with_methods_parse_plain_nooption(methodname, propname, proptype, param)
  }

  private def _builder_with_method_parse_plain_option(name: String, proptype: TypeName, param: Parameter): GenM[Unit] = {
    val methodname = with_method_name(name)
    val propname = name
    // define_cmethods(methodname, builder_type, param) {
    //   println(proptype.name, ".parse(", varname, ").map(x => copy(", propname, " = Some(x)))")
    // }
    _builder_with_methods_parse_plain_option(methodname, propname, proptype, param)
  }

  // private def _builder_define_with_methods() = {
  //   for {
  //     _ <- _define_method_within_failure()
  //   } yield ()
  // }

  private def _builder_with_methods_parse_plain_nooption(
    methodname: String,
    propname: String,
    proptype: TypeName,
    param: Parameter
  ) = {
    for {
      _ <- _builder_with_method_parse_plain_nooption_nooption(methodname, propname, proptype, param)
      _ <- _builder_with_method_parse_plain_nooption_option(methodname, propname, param)
    } yield ()
  }

  private def _builder_with_methods_parse_plain_option(
    methodname: String,
    propname: String,
    proptype: TypeName,
    param: Parameter
  ) = {
    for {
      _ <- _builder_with_method_parse_plain_option_nooption(methodname, propname, proptype, param)
      _ <- _builder_with_method_parse_plain_option_option(methodname, propname, param)
    } yield ()
  }

  private def _builder_with_method_parse_plain_nooption_nooption(
    methodname: String,
    propname: String,
    proptype: TypeName,
    param: Parameter
  ) = {
    // define_method(methodname, builder_type, param) {
    //   block(s"${proptype.name}.parse(${propname}) match") {
    //     for {
    //       _ <- println(s"case Consequence.Success(s) => copy(${propname} = s)")
    //       _ <- println(s"case m: Consequence.Failure[_] => copy(_failures = _failures :+ m)")
    //     } yield ()
    //   }
    // }
    _builder_with_method_parse_plain_option_nooption(methodname, propname, proptype, param)
  }

  private def _builder_with_method_parse_plain_nooption_option(
    methodname: String,
    propname: String,
    param: Parameter
  ) = {
    // define_method(methodname, builder_type, param) {
    //   block(s"${propname} match") {
    //     for {
    //       _ <- println(s"case Some(s) => ${methodname}(s)")
    //       _ <- println(s"case None => this")
    //     } yield ()
    //   }
    // }
    _builder_with_method_parse_plain_option_option(methodname, propname, param)
  }

  private def _builder_with_method_parse_plain_option_nooption(
    methodname: String,
    propname: String,
    proptype: TypeName,
    param: Parameter
  ) = {
    define_method(methodname, builder_type, param.toRawType) {
      block(s"${proptype.name}.parse(${propname}) match") {
        for {
          _ <- println(s"case Consequence.Success(s) => copy(${propname} = Some(s))")
          _ <- println(s"case m: Consequence.Failure[_] => copy(_failures = _failures :+ m)")
        } yield ()
      }
    }
  }

  private def _builder_with_method_parse_plain_option_option(
    methodname: String,
    propname: String,
    param: Parameter
  ) = {
    define_method(s"${methodname}Option", builder_type, param.toOptionType) {
      block(s"${propname} match") {
        for {
          _ <- println(s"case Some(s) => ${methodname}(s)")
          _ <- println(s"case None => this")
        } yield ()
      }
    }
  }

  protected final def with_method_name(name: String): String =
    s"with${StringUtils.makeTitle(name)}"

  protected def builder_buildc_method: GenM[Unit] = {
    val m = SMethod.query("buildC", TypeName.consequence(clazz)) {
      _builder_buildc_method_body(builder_parameters)
    }
    define_method(m)
  }

  protected def builder_buildc_with_execution_context_method: GenM[Unit] =
    if (clazz.directive.isCreate)
      for {
        _ <- print("def buildCWithExecutionContext(using ctx: org.goldenport.cncf.context.ExecutionContext): ")
        _ <- print(TypeName.consequence(clazz).name)
        _ <- println(" = {")
        _ <- indent
        _ <- _builder_buildc_method_body(builder_parameters_with_execution_context)
        _ <- outdent
        _ <- println("}")
      } yield ()
    else
      unit

  private def _builder_buildc_method_body(body: => GenM[Unit]): GenM[Unit] = {
    val arity = clazz.parameterSequence.parameters.size
    for {
      _ <- if (arity == 0) {
        println(s"Consequence.success(${clazz.className.name}.apply())")
      } else {
        for {
          _ <- println("(")
          _ <- indent
          _ <- body
          _ <- outdent
          _ <- if (arity == 1)
            print(").map(")
          else
            print(").mapN(")
          _ <- print(clazz.className.name)
          _ <- print(".apply")
          _ <- println(")")
        } yield ()
      }
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

  protected def builder_parameters_with_execution_context: GenM[Unit] = {
    val a = clazz.parameterSequence
    a.parameters.lastOption match {
      case Some(s) => for {
        _ <- a.parameters.init.traverse_(builder_parameter_with_execution_context)
        _ <- builder_parameter_with_execution_context_last(s)
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

  protected def builder_parameter_with_execution_context(p: Parameter): GenM[Unit] =
    for {
      _ <- print(builder_parameter_line_with_execution_context(p))
      _ <- println(",")
    } yield ()

  protected def builder_parameter_with_execution_context_last(p: Parameter): GenM[Unit] =
    println(builder_parameter_line_with_execution_context(p))

  protected def builder_parameter_line(p: Parameter): String =
    p.typeName match {
      case m: TypeName.Container => builder_parameter_line_container(p, m)
      case m => builder_parameter_line_raw(p)
    }

  protected def builder_parameter_line_with_execution_context(p: Parameter): String =
    p.typeName match {
      case m: TypeName.Container => builder_parameter_line_container_with_execution_context(p, m)
      case _ => builder_parameter_line_raw(p)
    }

  protected def builder_parameter_line_raw(p: Parameter): String =
    builder_parameter_line_raw(p.name.name)

  protected def builder_parameter_line_raw(p: String): String =
    _builder_default_expression_raw(p) match {
      case Some(expr) =>
        s"Consequence.success($p.getOrElse($expr))"
      case None =>
        s"Consequence.successOrPropertyNotFound(${property_name(p)}, $p)"
    }

  protected def builder_parameter_line_container(
    p: Parameter,
    container: TypeName.Container
  ): String =
    if (container.isOption)
      builder_parameter_line_container_option(p)
    else if (is_update_type(container))
      s"Consequence.success(${p.name.name}.getOrElse(Update.noop[${container.containee.name}]))"
    else if (is_condition_type(container))
      builder_parameter_line_raw(p.name.name)
    else if (container.isList)
      s"???"
    else if (container.isVector)
      s"???"
    else if (container.isSet)
      s"???"
    else
      RAISE.noReachDefect

  protected def builder_parameter_line_container_with_execution_context(
    p: Parameter,
    container: TypeName.Container
  ): String =
    if (container.isOption)
      _context_default_expression(p) match {
        case Some(expr) => s"Consequence.success(${p.name.name}.orElse($expr))"
        case None => builder_parameter_line_container_option(p)
      }
    else
      builder_parameter_line_container(p, container)

  protected def builder_parameter_line_container_option(
    p: Parameter
  ): String = s"Consequence.success(${p.name})"

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
    val m = SMethod.query("buildC", TypeName.consequence(clazz), Parameter.record) {
      _builder_buildc_method_body(_builder_record_parameters)
    }
    define_method(m)
  }

  protected def builder_build_recordc_with_execution_context_method: GenM[Unit] =
    if (clazz.directive.isCreate)
      for {
        _ <- print("def buildCWithExecutionContext(record: Record)(using ctx: org.goldenport.cncf.context.ExecutionContext): ")
        _ <- print(TypeName.consequence(clazz).name)
        _ <- println(" = {")
        _ <- indent
        _ <- _builder_buildc_method_body(_builder_record_parameters_with_execution_context)
        _ <- outdent
        _ <- println("}")
      } yield ()
    else
      unit

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

  private def _builder_record_parameters_with_execution_context: GenM[Unit] = {
    val params = clazz.parameterSequence.parameters
    for {
      _ <- FoldTraverseUtil.intercalateTraverseWithEnd_(
        params,
        println(", "),
        println()
      )(_build_record_param_with_execution_context)
    } yield ()
  }

  private def _build_record_param(p: Parameter): GenM[Unit] =
    _build_param_or_var(p)(_get_record_param(p))

  private def _build_record_param_with_execution_context(p: Parameter): GenM[Unit] =
    p.typeName match {
      case m: TypeName.Container if m.isOption =>
        _context_default_expression(p) match {
          case Some(expr) =>
            for {
              _ <- print("_record_get_as_c[", p.toRawType.typeName.name, "](record, ", input_keys_name(p.name.name), ")")
              _ <- print(".map(_  orElse ", p.name.name, ".orElse(", expr, "))")
            } yield ()
          case None =>
            _build_record_param(p)
        }
      case _ =>
        _build_record_param(p)
    }

  private def _build_param_or_var(p: Parameter)(param: => GenM[Unit]): GenM[Unit] =
    p.typeName match {
      case m: TypeName.Container => _build_param_or_var_container(p, m)(param)
      case _ => _build_param_or_var_raw(p)(param)
    }

  private def _build_param_or_var_raw(p: Parameter)(param: => GenM[Unit]): GenM[Unit] =
    if (_is_name_attributes_raw_parameter(p))
      _build_param_or_var_name_attributes(p)
    else if (_is_simple_object_attribute_parameter(p))
      _build_param_or_var_simple_object_attribute(p)
    else
      for {
        _ <- print("_record_get_as_c[", p.toRawType.typeName.name, "](record, ", input_keys_name(p.name.name), ").flatMap {")
        _ <- println()
        _ <- indent
        _ <- println("case Some(s) => Consequence.success(s)")
        _ <- _build_param_or_var_raw_default(p)
        _ <- outdent
        _ <- print("}")
      } yield ()

  private def _build_param_or_var_raw_default(p: Parameter): GenM[Unit] =
    _builder_default_expression_raw(p.name.name) match {
      case Some(expr) =>
        println("case None => Consequence.success(", p.name.name, ".getOrElse(", expr, "))")
      case None =>
        println("case None => Consequence.successOrPropertyNotFound(", property_name(p.name.name), ", ", p.name.name, ")")
    }

  private def _is_name_attributes_raw_parameter(p: Parameter): Boolean =
    p.name.name == "name_Attributes" && p.toRawType.typeName.name == "NameAttributes"

  private def _is_simple_object_attribute_parameter(p: Parameter): Boolean =
    _is_simple_object_attribute_type(p.toRawType.typeName)

  private def _build_param_or_var_simple_object_attribute(p: Parameter): GenM[Unit] =
    _builder_default_expression_raw(p.name.name) match {
      case Some(expr) =>
        print("Consequence.success(", p.name.name, ".getOrElse(", expr, "))")
      case None =>
        print("Consequence.successOrPropertyNotFound(", property_name(p.name.name), ", ", p.name.name, ")")
    }

  private def _build_param_or_var_name_attributes(p: Parameter): GenM[Unit] =
    for {
      _ <- println("(")
      _ <- indent
      _ <- println("_record_get_as_c[Name](record, List(\"name\")),")
      _ <- println("_record_get_as_c[String](record, List(\"title\"))")
      _ <- outdent
      _ <- println(").mapN { (namev, titlev) =>")
      _ <- indent
      _ <- println("val base = ", p.name.name, ".getOrElse(namev.map(NameAttributes.simple).getOrElse(NameAttributes.simple(Name(\"unknown\"))))")
      _ <- println("titlev.fold(base)(t => base.copy(title = Some(I18nTitle(t))))")
      _ <- outdent
      _ <- println("}")
    } yield ()

  private def _build_param_or_var_container(
    p: Parameter,
    container: TypeName.Container
  )(param: => GenM[Unit]): GenM[Unit] =
    if (container.isOption)
      for {
        _ <- param
        _ <- print(".map(_  orElse ", p.name.name, ")")
      } yield ()
    else if (is_condition_type(container))
      _build_param_or_var_condition(p, container)
    else if (is_update_type(container))
      _build_param_or_var_update(p, container)
    else if (container.isList)
      println("???")
    else if (container.isVector)
      println("???")
    else if (container.isSet)
      println("???")
    else
      RAISE.noReachDefect

  private def _build_param_or_var_condition(
    p: Parameter,
    container: TypeName.Container
  ): GenM[Unit] = {
    val propname = property_name(p.name.name)
    val keyname = input_keys_name(p.name.name)
    for {
      _ <- print("_record_get_as_c[", container.containee.name, "](record, ", keyname, ").flatMap {")
      _ <- println()
      _ <- indent
      _ <- println("case Some(s) => Consequence.success(Condition.is(s))")
      _ <- println("case None => Consequence.successOrPropertyNotFound(", propname, ", ", p.name.name, ")")
      _ <- outdent
      _ <- print("}")
    } yield ()
  }

  private def _build_param_or_var_update(
    p: Parameter,
    container: TypeName.Container
  ): GenM[Unit] = {
    val keyname = input_keys_name(p.name.name)
    for {
      _ <- print("_record_get_as_c[", container.containee.name, "](record, ", keyname, ").flatMap {")
      _ <- println()
      _ <- indent
      _ <- println("case Some(s) => Consequence.success(Update.set(s))")
      _ <- println("case None => Consequence.success(", p.name.name, ".getOrElse(Update.noop[", container.containee.name, "]))")
      _ <- outdent
      _ <- print("}")
    } yield ()
  }

  private def _get_record_param(p: Parameter): GenM[Unit] =
    print("_record_get_as_c[", p.toRawType.typeName.name, "](record, ", input_keys_name(p.name.name), ")")

  private def _context_default_expression(p: Parameter): Option[String] = {
    val key = p.name.name.toLowerCase(java.util.Locale.ROOT)
    key match {
      case "name" =>
        Some("Some(Name(ctx.security.principal.id.value))")
      case "createdat" =>
        Some("Some(java.time.ZonedDateTime.now(ctx.clock.withZone(ctx.timezone)))")
      case "updatedat" =>
        Some("Some(java.time.ZonedDateTime.now(ctx.clock.withZone(ctx.timezone)))")
      case "createdby" =>
        Some("Some(Identifier(ctx.security.principal.id.value))")
      case "updatedby" =>
        Some("Some(Identifier(ctx.security.principal.id.value))")
      case "poststatus" =>
        Some("Some(org.simplemodeling.model.statemachine.PostStatus.default)")
      case "aliveness" =>
        Some("Some(org.simplemodeling.model.statemachine.Aliveness.default)")
      case "traceid" =>
        Some("Some(ctx.observability.traceId.value)")
      case "correlationid" =>
        Some("ctx.observability.correlationId.map(_.value)")
      case _ =>
        None
    }
  }

  private def _builder_default_expression_raw(name: String): Option[String] = {
    if (clazz.directive.isUpdate) {
      name match {
        case "name_Attributes" => Some("org.simplemodeling.model.value.NameAttributesUpdate()")
        case "descriptive_Attributes" => Some("org.simplemodeling.model.value.DescriptiveAttributesUpdate()")
        case "lifecycle_Attributes" => Some("org.simplemodeling.model.value.LifecycleAttributesUpdate()")
        case "publication_Attributes" => Some("org.simplemodeling.model.value.PublicationAttributesUpdate()")
        case "security_Attributes" => Some("org.simplemodeling.model.value.SecurityAttributesUpdate()")
        case "resource_Attributes" => Some("org.simplemodeling.model.value.ResourceAttributesUpdate()")
        case "audit_Attributes" => Some("org.simplemodeling.model.value.AuditAttributesUpdate()")
        case "media_Attributes" => Some("org.simplemodeling.model.value.MediaAttributesUpdate()")
        case "contextual_Attribute" => Some("org.simplemodeling.model.value.ContextualAttributesUpdate()")
        case _ => None
      }
    } else {
    name match {
      case "name_Attributes" =>
        Some("org.simplemodeling.model.value.NameAttributes.simple(Name(\"unknown\"))")
      case "descriptive_Attributes" =>
        Some("org.simplemodeling.model.value.DescriptiveAttributes.empty")
      case "lifecycle_Attributes" =>
        Some("org.simplemodeling.model.value.LifecycleAttributes(java.time.ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, java.time.ZoneOffset.UTC), None, Identifier(\"system\"), None, org.simplemodeling.model.statemachine.PostStatus.default, org.simplemodeling.model.statemachine.Aliveness.default)")
      case "publication_Attributes" =>
        Some("org.simplemodeling.model.value.PublicationAttributes(None, None, None, None, None)")
      case "security_Attributes" =>
        Some("org.simplemodeling.model.value.SecurityAttributes(org.goldenport.datatype.ObjectId(Identifier(\"system\")), org.goldenport.datatype.ObjectId(Identifier(\"system\")), org.simplemodeling.model.value.SecurityAttributes.Rights(org.simplemodeling.model.value.SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true), org.simplemodeling.model.value.SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false), org.simplemodeling.model.value.SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)), org.goldenport.datatype.ObjectId(Identifier(\"system\")))")
      case "resource_Attributes" =>
        Some("org.simplemodeling.model.value.ResourceAttributes()")
      case "audit_Attributes" =>
        Some("org.simplemodeling.model.value.AuditAttributes()")
      case "media_Attributes" =>
        Some("org.simplemodeling.model.value.MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty)")
      case "contextual_Attribute" =>
        Some("org.simplemodeling.model.value.ContextualAttributes()")
      case _ =>
        None
    }
    }
  }

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

  protected def createc_method_required_with_execution_context: GenM[Unit] =
    if (clazz.directive.isCreate)
      createc_method_with_execution_context(clazz.parameterSequence.requiredPatameters)
    else
      unit

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

  protected def createc_method_with_execution_context(params: Seq[Parameter]): GenM[Unit] = {
    for {
      _ <- print("def createWithExecutionContextC")
      _ <- parameter_list(params)
      _ <- println(s"(using ctx: org.goldenport.cncf.context.ExecutionContext): Consequence[${clazz.className.name}] = {")
      _ <- indent
      _ <- builder_build_with_execution_context(params)
      _ <- outdent
      _ <- println("}")
    } yield ()
  }

  protected def builder_build(params: Seq[Parameter]): GenM[Unit] = {
    def _with_(p: Parameter) = s".with${StringUtils.makeTitle(p.name.name)}(${p.name})"
    val withs = params.map(_with_).mkString
    for {
      _ <- println("val builder = Builder()")
      _ <- if (withs.nonEmpty)
        for {
          _ <- println(s"val builder2 = builder$withs")
          _ <- println("builder2.buildC()")
        } yield ()
      else
        println("builder.buildC()")
    } yield ()
  }

  protected def builder_build_with_execution_context(params: Seq[Parameter]): GenM[Unit] = {
    def _with_(p: Parameter) = s".with${StringUtils.makeTitle(p.name.name)}(${p.name})"
    val withs = params.map(_with_).mkString
    for {
      _ <- println("val builder = Builder()")
      _ <- if (withs.nonEmpty)
        for {
          _ <- println(s"val builder2 = builder$withs")
          _ <- println("builder2.buildCWithExecutionContext")
        } yield ()
      else
        println("builder.buildCWithExecutionContext")
    } yield ()
  }

  protected def create_record_method: GenM[Unit] = {
    val param = Parameter.record
    define_method_consequence_take("create", clazz, param)
  }

  protected def create_method_with_execution_context: GenM[Unit] =
    if (clazz.directive.isCreate)
      for {
        _ <- print("def createWithExecutionContext(")
        _ <- required_parameter_list(clazz.parameterSequence)
        _ <- println(s")(using ctx: org.goldenport.cncf.context.ExecutionContext): ${clazz.className.name} = {")
        _ <- indent
        _ <- print("createWithExecutionContextC(")
        _ <- required_invoke_parameters(clazz.parameterSequence)
        _ <- println(").take")
        _ <- outdent
        _ <- println("}")
      } yield ()
    else
      unit

  protected def create_recordc_method: GenM[Unit] = {
    val m = SMethod.query("createC", TypeName.consequence(clazz), Parameter.record) {
      for {
        _ <- println("val builder = Builder()")
        _ <- println("builder.buildC(record)")
      } yield ()
    }
    define_method(m)
  }

  protected def create_recordc_method_with_execution_context: GenM[Unit] =
    if (clazz.directive.isCreate)
      for {
        _ <- println(s"def createWithExecutionContextC(record: Record)(using ctx: org.goldenport.cncf.context.ExecutionContext): Consequence[${clazz.className.name}] = {")
        _ <- indent
        _ <- println("val builder = Builder()")
        _ <- println("builder.buildCWithExecutionContext(record)")
        _ <- outdent
        _ <- println("}")
      } yield ()
    else
      unit

  protected def create_record_method_with_execution_context: GenM[Unit] =
    if (clazz.directive.isCreate)
      for {
        _ <- println(s"def createWithExecutionContext(record: Record)(using ctx: org.goldenport.cncf.context.ExecutionContext): ${clazz.className.name} = {")
        _ <- indent
        _ <- println("createWithExecutionContextC(record).take")
        _ <- outdent
        _ <- println("}")
      } yield ()
    else
      unit

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
      _ <- _entity(name)
    } yield ()
  }

  private def _can_equal(name: String): GenM[Unit] =
    if (is_value)
      println(s"given CanEqual[", name, ",", name, "] = CanEqual.derived")
    else
      unit

  private def _eq(name: String): GenM[Unit] =
    if (is_value) {
      for {
        _ <- println("// Domain semantic equality = equality of identity")
        _ <- if (clazz.directive.isQuery || clazz.directive.isUpdate)
          println(s"given Eq[", name, "] = Eq.fromUniversalEquals")
        else if (_has_id_parameter)
          println(s"given Eq[", name, "] = Eq.by(_.id)")
        else
          println(s"given Eq[", name, "] = Eq.fromUniversalEquals")
      } yield ()
    } else {
      unit
    }

  private def _has_id_parameter: Boolean =
    clazz.parameterSequence.parameters.exists(_.name.name == "id")

  private def _entity(name: String): GenM[Unit] =
    if (is_query) {
      for {
        _ <- println(s"""val collectionId: EntityCollectionId = EntityCollectionId("major", "minor", "${StringUtils.camelToUnderscore(name)}")""") // TODO major, minor
        _ <- println(s"given EntityPersistentQuery[$name] = EntityPersistentQuery.derived(createC, collectionId)")
      } yield ()
    } else if (is_update) {
      for {
        _ <- println(s"""val collectionId: EntityCollectionId = EntityCollectionId("major", "minor", "${StringUtils.camelToUnderscore(name)}")""") // TODO major, minor
        _ <- println(s"given EntityPersistentUpdate[$name] with")
        _ <- println(s"  def toRecord(e: $name): Record = e.toDataStore()")
        _ <- println(s"  def fromRecord(r: Record): Consequence[$name] = createC(r)")
        _ <- println(s"  def collection(e: $name): EntityCollectionId = collectionId")
      } yield ()
    } else if (is_entity_value_create) {
      for {
        _ <- println(s"""val collectionId: EntityCollectionId = EntityCollectionId("major", "minor", "${StringUtils.camelToUnderscore(name)}")""") // TODO major, minor
        _ <- println(s"given EntityPersistentCreate[$name] with")
        _ <- println(s"  def id(e: $name): Option[EntityId] = e.id")
        _ <- println(s"  def collection(e: $name): EntityCollectionId = collectionId")
        _ <- println(s"  def toRecord(e: $name): Record = e.toDataStore()")
      } yield ()
    } else if (is_entity_value) {
      for {
        _ <- println(s"""val collectionId: EntityCollectionId = EntityCollectionId("major", "minor", "${StringUtils.camelToUnderscore(name)}")""") // TODO major, minor
        _ <- println(s"given EntityPersistent[$name] with")
        _ <- println(s"  def id(e: $name): EntityId = e.id")
        _ <- println(s"  def toRecord(e: $name): Record = e.toDataStore()")
        _ <- println(s"  def fromRecord(r: Record): Consequence[$name] = createC(r)")
      } yield ()
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
    s"${p.name.name}: ${p.typeName.name}${_parameter_default_suffix(p)}"
  }

  protected def class_parameter_list(p: ParameterSequence): GenM[Unit] =
    class_parameter_list(p.parameters)

  protected def class_parameter_list(ps: Seq[Parameter]): GenM[Unit] = {
    val s = ps.map(class_parameter_item).mkString("(", ", ", ")")
    print(s)
  }

  protected def class_parameter_item(p: Parameter): String = {
    val n = p.name.name
    val constructorname = _class_constructor_parameter_name(n)
    val prefix =
      if (_is_override_parameter_for_parent(n) && constructorname == n)
        "override val "
      else
        ""
    s"$prefix$constructorname: ${p.typeName.name}${_parameter_default_suffix(p)}"
  }

  private def _is_override_parameter_for_parent(name: String): Boolean =
    clazz.parentClass.exists {
      case TypeName.Plain(pkg, "SimpleEntity", _) if pkg.name == "org.simplemodeling.model" =>
        _simple_entity_override_keys.contains(name)
      case TypeName.Plain(pkg, "SimpleEntityCreate", _) if pkg.name == "org.simplemodeling.model" =>
        _simple_entity_create_override_keys.contains(name)
      case TypeName.Plain(pkg, "SimpleEntityUpdate", _) if pkg.name == "org.simplemodeling.model" =>
        _simple_entity_update_override_keys.contains(name)
      case TypeName.Plain(pkg, "SimpleEntityQuery", _) if pkg.name == "org.simplemodeling.model" =>
        _simple_entity_query_override_keys.contains(name)
      case _ =>
        false
    }

  private val _simple_object_attribute_keys: Set[String] = Set(
    "name_Attributes",
    "descriptive_Attributes",
    "lifecycle_Attributes",
    "publication_Attributes",
    "security_Attributes",
    "resource_Attributes",
    "audit_Attributes",
    "media_Attributes",
    "contextual_Attribute"
  )

  private val _simple_entity_override_keys: Set[String] =
    _simple_object_attribute_keys ++ Set("id", "name", "title")

  private val _simple_entity_create_override_keys: Set[String] =
    _simple_object_attribute_keys + "id"

  private val _simple_entity_update_override_keys: Set[String] =
    _simple_object_attribute_keys + "id"

  private val _simple_entity_query_override_keys: Set[String] =
    _simple_object_attribute_keys + "id"

  private val _simple_object_attribute_constructor_names: Map[String, String] = Map(
    "name_Attributes" -> "nameAttributes",
    "descriptive_Attributes" -> "descriptiveAttributes",
    "lifecycle_Attributes" -> "lifecycleAttributes",
    "publication_Attributes" -> "publicationAttributes",
    "security_Attributes" -> "securityAttributes",
    "resource_Attributes" -> "resourceAttributes",
    "audit_Attributes" -> "auditAttributes",
    "media_Attributes" -> "mediaAttributes",
    "contextual_Attribute" -> "contextualAttribute"
  )

  private def _class_constructor_parameter_name(name: String): String =
    _simple_object_attribute_constructor_names.getOrElse(name, name)

  private def _simple_object_attribute_alias_definitions: GenM[Unit] = {
    val aliases = clazz.parameterSequence.parameters.collect {
      case p if _simple_object_attribute_keys.contains(p.name.name) =>
        (p.name.name, p.typeName.name, _class_constructor_parameter_name(p.name.name))
    }.filter { case (original, _, constructorname) => original != constructorname }
    if (aliases.isEmpty)
      unit
    else
      for {
        _ <- aliases.traverse_ {
          case (original, typename, constructorname) =>
            _simple_object_attribute_alias_definition(original, typename, constructorname)
        }
        _ <- separator
      } yield ()
  }

  private def _simple_object_attribute_alias_definition(
    original: String,
    typename: String,
    constructorname: String
  ): GenM[Unit] = {
    val access =
      clazz.parentClass match {
        case Some(TypeName.Plain(pkg, "SimpleEntity", _)) if pkg.name == "org.simplemodeling.model" =>
          "protected "
        case _ =>
          ""
      }
    println(s"override ${access}def $original: $typename = $constructorname")
  }

  private def _parameter_default_suffix(p: Parameter): String =
    if (p.isDefault) {
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
