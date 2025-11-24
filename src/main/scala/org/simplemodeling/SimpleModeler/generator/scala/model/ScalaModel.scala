package org.simplemodeling.SimpleModeler.generator.scala.model

import scalaz._, Scalaz._
import org.goldenport.context.Showable
import org.goldenport.context.Consequence
import org.goldenport.datatype
import org.goldenport.collection.VectorMap
import org.goldenport.tree.Tree
import org.goldenport.values.PathName
import org.goldenport.record.v2._
import org.goldenport.util.StringUtils
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM

/*
 * @since   May. 13, 2025
 *  version May. 17, 2025
 *  version Sep. 30, 2025
 *  version Oct.  7, 2025
 * @version Nov. 18, 2025
 * @author  ASAMI, Tomoharu
 */
case class ScalaModel(
  packages: Tree[SPackage]
) {
  import ScalaModel._
}

object ScalaModel {
  abstract class Context() {
    def optionType(p: TypeName): TypeName = TypeName.Container.option(p)
    def stringType: TypeName = TypeName.Primitive.string
    def optionParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => ???
      case _ => Parameter(p.name, optionType(p.typeName))
    }
    def stringParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.string))
      case _ => Parameter(p.name, TypeName.Primitive.string)
    }
    def shortParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.short))
      case _ => Parameter(p.name, TypeName.Primitive.short)
    }
    def intParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.int))
      case _ => Parameter(p.name, TypeName.Primitive.int)
    }
    def longParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.long))
      case _ => Parameter(p.name, TypeName.Primitive.long)
    }
    def floatParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.float))
      case _ => Parameter(p.name, TypeName.Primitive.float)
    }
    def doubleParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.double))
      case _ => Parameter(p.name, TypeName.Primitive.double)
    }
    def bigintParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.bigint))
      case _ => Parameter(p.name, TypeName.Primitive.bigint)
    }
    def bigdecimalParameter(p: Parameter): Parameter = p.typeName match {
      case m: TypeName.Container => Parameter(p.name, m.withContainee(TypeName.Primitive.bigdecimal))
      case _ => Parameter(p.name, TypeName.Primitive.bigdecimal)
    }
  }
  object Context {
    val default = Default()

    case class Default() extends Context() {
    }
  }
}

case class SPackage(
  traits: Vector[STrait] = Vector.empty,
  caseClasses: Vector[SCaseClass] = Vector.empty,
  enums: Vector[SEnum] = Vector.empty,
  controlClasses: Vector[SControlClass] = Vector.empty
)

case class PackageName(name: String) extends datatype.Name {
  def moveToSubPackage(subpkg: String): PackageName = PackageName(s"$name.$subpkg")

  def toPathName: PathName = PathName(name.replace('.', '/'))
}
object PackageName {
  val orgSimplemodeling = PackageName("org.simplemodeling")
  val orgSimplemodelingRecord = PackageName("org.simplemodeling.record")

  def apply(pn: PackageName, name: String): PackageName =
    PackageName(pn.name + "." + name)
}

case class ClassName(name: String) extends datatype.Name

case class ParameterName(name: String) extends datatype.Name

case class FieldName(name: String) extends datatype.Name

case class AttributeName(name: String) extends datatype.Name

sealed trait ClassDeclaration extends Showable.Value {
}
object ClassDeclaration {
  object CaseClass extends ClassDeclaration {
    protected def print_String = "case class"
  }
}

sealed trait TypeName {
  def fullName: String
  def name: String
  def contentType: TypeName = this
  def isRequired: Boolean
  def isPrimitive: Boolean = false
  def isString: Boolean = false
  def isNumber: Boolean = false
  def isNumberOrigin: Boolean = false
}
object TypeName {
  val datatypePkg = PackageName("org.simplemodeling.datatype")

  case class Primitive(
    datatype: DataType,
    override val isString: Boolean = false,
    override val isNumber: Boolean = false
  ) extends TypeName {
    val name = StringUtils.makeTitle(datatype.name)
    def fullName = name
    def isRequired = true
    override def isPrimitive: Boolean = true
  }
  object Primitive {
    val string = Primitive(XString, isString = true)
    val short = Primitive(XInt, isNumber = true)
    val int = Primitive(XInt, isNumber = true)
    val long = Primitive(XLong, isNumber = true)
    val float = Primitive(XFloat, isNumber = true)
    val double = Primitive(XDouble, isNumber = true)
    val bigint = Primitive(XInteger, isNumber = true)
    val bigdecimal = Primitive(XDecimal, isNumber = true)

    def create(p: DataType): Primitive = createOption(p).get

    def createOption(p: DataType): Option[Primitive] = Option(p).collect {
      case XString => string
      case XShort => short
      case XInt => int
      case XLong => long
      case XFloat => float
      case XDouble => double
      case XInteger => bigint
      case XDecimal => bigdecimal
    }

    def createMarshalling(p: MDatatype): Primitive = createMarshalling(p.datatype)

    def createMarshalling(p: DataType): Primitive = p match {
      case XString => string
      case XInt => int
      case XAge => int
      case _ => string
    }

    def create(name: String): Primitive = ???
  }

  case class Plain(
    packageName: PackageName,
    name: String,
    override val isNumberOrigin: Boolean = false
  ) extends TypeName {
  def fullName =
    if (packageName.name.isEmpty)
      name
    else
      packageName.name + "." + name

    def isRequired = true
  }
  object Plain {
    def apply(p: SClassBase): Plain = Plain(p.packageName, p.className.name)

    def create(pkgname: String, name: String): Plain = Plain(PackageName(pkgname), name)
  }

  case class Container(
    container: TypeName,
    containee: TypeName
  ) extends TypeName {
    def fullName = s"${container.fullName}[${containee.fullName}]"
    def name = s"${container.name}[${containee.name}]"
    override def contentType: TypeName = containee
    def isRequired = false

    def isOption: Boolean = container.name == "Option"
    def isList: Boolean = container.name == "List"
    def isVector: Boolean = container.name == "Vector"
    def isSet: Boolean = container.name == "Set"

    def withContainee(p: TypeName) = copy(containee = p)
  }
  object Container {
    val option = Plain(PackageName("scala"), "Option")
    val consequence = Plain(PackageName("org.simplemodeling"), "Consequence")

    def option(p: TypeName): TypeName = Container(option, p)
  }

  case class Function(
    in: TypeName,
    out: TypeName
  ) extends TypeName {
    def fullName = s"${in.fullName} => ${out.fullName}]"
    def name = s"${in.name}[${out.name}]"
    def isRequired = true
  }

  def option = Container.option
  def consequence = Container.consequence

  val record = Plain(PackageName.orgSimplemodelingRecord, "Record")

//  def apply(name: String): TypeName = Primitive(name)

  def apply(pkg: PackageName, name: String): TypeName = Plain(pkg, name)

  def create(pkg: String, name: String): TypeName = Plain(PackageName(pkg), name)
  def create(p: SClassBase): TypeName = Plain(p)

  def create(p: DataType): TypeName = Primitive.createOption(p).getOrElse(
    Plain(datatypePkg, StringUtils.makeTitle(p.name))
  )

  def option(p: TypeName): Container = Container(option, p)
  def consequence(p: SClassBase): Container = consequence(TypeName.create(p))
  def consequence(p: TypeName): Container = Container(consequence, p)

  def parseC(p: String): Consequence[TypeName] = Consequence(parse(p))

  def parse(s: String): TypeName = _parse_type(s.trim, inParam = false)

  private def _parse_type(s: String, inParam: Boolean): TypeName = {
    val (base, paramOpt) = _split_top_level_param(s)
    paramOpt match {
      case Some(paramStr) =>
        val outer = _to_plain(base.trim)
        val inner = _parse_type(paramStr.trim, inParam = true)
        Container(outer, inner)
      case None => _parse_leaf(base.trim, inParam)
    }
  }

  // Splits "A.B.C[X[Y]]" => ("A.B.C", Some("X[Y]"))
  // If no param => (s, None)
  private def _split_top_level_param(s: String): (String, Option[String]) = {
    val i = s.indexOf('[')
    if (i < 0) (s, None)
    else {
      var level = 0
      var j = i
      while (j < s.length) {
        s.charAt(j) match {
          case '[' => level += 1
          case ']' =>
            level -= 1
            if (level == 0) {
              val base   = s.substring(0, i)
              val inside = s.substring(i + 1, j)
              return (base, Some(inside))
            }
          case _ => ()
        }
        j += 1
      }
      // 不正（']'が無い）でも無理せずそのまま扱う
      (s, None)
    }
  }

  private val _primitive_names: Set[String] =
    Set("String", "Boolean", "Int", "Long", "Short", "Byte", "Double", "Float",
        "Char", "Unit", "BigInt", "BigDecimal")

  private def _parse_leaf(token: String, inParam: Boolean): TypeName = {
    if (token.isEmpty)
      ???

    // qualified?
    val lastDot = token.lastIndexOf('.')
    if (lastDot >= 0) {
      val pkg  = token.substring(0, lastDot)
      val name = token.substring(lastDot + 1)
      // scala.String は Primitive とみなす
      if ((pkg == "scala" || pkg.isEmpty) && _primitive_names(name))
        Primitive.create(name)
      else
        Plain.create(pkg, name)
    } else {
      // unqualified
      if (_primitive_names(token)) {
        if (inParam) Plain.create("scala", token)
        else Primitive.create(token)
      } else {
        // パッケージ不明の標準コンテナは scala とみなす
        token match {
          case "Option" | "List" | "Seq" | "Vector" | "Set" | "Either" | "Try" | "Array" =>
            Plain.create("scala", token)
          case _ =>
            // それ以外は（必要なら）デフォルトパッケージ扱い
            Plain.create("", token)
        }
      }
    }
  }

  private def _to_plain(base: String): Plain = {
    val t = base.trim
    val i = t.lastIndexOf('.')
    if (i >= 0) Plain.create(t.substring(0, i), t.substring(i + 1))
    else Plain.create("scala", t) // パッケージ未指定のコンテナは scala 名前空間想定
  }
}

case class Parameter(
  name: ParameterName,
  typeName: TypeName,
  isAttribute: Boolean = false,
  isDefault: Boolean = false
) {
  def isRequired: Boolean = typeName.isRequired
  def titleName = name.toTitle
}
object Parameter {
  val record = create("record", TypeName.record)

  def create(name: String, typename: String): Parameter =
    create(name, TypeName.parse(typename))

  def create(name: String, typename: TypeName): Parameter =
    Parameter(ParameterName(name), typename)
}

case class ParameterSequence(
  parameters: Vector[Parameter] = Vector.empty
) {
  def distillAttributes: AttributeSequence = AttributeSequence(
    parameters.flatMap {
      case m if (m.isAttribute) => Some(Attribute(m.name.name, m.typeName))
      case _ => None
    }
  )

  def requiredPatameters: Vector[Parameter] = parameters.filter(_.isRequired)
}
object ParameterSequence {
  val empty = ParameterSequence()
}

case class Attribute(
  name: AttributeName,
  typeName: TypeName
) {
}
object Attribute {
  def apply(name: String, typeName: TypeName): Attribute = Attribute(
    AttributeName(name), typeName
  )
}

case class AttributeSequence(
  attributes: Vector[Attribute] = Vector.empty
) {
  def +(rhs: AttributeSequence) = copy(attributes ++ rhs.attributes)
}
object AttributeSequence {
  val empty = AttributeSequence()

  implicit val attributeSequenceMonoid = new Monoid[AttributeSequence] {
    def zero = empty
    def append(lhs: AttributeSequence, rhs: => AttributeSequence) = lhs + rhs
  }
}

case class Field(
  name: FieldName,
  typeName: TypeName
)

case class FieldCompartment(
  fields: Vector[Field] = Vector.empty
) {
  def distillAttributes: AttributeSequence = AttributeSequence.empty // TODO
}
object FieldCompartment {
  val empty = FieldCompartment()
}

case class MethodName(name: String)

case class SMethod(
  name: MethodName,
  parameters: ParameterSequence,
  returnType: TypeName,
  body: Option[() => GenM[Unit]] = None
) {
}
object SMethod {
  def create(name: String, params: ParameterSequence, rtype: TypeName)(body: () => GenM[Unit]): SMethod =
    SMethod(MethodName(name), params, rtype, Some(body))

  def create(name: String, rtype: TypeName, param: Parameter, params: Parameter*)(body: => GenM[Unit]): SMethod =
    create(name, rtype, param +: params)(body)

  def create(name: String, rtype: TypeName, params: Seq[Parameter])(body: => GenM[Unit]): SMethod = {
    val ps = ParameterSequence(params.toVector)
    SMethod(MethodName(name), ps, rtype, Some(() => body))
  }

  def create(name: String, rtype: TypeName)(body: => GenM[Unit]): SMethod = {
    val ps = ParameterSequence.empty
    SMethod(MethodName(name), ps, rtype, Some(() => body))
  }
}

case class MethodCompartment(
  methods: Vector[SMethod] = Vector.empty
)
object MethodCompartment {
  val empty = MethodCompartment()
}

case class Receptor()

case class ReceptionCompartment(
  receptors: Vector[Receptor] = Vector.empty
)
object ReceptionCompartment {
  val empty = ReceptionCompartment()
}

sealed trait SClassBase {
  def packageName: PackageName
  def importNames: Vector[TypeName]
  def declaration: ClassDeclaration
  def className: ClassName
  def parentClass: Option[TypeName]
  def traitList: List[TypeName]
  def parameterSequence: ParameterSequence
//  def attributeSequence: AttributeSequence
  def fieldCompartment: FieldCompartment
  def methodCompartment: MethodCompartment
  def receptionCompartment: ReceptionCompartment

  def attributeSequence: AttributeSequence = parameterSequence.distillAttributes + fieldCompartment.distillAttributes
}

case class ClassCore(
  packageName: PackageName,
  declaration: ClassDeclaration,
  className: ClassName,
  parentClass: Option[TypeName],
  traitList: List[TypeName],
  parameterSequence: ParameterSequence,
  fieldCompartment: FieldCompartment,
  methodCompartment: MethodCompartment,
  receptionCompartment: ReceptionCompartment
) {
  import ClassCore._

  def moveToSubPackage(subpkg: String): ClassCore =
    copy(packageName = packageName.moveToSubPackage(subpkg))

  def importNames: Vector[TypeName.Plain] = {
    case class Z(
      pkgs: VectorMap[PackageName, Set[TypeName.Plain]] = VectorMap.empty
    ) {
      def r: Vector[TypeName.Plain] = {
        val ps = pkgs.keys.toVector.sortBy(_.name)
        ps.foldLeft(Vector.empty[TypeName.Plain])((z, x) =>
          pkgs.get(x).fold(z) { xs =>
            xs.toVector.sortBy(_.name).distinct
          }
        )
      }

      def add(p: Seq[TypeName]): Z = p.foldLeft(this)(_ add _)

      def add(p: Option[TypeName]): Z = p.fold(this)(add)

      def add(p: TypeName): Z = p match {
        case m: TypeName.Primitive => _add(_fqname_primitive(m.name))
        case m: TypeName.Plain => _add(VectorMap(m.packageName -> Set(m)))
        case m: TypeName.Container => add(m.container).add(m.containee)
      }

      def add(p: ParameterSequence): Z =
        p.parameters.foldLeft(this)((z, x) => z.add(x.typeName))

      def add(p: MethodCompartment): Z = 
        p.methods.foldLeft(this)((z, x) => z.add(x.parameters).add(x.returnType))

      def add(p: ReceptionCompartment): Z = this // TODO

      private def _add(p: VectorMap[PackageName, Set[TypeName.Plain]]) =
        copy(pkgs = pkgs |+| p)

      private def _fqname_primitive(name: String): VectorMap[PackageName, Set[TypeName.Plain]] =
        if (builtInTypes.contains(name))
          VectorMap.empty
        else
          _pkg_simplemodeling_datatype(name)

      private def _pkg_simplemodeling_datatype(name: String): VectorMap[PackageName, Set[TypeName.Plain]] = {
        val pkg = PackageName("org.simplemodeling.datatype")
        VectorMap(pkg -> Set(TypeName.Plain(pkg, name)))
      }
    }
    Z().add(parentClass).add(traitList).
      add(parameterSequence).
      add(methodCompartment).
      add(receptionCompartment).r
  }
}
object ClassCore {
  val builtInTypes = Set("String", "Int", "Long", "Boolean", "Double", "Float", "Short", "Byte", "Char")

  trait Holder {
    def core: ClassCore

    def packageName = core.packageName
    def declaration = core.declaration
    def className = core.className
    def parentClass = core.parentClass
    def traitList = core.traitList
    def parameterSequence = core.parameterSequence
//    def attributeSequence = core.attributeSequence
    def fieldCompartment = core.fieldCompartment
    def methodCompartment = core.methodCompartment
    def receptionCompartment = core.receptionCompartment
    def importNames = core.importNames
  }
}

case class SComponent(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

case class STrait(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

case class SCaseClass(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

case class SEnum(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

case class SControlClass(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

case class SEntityClass(core: ClassCore) extends SClassBase with ClassCore.Holder {
}

sealed trait SSlot {
}

case class SAttribute() extends SSlot {
}

case class SAbstractMethod() extends SSlot {
}

case class SConcreteMethod() extends SSlot {
}
