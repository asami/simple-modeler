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
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind

/*
 * @since   May. 13, 2025
 *  version May. 17, 2025
 *  version Sep. 30, 2025
 *  version Oct.  7, 2025
 *  version Nov. 18, 2025
 *  version Feb. 28, 2026
 * @version Mar. 23, 2026
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
      case m: TypeName.Container => p
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

case class PackageName(name: String) {
  def moveToSubPackage(subpkg: String): PackageName = PackageName(s"$name.$subpkg")

  def toPathName: PathName = PathName(name.replace('.', '/'))

  def isPlatform = name.startsWith("org.goldenport.")
}
object PackageName {
  val orgSimplemodeling = PackageName("org.goldenport")
  val orgSimplemodelingRecord = PackageName("org.goldenport.record")

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
  object Service extends ClassDeclaration {
    protected def print_String = "class"
  }
  object Control extends ClassDeclaration {
    protected def print_String = "class"
  }
}

sealed abstract class TypeName {
  def fullName: String
  def name: String
  def contentType: TypeName = this
  def isRequired: Boolean
  def isPrimitive: Boolean = false
  def isString: Boolean = false
  def isNumber: Boolean = false
  def isNumberOrigin: Boolean = false
  def isPlatform: Boolean = false

  def toRawType: TypeName = this
  def toOptionType: TypeName = TypeName.option(this)
  def toListType: TypeName = TypeName.list(this)
  def toVectorType: TypeName = TypeName.vector(this)
  def toSetType: TypeName = TypeName.set(this)
}
object TypeName {
  val datatypePkg = PackageName("org.goldenport.datatype")
  val modelDatatypePkg = PackageName("org.goldenport.model.datatype")

  case class Unit() extends TypeName {
    val name = "unit"
    val fullName = name
    def isRequired = true
  }

  case class Primitive(
    datatype: DataType,
    override val isString: Boolean = false,
    override val isNumber: Boolean = false
  ) extends TypeName {
    val name = StringUtils.makeTitle(datatype.name)
    def fullName = name
    def isRequired = true
    override def isPrimitive: Boolean = true
    override def isPlatform: Boolean = true
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

    def createMarshalling(p: MDataType): Primitive = createMarshalling(p.datatype)

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
    override def isPlatform: Boolean = packageName.isPlatform
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

    override def toRawType: TypeName = containee

    override def toOptionType: TypeName =
      if (isOption)
        this
      else
        Container.option(containee)

    override def toListType: TypeName =
      if (isList)
        this
      else
        Container.list(containee)

    override def toVectorType: TypeName =
      if (isVector)
        this
      else
        Container.vector(containee)

    override def toSetType: TypeName =
      if (isSet)
        this
      else
        Container.set(containee)
  }
  object Container {
    val option = Plain(PackageName("scala"), "Option")
    val list = Plain(PackageName("scala"), "List")
    val vector = Plain(PackageName("scala"), "Vector")
    val set = Plain(PackageName("scala"), "Set")
    val consequence = Plain(PackageName("org.simplemodeling"), "Consequence")
    val consequenceFailure = Plain(PackageName("org.simplemodeling"), "Consequence.Failure[_]")

    def option(p: TypeName): Container = Container(option, p)
    def list(p: TypeName): Container = Container(list, p)
    def vector(p: TypeName): Container = Container(vector, p)
    def set(p: TypeName): Container = Container(set, p)
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
  def consequenceFailure = Container.consequenceFailure

  def failureVector = Container.vector(consequenceFailure)

  val record = Plain(PackageName.orgSimplemodelingRecord, "Record")

//  def apply(name: String): TypeName = Primitive(name)

  def apply(pkg: PackageName, name: String): TypeName = Plain(pkg, name)

  def create(pkg: String, name: String): TypeName = Plain(PackageName(pkg), name)
  def create(p: SClassBase): TypeName = Plain(p)

  def create(p: DataType): TypeName = Primitive.createOption(p).getOrElse {
    p match {
      case XEntityId => Plain(modelDatatypePkg, "EntityId")
      case _ => Plain(datatypePkg, StringUtils.makeTitle(p.name))
    }
  }

  def option(p: TypeName): Container = Container.option(p)
  def list(p: TypeName): Container = Container.list(p)
  def vector(p: TypeName): Container = Container.vector(p)
  def set(p: TypeName): Container = Container.set(p)
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
  isDefault: Boolean = false,
  value: Option[SClassBase] = None,
  dbColumnName: Option[String] = None,
  dbColumnType: Option[String] = None,
  externalName: Option[String] = None
) {
  def isRequired: Boolean = typeName.isRequired
  def titleName = name.toTitle

  def toRawType: Parameter = copy(typeName = typeName.toRawType)
  def toOptionType: Parameter = copy(typeName = typeName.toOptionType)
  def toListType: Parameter = copy(typeName = typeName.toListType)
  def toVectorType: Parameter = copy(typeName = typeName.toVectorType)
  def toSetType: Parameter = copy(typeName = typeName.toSetType)
}
object Parameter {
  val record = create("record", TypeName.record)

  def create(name: String, typename: String): Parameter =
    create(name, TypeName.parse(typename))

  def create(name: String, typename: TypeName): Parameter =
    Parameter(ParameterName(name), typename)

  def create(name: String, value: SClassBase): Parameter =
    Parameter(ParameterName(name), TypeName.create(value), value = Some(value))
}

case class ParameterSequence(
  parameters: Vector[Parameter] = Vector.empty
) {
  def distillAttributes: AttributeSequence = AttributeSequence(
    parameters.flatMap {
      case m if (m.isAttribute) => Some(
        Attribute(
          m.name.name,
          m.typeName,
          m.dbColumnName,
          m.dbColumnType,
          m.externalName
        )
      )
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
  typeName: TypeName,
  dbColumnName: Option[String] = None,
  dbColumnType: Option[String] = None,
  externalName: Option[String] = None
) {
}
object Attribute {
  def apply(name: String, typeName: TypeName): Attribute = Attribute(
    AttributeName(name), typeName
  )

  def apply(
    name: String,
    typeName: TypeName,
    dbColumnName: Option[String],
    dbColumnType: Option[String]
  ): Attribute = Attribute(
    AttributeName(name),
    typeName,
    dbColumnName,
    dbColumnType,
    None
  )

  def apply(
    name: String,
    typeName: TypeName,
    dbColumnName: Option[String],
    dbColumnType: Option[String],
    externalName: Option[String]
  ): Attribute = Attribute(
    AttributeName(name),
    typeName,
    dbColumnName,
    dbColumnType,
    externalName
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
  descriptor: SMethod.Descriptor,
  parameters: ParameterSequence,
  returnType: TypeName,
  body: Option[() => GenM[Unit]] = None
) {
}
object SMethod {
  sealed trait Kind
  object Kind {
    case object Command extends Kind
    case object Query extends Kind
  }

  case class Descriptor(
    kind: Kind
  )
  object Descriptor {
    val command = Descriptor(Kind.Command)
    val query = Descriptor(Kind.Query)
  }

  def query(name: String, params: ParameterSequence, rtype: TypeName)(body: () => GenM[Unit]): SMethod =
    SMethod(MethodName(name), Descriptor.query, params, rtype, Some(body))

  def query(name: String, rtype: TypeName, param: Parameter, params: Parameter*)(body: => GenM[Unit]): SMethod =
    query(name, rtype, param +: params)(body)

  def query(name: String, rtype: TypeName, params: Seq[Parameter])(body: => GenM[Unit]): SMethod = {
    val ps = ParameterSequence(params.toVector)
    SMethod(MethodName(name), Descriptor.query, ps, rtype, Some(() => body))
  }

  def query(name: String, rtype: TypeName)(body: => GenM[Unit]): SMethod = {
    val ps = ParameterSequence.empty
    SMethod(MethodName(name), Descriptor.query, ps, rtype, Some(() => body))
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

case class Directive(
  classKind: Option[ClassKind] = None,
  purpose: Option[Purpose] = None,
  canonicalSchemaOwner: Option[TypeName.Plain] = None
) {
  def isPlain: Boolean = purpose.fold(true)(_ == Purpose.Plain)
  def isCreate: Boolean = purpose.fold(false)(_ == Purpose.Create)
  def isQuery: Boolean = purpose.fold(false)(_ == Purpose.Query)
  def isUpdate: Boolean = purpose.fold(false)(_ == Purpose.Update)

  def withEntityValue = copy(classKind = Some(ClassKind.EntityValue))
  def withPurpose(purpose: Purpose) = copy(purpose = Some(purpose))
  def withCanonicalSchemaOwner(owner: TypeName.Plain) = copy(canonicalSchemaOwner = Some(owner))
}
object Directive {
  val default = Directive()
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

  def fullName: String = s"${packageName.name}.${className.name}"

  def directive: Directive
}

case class ClassCore(
  packageName: PackageName,
  declaration: ClassDeclaration,
  className: ClassName,
  parentClass: Option[TypeName] = None,
  traitList: List[TypeName] = Nil,
  parameterSequence: ParameterSequence = ParameterSequence.empty,
  fieldCompartment: FieldCompartment = FieldCompartment.empty,
  methodCompartment: MethodCompartment = MethodCompartment.empty,
  receptionCompartment: ReceptionCompartment = ReceptionCompartment.empty,
  directive: Directive = Directive.default
) {
  import ClassCore._

  def moveToSubPackage(subpkg: String): ClassCore =
    copy(packageName = packageName.moveToSubPackage(subpkg))

  def withClassName(name: String): ClassCore = copy(className = ClassName(name))

  def withEntityValue: ClassCore = copy(directive = directive.withEntityValue)

  def withPurpose(purpose: Purpose): ClassCore = copy(directive = directive.withPurpose(purpose))

  def importNames: Vector[TypeName.Plain] = {
    case class Z(
      pkgs: VectorMap[PackageName, Set[TypeName.Plain]] = VectorMap.empty
    ) {
      def r: Vector[TypeName.Plain] = {
        val ps = pkgs.keys.toVector.sortBy(_.name)
        val xs = ps.foldLeft(Vector.empty[TypeName.Plain])((z, x) =>
          pkgs.get(x).fold(z) { xs =>
            xs.toVector.sortBy(_.name).distinct
          }
        )
        xs.map(_normalize)
      }

      private def _normalize(p: TypeName.Plain) =
        if (p.name.contains("."))
          p.copy(name = p.name.takeWhile(_ != '.'))
        else
          p

      def add(p: Seq[TypeName]): Z = p.foldLeft(this)(_ add _)

      def add(p: Option[TypeName]): Z = p.fold(this)(add)

      def add(p: TypeName): Z = p match {
        case m: TypeName.Primitive => _add(_fqname_primitive(m.name))
        case m: TypeName.Plain => _add(VectorMap(m.packageName -> Set(m)))
        case m: TypeName.Container => add(m.container).add(m.containee)
        case m: TypeName.Function => add(m.in).add(m.out)
        case m: TypeName.Unit => this
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
    Z().add(parentClass).
      add(traitList).
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
    def directive = core.directive
  }

  def service(
    pkg: String,
    name: String,
    methods: MethodCompartment
  ): ClassCore = ClassCore(
    PackageName(pkg),
    ClassDeclaration.Service,
    ClassName(s"${StringUtils.makeTitle(name)}Service"),
    methodCompartment = methods
  )
}

trait SClassBaseWithCore extends SClassBase with ClassCore.Holder

case class STrait(core: ClassCore) extends SClassBaseWithCore {
}

case class SCaseClass(core: ClassCore) extends SClassBaseWithCore {
}

case class SEnum(core: ClassCore) extends SClassBaseWithCore {
}

case class SControlClass(core: ClassCore) extends SClassBaseWithCore {
}

case class STypedClass(
  container: SClassBaseWithCore,
  containee: SClassBaseWithCore
) extends SClassBaseWithCore {
  def core = container.core
}

case class SEntityClass(core: ClassCore) extends SClassBaseWithCore {
}

sealed trait SSlot {
}

case class SAttribute() extends SSlot {
}

case class SAbstractMethod() extends SSlot {
}

case class SConcreteMethod() extends SSlot {
}

case class SService(
  core: ClassCore,
  serviceCore: SService.ServiceCore
) extends SClassBaseWithCore with SService.ServiceCore.Holder {
  def methods: Vector[SMethod] = methodCompartment.methods
}
object SService {
  case class ServiceCore(
    serviceName: String,
    actions: Vector[SCaseClass]
  )
  object ServiceCore {
    trait Holder {
      def serviceCore: ServiceCore

      def serviceName = serviceCore.serviceName
      def actions = serviceCore.actions
    }
  }

  def apply(
    pkg: String,
    name: String,
    methods: MethodCompartment,
    actions: Seq[SCaseClass]
  ): SService = {
    SService(
      ClassCore.service(pkg, name, methods),
      ServiceCore(name, actions.toVector)
    )
  }
}

case class SComponent(
  core: ClassCore,
  componentCore: SComponent.ComponentCore
) extends SClassBaseWithCore with SComponent.ComponentCore.Holder {
}
object SComponent {
  sealed trait TransitionTrigger
  object TransitionTrigger {
    case object Save extends TransitionTrigger
    case object Update extends TransitionTrigger
  }

  sealed trait RuleGuard
  object RuleGuard {
    final case class Ref(name: String) extends RuleGuard
    final case class Expression(expr: String) extends RuleGuard
  }

  final case class RuleAction(
    script: String
  )

  final case class RulePlan(
    exit: Vector[RuleAction] = Vector.empty,
    transition: Option[RuleAction] = None,
    entry: Vector[RuleAction] = Vector.empty
  )

  final case class StateMachineTransitionRule(
    collectionName: String,
    trigger: TransitionTrigger,
    eventName: String,
    priority: Int = 0,
    declarationOrder: Int = 0,
    guard: Option[RuleGuard] = None,
    plan: RulePlan = RulePlan()
  )

  final case class EventReceptionDefinition(
    name: String,
    category: String = "NonActionEvent",
    kind: Option[String] = None,
    selectors: Map[String, String] = Map.empty,
    actionName: Option[String] = None,
    priority: Int = 0
  )

  final case class EventRoutingDefinition(
    name: String,
    when: Option[String] = None,
    topic: Option[String] = None,
    service: Option[String] = None,
    partition: Option[String] = None
  )

  final case class EventSubscriptionDefinition(
    name: String,
    eventName: String,
    route: String = "Unicast",
    entityName: Option[String] = None,
    target: Option[String] = None,
    targets: Vector[String] = Vector.empty,
    selector: Option[String] = None,
    actionName: String,
    declaredTargetUpperBound: Int = 1,
    activation: Option[String] = None
  )

  final case class AggregateDefinition(
    name: String,
    entityName: String
  )

  final case class ViewDefinition(
    name: String,
    entityName: String,
    viewNames: Vector[String] = Vector.empty
  )

  final case class ComponentCoordinate(
    group: String,
    artifact: String,
    version: String
  ) {
    def asString: String = s"${group}:${artifact}:${version}"
  }

  final case class ComponentDefinition(
    name: String,
    coordinates: Vector[ComponentCoordinate] = Vector.empty,
    componentlets: Vector[String] = Vector.empty,
    extensionPoints: Vector[String] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty
  )

  final case class SubsystemDefinition(
    name: String,
    components: Vector[ComponentCoordinate] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    config: Map[String, String] = Map.empty
  )

  final case class OperationDefinition(
    name: String,
    kind: String,
    inputType: String,
    outputType: String,
    inputValueKind: String,
    parameters: Vector[OperationField] = Vector.empty
  )

  final case class OperationField(
    name: String,
    datatype: String,
    multiplicity: String = "1"
  )

  case class ComponentCore(
    componentName: String,
    services: List[SService],
    stateMachineTransitionRules: Vector[StateMachineTransitionRule] = Vector.empty,
    eventReceptionDefinitions: Vector[EventReceptionDefinition] = Vector.empty,
    eventRoutingDefinitions: Vector[EventRoutingDefinition] = Vector.empty,
    eventSubscriptionDefinitions: Vector[EventSubscriptionDefinition] = Vector.empty,
    aggregateDefinitions: Vector[AggregateDefinition] = Vector.empty,
    viewDefinitions: Vector[ViewDefinition] = Vector.empty,
    operationDefinitions: Vector[OperationDefinition] = Vector.empty,
    componentDefinitions: Vector[ComponentDefinition] = Vector.empty,
    subsystemDefinitions: Vector[SubsystemDefinition] = Vector.empty
  )
  object ComponentCore {
    trait Holder {
      def componentCore: ComponentCore

      def componentName = componentCore.componentName
      def services = componentCore.services
      def stateMachineTransitionRules = componentCore.stateMachineTransitionRules
      def eventReceptionDefinitions = componentCore.eventReceptionDefinitions
      def eventRoutingDefinitions = componentCore.eventRoutingDefinitions
      def eventSubscriptionDefinitions = componentCore.eventSubscriptionDefinitions
      def aggregateDefinitions = componentCore.aggregateDefinitions
      def viewDefinitions = componentCore.viewDefinitions
      def operationDefinitions = componentCore.operationDefinitions
      def componentDefinitions = componentCore.componentDefinitions
      def subsystemDefinitions = componentCore.subsystemDefinitions
    }
  }

  abstract class Processor() {
    def component: SComponent

    final protected def component_name: String = component.componentName

    final protected def component_class_name: String = make_title(component_name) + "Component"

    final protected def component_packagename: PackageName = component.packageName

    final protected def component_factory_class_name: String = s"$component_class_name.Factory"

    final protected def component_factory_typename: TypeName =
      TypeName(component_packagename, component_factory_class_name)

    final protected def services: List[SService] = component.services

    final protected def service_name(service: SService) = service.serviceName

    final protected def service_object_name(service: SService): String =
      make_title(service.serviceName) + "Service"

    final protected def operation_name(op: SMethod): String = op.name.name

    final protected def operation_object_name(op: SMethod): String = make_title(operation_name(op)) + "Operation"

    final protected def action_class_name(op: SMethod): String = {
      val actionclass = op.descriptor.kind match {
        case SMethod.Kind.Query => "Query"
        case SMethod.Kind.Command => "Command"
      }
      make_title(operation_name(op)) + actionclass
    }

    final protected def action_call_class_name(op: SMethod): String =
      make_title(operation_name(op)) + "ActionCall"

    final protected def typename_relative_name(p: TypeName): String =
      p match {
        case TypeName.Plain(pkg, name, _) => _relative_plain_name(pkg, name)
        case TypeName.Container(container, containee) =>
          s"${typename_relative_name(container)}[${typename_relative_name(containee)}]"
        case TypeName.Function(in, out) =>
          s"${typename_relative_name(in)} => ${typename_relative_name(out)}"
        case _ => p.name
      }

    private def _relative_plain_name(pkg: PackageName, name: String): String = {
      if (!_is_component_subpackage(pkg))
        if (_is_entity_family_package(pkg))
          s"_root_.${pkg.name}.$name"
        else
          name
      else {
        val relativeSegments = _relative_package_segments(pkg)
        if (relativeSegments.isEmpty)
          name
        // Keep entity-family references absolute to avoid collisions with
        // local parameter names such as `entity` and imported CNCF symbols.
        else if (
          relativeSegments.head == "entity" ||
          relativeSegments.head == "aggregate" ||
          relativeSegments.head == "view"
        )
          s"_root_.${pkg.name}.$name"
        else
          s"${relativeSegments.mkString(".")}.$name"
      }
    }

    private def _is_entity_family_package(pkg: PackageName): Boolean = {
      val segments = _package_segments(pkg)
      segments.contains("entity") || segments.contains("aggregate") || segments.contains("view")
    }

    private def _relative_package_segments(pkg: PackageName): Vector[String] = {
      val target = _package_segments(pkg)
      val base = _package_segments(component_packagename)
      val shared = target.zip(base).takeWhile { case (x, y) => x == y }.length
      target.drop(shared)
    }

    // true when `pkg` is equal to or lives below the component package
    private def _is_component_subpackage(pkg: PackageName): Boolean = {
      val target = _package_segments(pkg)
      val base = _package_segments(component_packagename)
      base.isEmpty || (
        target.length >= base.length && target.take(base.length) == base
      )
    }

    private def _package_segments(pkg: PackageName): Vector[String] =
      if (pkg.name.isEmpty) Vector.empty
      else pkg.name.split('.').toVector

    final protected def make_title(s: String) = {
      val raw = StringUtils.makeTitle(s)
      if (raw.length <= 32)
        raw
      else
        raw.take(32)
    }
  }
}
