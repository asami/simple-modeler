package org.simplemodeling.SimpleModeler.generator.scala.model

import scalaz._, Scalaz._
import org.goldenport.context.Showable
import org.goldenport.datatype
import org.goldenport.collection.VectorMap
import org.goldenport.tree.Tree
import org.goldenport.record.v2.DataType
import org.goldenport.util.StringUtils

/*
 * @since   May. 13, 2025
 *  version May. 17, 2025
 * @version Sep. 17, 2025
 * @author  ASAMI, Tomoharu
 */
case class ScalaModel(
  packages: Tree[SPackage]
) {
  import ScalaModel._
}

object ScalaModel {
}

case class SPackage(
  traits: Vector[STrait] = Vector.empty,
  caseClasses: Vector[SCaseClass] = Vector.empty,
  enums: Vector[SEnum] = Vector.empty,
  controlClasses: Vector[SControlClass] = Vector.empty
)

case class ParameterName(name: String) extends datatype.Name

case class PackageName(name: String) extends datatype.Name

case class ClassName(name: String) extends datatype.Name

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
}
object TypeName {
  case class Primitive(datatype: DataType) extends TypeName {
    val name = StringUtils.makeTitle(datatype.name)
    def fullName = name
  }

  case class Plain(
    packageName: PackageName,
    name: String
  ) extends TypeName {
  def fullName =
    if (packageName.name.isEmpty)
      name
    else
      packageName.name + "." + name
  }

  case class Container(
    container: TypeName,
    containee: TypeName
  ) extends TypeName {
    def fullName = s"${container.fullName}[${containee.fullName}]"
    def name = s"${container.name}[${containee.name}]"
  }

//  def apply(name: String): TypeName = Primitive(name)

  def apply(pkg: PackageName, name: String): TypeName = Plain(pkg, name)
  def apply(pkg: String, name: String): TypeName = Plain(PackageName(pkg), name)
}

case class Parameter(
  name: ParameterName,
  typeName: TypeName
)

case class ParameterSequence(
  parameters: Vector[Parameter] = Vector.empty
)

case class MethodName(name: String)

case class SMethod(
  name: MethodName,
  parameters: ParameterSequence,
  returnType: TypeName
) {
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
  def methodCompartment: MethodCompartment
  def receptionCompartment: ReceptionCompartment
}

case class ClassCore(
  packageName: PackageName,
  declaration: ClassDeclaration,
  className: ClassName,
  parentClass: Option[TypeName],
  traitList: List[TypeName],
  parameterSequence: ParameterSequence,
  methodCompartment: MethodCompartment,
  receptionCompartment: ReceptionCompartment
) {
  import ClassCore._

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
