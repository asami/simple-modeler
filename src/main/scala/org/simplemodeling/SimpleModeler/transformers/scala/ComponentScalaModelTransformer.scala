package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 11, 2026
 * @version Feb. 18, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentScalaModelTransformer() extends ScalaModelTransformer() {
  protected def accept_Purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_Accept_Object(p: MObject): Boolean = p.isInstanceOf[MComponent]
  def apply(p: (MObject, Purpose)): Consequence[Vector[SClassBase]] =
    p match {
      case (m: MComponent, purpose) if is_accept_purpose(purpose) => transform_component(m)
      case _ => Consequence.noReachDefect(s"ComponentShellScalaModelTransformer#apply")
    }

  protected def transform_component(p: MComponent): Consequence[Vector[SClassBase]] = Consequence {
    val c = _to_component(p)
    Vector(c, _to_implementation(c))
  }

  private def _to_component(p: MComponent): SComponent =
    new ComponentBuilder(p).build()

  class ComponentBuilder(
    val source: MComponent
  ) {
//    val componentClassName = make_title_name(source.name, "Component")
    val componentName = source.name
    val componentClassName = make_title_name(source.name, "Component")
    val core = to_scala_core_with_subpackage(source).
      withClassName(componentClassName)
    val componentPackageName = PackageName(source.packageName)

    // private var _actions: Vector[SCaseClass] = Vector.empty

    def build(): SComponent = {
      val services = to_services(source.services)
      val ccore = SComponent.ComponentCore(
        componentName,
        services
      )
      SComponent(core, ccore)
    }

    final protected def to_services(ps: Seq[MService]): List[SService] =
      ps.map(to_service).toList

    final protected def to_service(p: MService): SService = {
      val name = p.name
      val (ops, actions) = to_actions(p.operations)
      SService(p.packageName, name, ops, actions)
    }

    final protected def to_actions(
      ps: List[MOperation]
    ): (MethodCompartment, Vector[SCaseClass]) = {
      case class Z(
        methods: Vector[SMethod] = Vector.empty,
        actions: Vector[SCaseClass] = Vector.empty
      ) {
        def r = {
          (MethodCompartment(methods), actions)
        }

        def +(rhs: (SMethod, Vector[SCaseClass])) = {
          val (m, as) = rhs
          copy(methods = methods :+ m, actions = actions ++ as)
        }
      }
      ps.map(to_action).foldLeft(Z())(_+_).r
    }

    final protected def to_action(p: MOperation): (SMethod, Vector[SCaseClass]) = {
      val action = _create_action(p)
      val actionclassname = action.fullName
      val rtype = to_result(p.result)
      val ap = Parameter.create("action", actionclassname)
      val aps = ParameterSequence(Vector(ap))
      val method = SMethod(MethodName(p.name), aps, rtype)
      (method, Vector(action))
    }

    private def _create_action(p: MOperation): SCaseClass = {
      val pkgname = componentPackageName // TODO
      val name = p.name
      val actionkind = "command"
      val actionname = make_title_name(name, actionkind)
      val params = p.parameters.toVector.map(to_parameter)
      val parameters = ParameterSequence(params)
      val action = SCaseClass(
        ClassCore(
          pkgname,
          ClassDeclaration.CaseClass,
          ClassName(actionname),
          parameterSequence = parameters
        )
      )
//      _actions = _actions :+ action
//      actionname
      action
    }

    // final protected def to_parameter(p: MParameter): Parameter = {
    //   import MParameter._

    //   p.parameterType match {
    //     case MDataTypeParameterType(dt) => ???
    //     case MObjectParameterType(o) => ???
    //     case MObjectRefParameterType(ref) => ???
    //   }
    // }

    // final protected def to_datatype(p: MResult): TypeName = ???
  }

  private def _to_implementation(p: SComponent): SControlClass = {
    new ImplementationBuilder(p).build()
  }

  class ImplementationBuilder(val component: SComponent) extends SComponent.Processor {
    def build(): SControlClass = {
      val pkg = _impl_package(component)
      val decl = ClassDeclaration.Control
      val classname = make_title_name("ComponentFactory") // TODO
      val parent = component_factory_typename
      val core = ClassCore(pkg, decl, ClassName(classname), Some(parent))
      SControlClass(core)
    }

    private def _impl_package(p: SComponent): PackageName = {
      val a = sub_Package_Name.fold("")(x => "." + x)
      PackageName(p.packageName + a + ".impl")
    }
  }
}
