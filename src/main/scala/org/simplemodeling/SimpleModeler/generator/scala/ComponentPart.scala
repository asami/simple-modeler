package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import model._
import Generator.{State => GState, _}
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM

/*
 * @since   Feb. 12, 2026
 *  version Feb. 27, 2026
 * @version Mar.  9, 2026
 * @author  ASAMI, Tomoharu
 */
trait ComponentPart[T <: SClassBase] { self: Scala3ClassGeneratorExecutor[T] =>
  private val _component: Option[SComponent] = clazz match {
    case m: SComponent => Some(m)
    case _ => None
  }

  protected val service_vector: Vector[SService] = _component match {
    case Some(s) => s.services.toVector
    case None => Vector.empty
  }

  protected final def component_class_part(
  ): GenM[Unit] = {
    unit
  }

  protected final def component_object_part(
  ): GenM[Unit] =
    _component match {
      case Some(s) => new ComponentProcessor(s).serviceObjectPart()
      case None => unit
    }

  class ComponentProcessor(
    val component: SComponent
  ) extends SComponent.Processor {
    import ComponentProcessor._

    def serviceObjectPart(): GenM[Unit] = {
      for {
        ds <- _services()
        _ <- _factory(ds)
      } yield ()
    }

    private def _factory(actioncalldescs: ActionCallDescriptorCollection): GenM[Unit] = {
      val servicedefs = services.map(service_object_name)
      for {
        _ <- println(s"""val name = "${component_name}"""")
        _ <- println(s"val componentId = ComponentId(name) // TODO")
        _ <- separator
        _ <- println(s"class Factory extends Component.Factory {")
        _ <- indent
        _ <- println("protected def create_Components(params: ComponentCreate): Vector[Component] =")
        _ <- indent
        _ <- println(s"Vector(${component_class_name}())")
        _ <- outdent
        _ <- separator
        _ <- println(s"protected def create_Core(")
        _ <- indent
        _ <- println(s"params: ComponentCreate,")
        _ <- println(s"comp: Component")
        _ <- outdent
        _ <- println(s"): Component.Core = spec_create(")
        _ <- indent
        _ <- println(s"name,")
        _ <- println(s"componentId,")
        _ <- blockExpression("Vector")(servicedefs)
        _ <- outdent
        _ <- println(s")")
        _ <- separator
        _ <- actioncalldescs.setup
        _ <- outdent
        _ <- println(s"}")
        _ <- actioncalldescs.define
      } yield ()
    }

    private def _action_call_factory(actioncalldescs: ActionCallDescriptorCollection): GenM[Unit] = {
      actioncalldescs.methodsInServices
    }

    // private def _action_call_create(): GenM[Unit] = {
    //   val actionclassname = ???
    //   val actioncallname: String = ???
    //   val actioncallclassname = ???
    //   for {
    //     _ <- block(s"def create${actioncallname}(") {
    //       for {
    //         _ <- println(s"core: ActionCall.Core,")
    //         _ <- println(s"action: ${actionclassname}")
    //       } yield ()
    //     }
    //     _ <- block(s"): $actioncallname =") {
    //       println(s"$actioncallclassname(core, action)")
    //     }
    //   } yield()
    // }

    private def _services(): GenM[ActionCallDescriptorCollection] =
      services.traverse(_service(_)).map(ActionCallDescriptorCollection.combineAll)

    private def _service(service: SService): GenM[ActionCallDescriptorCollection] = {
      val servicename = service_name(service)
      val serviceobjectname = service_object_name(service)
      val ops = service.methods.map(operation_object_name)
      for {
        ds <- blockR(s"object ${serviceobjectname} extends ServiceDefinition {") {
          for {
            _ <- block(s"""val specification = ServiceDefinition.Specification.Builder("${servicename}").""") {
              ops.toList match {
                case Nil => unit
                case x :: xs => for {
                  _ <- block("operation(") {
                    println(x)
                  }
                  _ <- xs.traverse(x =>
                    block(").operation(") {
                      println(x)
                    }
                  )
                  _ <- println(s").build()")
                } yield ()
              }
            }
            ds <- service.methods.traverse(_operation(servicename))
          } yield ds
        }
      } yield ActionCallDescriptorCollection(servicename, serviceobjectname, ds)
    }

    private def _operation(servicename: String)(op: SMethod): GenM[ActionCallDescriptor] = {
      val operationobject = operation_object_name(op)
      val operationname = operation_name(op)
      val actionclassname = action_class_name(op)
      for {
        _ <- separator
        _ <- block(s"object ${operationobject} extends OperationDefinition") {
          for {
            _ <- block(s"""val specification = OperationDefinition.Specification.Builder("$operationname").""") {
              println(s"build()")
            }
            _ <- separator
            _ <- block("override def createOperationRequest(") {
              println("req: Request")
            }
            _ <- block(s"): Consequence[${actionclassname}] =") {
              println(s"${actionclassname}.create(req)")
            }
          } yield ()
        }
        _ <- _action(servicename, op)
        ds <- _action_call(op)
      } yield ds
    }

    private def _action(servicename: String, op: SMethod): GenM[Unit] = {
      val actionclassname = action_class_name(op)
      val actioncallclassname = action_call_class_name(op)
      val factorymethodname = s"create${actioncallclassname}"
      val opkind = op.descriptor.kind match {
        case SMethod.Kind.Query => "QueryAction"
        case SMethod.Kind.Command => "CommandAction"
      }
      val (paramname, paramclasstype) = _param_descriptor(op)
      for {
        _ <- separator
        _ <- block(s"final case class ${actionclassname}(") {
          for {
            _ <- println("request: Request,")
            _ <- println(s"${paramname}: ${typename_relative_name(paramclasstype)}")
          } yield ()
        }
        _ <- block(s") extends ${opkind}()") {
          block(s"override def createCall(core: ActionCall.Core): ActionCall =") {
            for {
              _ <- block(s"core.getFactory[${component_factory_class_name}] match") {
                for {
                  _ <- println(s"case Some(s) => s.${servicename}.${factorymethodname}(core, this)")
                  _ <- println(s"case None => ${actioncallclassname}(core, this)")
                } yield ()
              }
            } yield ()
          }
        }
        _ <- _action_companion_object(op, paramname, paramclasstype)
      } yield ()
    }

    private def _param_descriptor(op: SMethod): (String, TypeName) =
      op.parameters.parameters.headOption match {
        case Some(s) => s.value match {
          case Some(v) => _action_descriptor(v)
          case None => (s.name.name, s.typeName)
        }
        case None => ("p", TypeName.parse("Record")) // TODO
      }

    private def _action_descriptor(action: SClassBase): (String, TypeName) =
      action.parameterSequence.parameters.headOption match {
        case Some(s) => s.value match {
          case Some(v) => (s.name.name, TypeName.create(v))
          case None => (s.name.name, s.typeName)
        }
        case None => ("p", TypeName.create(action))
      }

    private def _action_companion_object(
      op: SMethod,
      paramname: String,
      paramtype: TypeName
    ): GenM[Unit] = {
      val actionclassname = action_class_name(op)
      block(s"object $actionclassname") {
        block(s"def create(request: Request): Consequence[$actionclassname] =") {
          _action_companion_object_create_body(paramname, paramtype, actionclassname)
        }
      }
    }

    private def _action_companion_object_create_body(
      paramname: String,
      paramtype: TypeName,
      actionclassname: String
    ): GenM[Unit] = paramtype match {
      case m if m.fullName == "org.goldenport.record.Record" =>
        println(s"Consequence.success($actionclassname(request, request.toRecord))")
      case m if m.isPlatform =>
        block(s"""Consequence.successOrRecordNotFound[${paramtype.name}]("${paramname}", request.toRecord).""") {
          println(s"map($actionclassname(request, _))")
        }
      case TypeName.Container(container, containee) => containee match {
        case mm if mm.fullName == "org.goldenport.record.Record" =>
          println(s"Consequence.success($actionclassname(request, ${container.name}(request.toRecord)))")
        case mm if mm.isPlatform => 
          block(s"""Consequence.successOrRecordNotFound[${paramtype.name}]("${paramname}", request.toRecord).""") {
            println(s"map(x => $actionclassname(request, ${container.name}(x)))")
          }
        case _ => 
          block(s"${containee.fullName}.createC(request.toRecord).") {
            println(s"map(${container.name}(_)).map($actionclassname(request, _))")
          }
      }
      case _ =>
        block(s"${paramtype.fullName}.createC(request.toRecord).") {
          println(s"map($actionclassname(request, _))")
        }
    }

    private def _action_call(op: SMethod): GenM[ActionCallDescriptor] = {
      val paramtypename = _param_type_fullname(op)
      val actionclassname = action_class_name(op)
      val actioncallclassname = action_call_class_name(op)
      for {
        _ <- separator
        _ <- block(s"abstract class ${actioncallclassname}() extends FunctionalActionCall") {
          // println("def execute(): Consequence[OperationResponse] = ???")
          println("")
        }
        _ <- block(s"object ${actioncallclassname}") {
          for {
            _ <- block("case class Instance(") {
              for {
                _ <- println(s"core: ActionCall.Core,")
                _ <- println(s"override val action: ${actionclassname}")
              } yield ()
            }
            _ <- block(s") extends ${actioncallclassname}") {
              block("protected def build_Program: ExecUowM[OperationResponse] =") {
                _action_program(op)
              }
            }
            _ <- separator
            _ <- block(s"def apply(") {
              for {
                _ <- println(s"core: ActionCall.Core,")
                _ <- println(s"action: ${actionclassname}")
              } yield ()
            }
            _ <- println(s"): ${actioncallclassname} = Instance(core, action)")
          } yield ()
        }
      } yield ActionCallDescriptor(actionclassname, actioncallclassname)
    }

    private def _param_type_fullname(op: SMethod): String = {
      val (paramname, paramclasstype) = _param_descriptor(op)
      _param_type_fullname(paramclasstype)
    }

    private def _param_type_fullname(paramtype: TypeName): String =
      paramtype match {
        case m if m.fullName == "org.goldenport.record.Record" => "Record"
        case m if m.isPlatform => paramtype.name
        case TypeName.Container(container, containee) => _param_type_fullname(containee)
        case m => paramtype.fullName
      }

    private def _action_program(op: SMethod): GenM[Unit] = {
      op.body match {
        case Some(s) => s()
        case None => println("uowmNotImplemented")
      }
    }

    private def _action_program_entity: GenM[Unit] = {
      ???
    }

    private def _action_program_entity_create: GenM[Unit] = {
      for {
        //                  _ <- println(s"given EntityPersistentCreate[${paramtypename}] = EntityPersistentCreate[${paramtypename}]")
        _ <- block("for") {
          println("r <- entity_create(action.entity)")
        }
        _ <- println("yield OperationResponse(r.toRecord)")
      } yield ()
    }
  }
  object ComponentProcessor {
    case class ActionCallDescriptorCollection(
      descriptors: Vector[ActionCallServiceDescriptor] = Vector.empty
    ) {
      def +(rhs: ActionCallDescriptorCollection) =
        copy(descriptors ++ rhs.descriptors)

      def setup: GenM[Unit] = descriptors.traverse(_.setup).void

      def define: GenM[Unit] = descriptors.traverse(_.define).void

      def methodsInServices: GenM[Unit] = descriptors.traverse(_.methodsInService).void
    }
    object ActionCallDescriptorCollection {
      val empty = new ActionCallDescriptorCollection(Vector.empty)

      implicit object ActionCallDescriptorCollectionMonoid extends Monoid[ActionCallDescriptorCollection] {
        def zero = empty
        def append(lhs: ActionCallDescriptorCollection, rhs: => ActionCallDescriptorCollection) = lhs + rhs
      }

      def apply(
        servicename: String,
        serviceclassname: String,
        xs: Seq[ActionCallDescriptor]
      ): ActionCallDescriptorCollection =
        ActionCallDescriptorCollection(
          Vector(
            ActionCallServiceDescriptor(servicename, serviceclassname, xs.toVector)
          )
        )

      def combineAll(xs: Seq[ActionCallDescriptorCollection]): ActionCallDescriptorCollection =
        xs.foldLeft(empty)(_ + _)
    }

    case class ActionCallServiceDescriptor(
      name: String,
      serviceClassName: String,
      actionCall: Vector[ActionCallDescriptor]
    ) {
      val factoryClassName = s"${serviceClassName}Factory"

      def setup: GenM[Unit] = println(s"val $name = $factoryClassName()")

      def define: GenM[Unit] = {
        block(s"class ${factoryClassName}()") {
          for {
            _ <- println(s"import ${serviceClassName}.*")
            _ <- actionCall.traverse(_.method).void
          } yield ()
        }
      }

      def methodsInService: GenM[Unit] = {
        block(s"object ${factoryClassName}") {
          for {
            _ <- println(s"import ${serviceClassName}.*")
            _ <- actionCall.traverse(_.method).void
          } yield ()
        }
      }
    }

    case class ActionCallDescriptor(
      actionClassName: String,
      actionCallClassName: String
    ) {
      def method: GenM[Unit] = {
        for {
          _ <- block(s"def create${actionCallClassName}(") {
            for {
              _ <- println("core: ActionCall.Core,")
              _ <- println(s"action: ${actionClassName}")
            } yield ()
          }
          _ <- block(s"): ${actionCallClassName} =") {
            println(s"${actionCallClassName}(core, action)")
          }
        } yield ()
      }
    }
  }
}

object ComponentPart {
}

/*
package com.example.sample

import org.goldenport.Consequence
import org.goldenport.protocol.Request
import org.goldenport.protocol.spec.*
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.cncf.component.Component
import org.goldenport.cncf.component.CollaboratorComponent
import org.goldenport.cncf.component.ComponentId
import org.goldenport.cncf.component.ComponentCreate
import org.goldenport.cncf.action.{Action, ActionCall, CollaboratorActionCall}
import org.goldenport.cncf.action.Query

final class SampleComponent() extends CollaboratorComponent {
}

object SampleCollaboratorComponent {
  val name = "sample"
  val componentId = ComponentId(name)

  class Factory extends Component.Factory {
    protected def create_Components(params: ComponentCreate): Vector[Component] =
      Vector(SampleComponent())

    protected def create_Core(
      params: ComponentCreate,
      comp: Component
    ): Component.Core = spec_create(
      name,
      componentId,
      MainService
    )
  }
}

object MainService extends ServiceDefinition {
  val specification = ServiceDefinition.Specification.Builder("main").
    operation(
      PingOperation
    ).build()
  
  object PingOperation extends OperationDefinition {
    val specification = OperationDefinition.Specification.Builder("ping").
      build()

    override def createOperationRequest(
      req: Request
    ): Consequence[PingQuery] =
      Consequence.success(PingQuery(req))
  }
}

final case class PingQuery(
  request: Request
) extends Query() {
  override def createCall(core: ActionCall.Core): ActionCall = {
    val ccore = core.createCollaboratorActionCallCore("ping")
    PingActionCall(core, ccore, this)
  }
}

final case class PingActionCall(
  core: ActionCall.Core,
  collaboratorCore: CollaboratorActionCall.Core,
  query: PingQuery,
) extends CollaboratorActionCall {
}
 */ 
