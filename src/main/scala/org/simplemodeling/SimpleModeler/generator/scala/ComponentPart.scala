package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz._
import model._
import Generator.{State => GState, _}
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM

/*
 * @since   Feb. 12, 2026
 *  version Feb. 27, 2026
 * @version Apr. 17, 2026
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
    _component match {
      case Some(s) =>
        for {
          _ <- println("// StateMachine transition rule provider")
          _ <- println("protected def stateMachineGuardResolver: GuardBindingResolver[Any, TransitionEvent] = new GuardBindingResolver[Any, TransitionEvent] {")
          _ <- indent
          _ <- println("def resolve(name: String): Consequence[Guard[Any, TransitionEvent]] =")
          _ <- indent
          _ <- println("Consequence.failure(s\"Missing state machine guard binding: $name\")")
          _ <- outdent
          _ <- outdent
          _ <- println("}")
          _ <- _state_machine_rules_method(s.stateMachineTransitionRules)
          _ <- _state_machine_definitions_method(s.stateMachineDefinitions)
          _ <- _event_reception_definitions_method(s.eventReceptionDefinitions)
          _ <- _event_routing_definitions_method(s.eventRoutingDefinitions)
          _ <- _event_subscription_definitions_method(s.eventSubscriptionDefinitions)
          _ <- _component_descriptors_method(s)
          _ <- _aggregate_definitions_method(s.aggregateDefinitions)
          _ <- _view_definitions_method(s.viewDefinitions)
          _ <- _operation_definitions_method(s.operationDefinitions)
          _ <- _component_definitions_method(s.componentDefinitions)
          _ <- _subsystem_definitions_method(s.subsystemDefinitions)
        } yield ()
      case None =>
        unit
    }
  }

  private def _component_descriptors_method(
    s: SComponent
  ): GenM[Unit] = {
    val defs = s.entityRuntimeDescriptors
    if (defs.isEmpty) {
      println("override def componentDescriptors: Vector[org.goldenport.cncf.component.ComponentDescriptor] = Vector.empty")
    } else {
      for {
        _ <- println("override def componentDescriptors: Vector[org.goldenport.cncf.component.ComponentDescriptor] = Vector(")
        _ <- indent
        _ <- println("org.goldenport.cncf.component.ComponentDescriptor(")
        _ <- indent
        _ <- println(s"name = Some(${_string_literal(s.componentName)}),")
        _ <- println(s"componentName = Some(${_string_literal(s.componentName)}),")
        _ <- println("entityRuntimeDescriptors = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _entity_runtime_descriptor_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
        _ <- outdent
        _ <- println(")")
        _ <- outdent
        _ <- println(")")
      } yield ()
    }
  }

  private def _entity_runtime_descriptor_expr(
    d: SComponent.EntityRuntimeDescriptor
  ): GenM[Unit] = {
    val usageKind = _string_literal(d.usageKind.getOrElse(""))
    val operationKind = _string_literal(d.operationKind.getOrElse(""))
    val applicationDomain = _string_literal(d.applicationDomain.getOrElse(""))
    for {
      _ <- println("org.goldenport.cncf.entity.runtime.EntityRuntimeDescriptor(")
      _ <- indent
      _ <- println(s"entityName = ${_string_literal(d.entityName)},")
      _ <- println(s"collectionId = ${d.entityObjectName}.collectionId,")
      _ <- println("memoryPolicy = org.goldenport.cncf.entity.runtime.EntityMemoryPolicy.LoadToMemory,")
      _ <- println("partitionStrategy = org.goldenport.cncf.entity.runtime.PartitionStrategy.byOrganizationMonthUTC,")
      _ <- println("maxPartitions = 64,")
      _ <- println("maxEntitiesPerPartition = 10000,")
      _ <- println(s"usageKind = org.goldenport.cncf.security.EntityUsageKind.parse(${usageKind}),")
      _ <- println(s"operationKind = org.goldenport.cncf.security.EntityOperationKind.parse(${operationKind}),")
      _ <- println(s"applicationDomain = org.goldenport.cncf.security.EntityApplicationDomain.parse(${applicationDomain}),")
      _ <- println(s"viewNames = ${_string_vector_expr(d.viewNames)}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _state_machine_rules_method(
    rules: Vector[SComponent.StateMachineTransitionRule]
  ): GenM[Unit] =
    if (rules.isEmpty) {
      println("override def stateMachineTransitionRules: Vector[CollectionTransitionRule[Any]] = Vector.empty")
    } else {
      for {
        _ <- println("override def stateMachineTransitionRules: Vector[CollectionTransitionRule[Any]] = Vector(")
        _ <- indent
        _ <- rules.zipWithIndex.foldLeft(unit) { case (z, (r, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _state_machine_rule_expr(r)
              _ <- if (i < rules.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _event_reception_definitions_method(
    defs: Vector[SComponent.EventReceptionDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def eventReceptionDefinitions: Vector[org.goldenport.cncf.event.CmlEventDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def eventReceptionDefinitions: Vector[org.goldenport.cncf.event.CmlEventDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _event_reception_definition_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _state_machine_definitions_method(
    defs: Vector[SComponent.StateMachineDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def stateMachineDefinitions: Vector[org.goldenport.cncf.statemachine.CmlStateMachineDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def stateMachineDefinitions: Vector[org.goldenport.cncf.statemachine.CmlStateMachineDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _state_machine_definition_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _state_machine_definition_expr(
    p: SComponent.StateMachineDefinition
  ): GenM[Unit] =
    for {
      _ <- println("org.goldenport.cncf.statemachine.CmlStateMachineDefinition(")
      _ <- indent
      _ <- println(s"name = ${_string_literal(p.name)},")
      _ <- println(s"states = ${_string_vector_expr(p.states)},")
      _ <- println(s"events = ${_string_vector_expr(p.events)}")
      _ <- outdent
      _ <- println(")")
    } yield ()

  private def _event_routing_definitions_method(
    defs: Vector[SComponent.EventRoutingDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def eventRoutingDefinitions: Vector[org.goldenport.cncf.event.CmlRoutingDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def eventRoutingDefinitions: Vector[org.goldenport.cncf.event.CmlRoutingDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _event_routing_definition_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _event_subscription_definitions_method(
    defs: Vector[SComponent.EventSubscriptionDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def eventSubscriptionDefinitions: Vector[org.goldenport.cncf.event.CmlSubscriptionDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def eventSubscriptionDefinitions: Vector[org.goldenport.cncf.event.CmlSubscriptionDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _event_subscription_definition_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _event_reception_definition_expr(
    p: SComponent.EventReceptionDefinition
  ): GenM[Unit] = {
    val category = _event_category_expr(p.category)
    val kind = p.kind.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val actionname = p.actionName.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val selectors =
      if (p.selectors.isEmpty)
        "Map.empty"
      else
        p.selectors.toVector.map { case (k, v) =>
          s"${_string_literal(k)} -> ${_string_literal(v)}"
        }.mkString("Map(", ", ", ")")
    for {
      _ <- println("org.goldenport.cncf.event.CmlEventDefinition(")
      _ <- indent
      _ <- println(s"name = ${_string_literal(p.name)},")
      _ <- println(s"category = ${category},")
      _ <- println(s"kind = ${kind},")
      _ <- println(s"selectors = ${selectors},")
      _ <- println(s"actionName = ${actionname},")
      _ <- println(s"priority = ${p.priority}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _event_routing_definition_expr(
    p: SComponent.EventRoutingDefinition
  ): GenM[Unit] = {
    val whenexpr = p.when.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val topic = p.topic.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val service = p.service.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val partition = p.partition.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    for {
      _ <- println("org.goldenport.cncf.event.CmlRoutingDefinition(")
      _ <- indent
      _ <- println(s"name = ${_string_literal(p.name)},")
      _ <- println(s"when = ${whenexpr},")
      _ <- println(s"topic = ${topic},")
      _ <- println(s"service = ${service},")
      _ <- println(s"partition = ${partition}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _event_subscription_definition_expr(
    p: SComponent.EventSubscriptionDefinition
  ): GenM[Unit] = {
    val route = _dispatch_route_expr(p.route)
    val entity = p.entityName.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val target = p.target.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val targets =
      if (p.targets.isEmpty)
        "Vector.empty"
      else
        p.targets.map(_string_literal).mkString("Vector(", ", ", ")")
    val selector = p.selector.map(x => s"Some(${_string_literal(x)})").getOrElse("None")
    val activation = _activation_expr(p.activation)
    for {
      _ <- println("org.goldenport.cncf.event.CmlSubscriptionDefinition(")
      _ <- indent
      _ <- println(s"name = ${_string_literal(p.name)},")
      _ <- println(s"eventName = ${_string_literal(p.eventName)},")
      _ <- println(s"route = ${route},")
      _ <- println(s"entityName = ${entity},")
      _ <- println(s"target = ${target},")
      _ <- println(s"targets = ${targets},")
      _ <- println(s"selector = ${selector},")
      _ <- println(s"actionName = ${_string_literal(p.actionName)},")
      _ <- println(s"declaredTargetUpperBound = ${p.declaredTargetUpperBound},")
      _ <- println(s"activation = ${activation}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _aggregate_definitions_method(
    defs: Vector[SComponent.AggregateDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def aggregateDefinitions: Vector[org.goldenport.cncf.entity.aggregate.AggregateDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def aggregateDefinitions: Vector[org.goldenport.cncf.entity.aggregate.AggregateDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"entityName = ${_string_literal(d.entityName)},")
              _ <- _aggregate_members("members", d.members)
              _ <- println(",")
              _ <- _aggregate_creates("creates", d.creates)
              _ <- println(",")
              _ <- _aggregate_commands("commands", d.commands)
              _ <- println(",")
              _ <- _aggregate_state("state", d.state)
              _ <- println(",")
              _ <- _aggregate_invariants("invariants", d.invariants)
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _aggregate_members(
    label: String,
    defs: Vector[SComponent.AggregateMemberDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty)
      println(s"$label = Vector.empty")
    else
      for {
        _ <- println(s"$label = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateMemberDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"entityName = ${_string_literal(d.entityName)},")
              _ <- println(s"kind = ${_option_string_literal(d.kind)},")
              _ <- println(s"boundary = ${_option_string_literal(d.boundary)},")
              _ <- println(s"join = ${_option_string_literal(d.join)},")
              _ <- println(s"joinFieldName = ${_option_string_literal(d.joinFieldName)},")
              _ <- println(s"multiplicity = ${_option_string_literal(d.multiplicity)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()

  private def _aggregate_creates(
    label: String,
    defs: Vector[SComponent.AggregateCreateDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty)
      println(s"$label = Vector.empty")
    else
      for {
        _ <- println(s"$label = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateCreateDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"input = ${_string_map_literal(d.input)},")
              _ <- println(s"validations = ${_string_vector_literal(d.validations)},")
              _ <- println(s"events = ${_string_vector_literal(d.events)},")
              _ <- println(s"initialState = ${_option_string_literal(d.initialState)},")
              _ <- println(s"implementation = ${_option_string_literal(d.implementation)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()

  private def _aggregate_commands(
    label: String,
    defs: Vector[SComponent.AggregateCommandDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty)
      println(s"$label = Vector.empty")
    else
      for {
        _ <- println(s"$label = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateCommandDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"input = ${_string_map_literal(d.input)},")
              _ <- println(s"validations = ${_string_vector_literal(d.validations)},")
              _ <- println(s"events = ${_string_vector_literal(d.events)},")
              _ <- println(s"newState = ${_option_string_literal(d.newState)},")
              _ <- println(s"implementation = ${_option_string_literal(d.implementation)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()

  private def _aggregate_state(
    label: String,
    defs: Vector[SComponent.AggregateStateDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty)
      println(s"$label = Vector.empty")
    else
      for {
        _ <- println(s"$label = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateStateDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"datatype = ${_option_string_literal(d.datatype)},")
              _ <- println(s"multiplicity = ${_option_string_literal(d.multiplicity)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()

  private def _aggregate_invariants(
    label: String,
    defs: Vector[SComponent.AggregateInvariantDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty)
      println(s"$label = Vector.empty")
    else
      for {
        _ <- println(s"$label = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.aggregate.AggregateInvariantDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"expression = ${_option_string_literal(d.expression)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()

  private def _view_query_definitions_expr(
    defs: Vector[SComponent.ViewQueryDefinition]
  ): String =
    if (defs.isEmpty)
      "Vector.empty"
    else
      defs.map { d =>
        val expr = _option_string_literal(d.expression)
        s"org.goldenport.cncf.entity.view.ViewQueryDefinition(name = ${_string_literal(d.name)}, expression = ${expr})"
      }.mkString("Vector(", ", ", ")")

  private def _view_definitions_method(
    defs: Vector[SComponent.ViewDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def viewDefinitions: Vector[org.goldenport.cncf.entity.view.ViewDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def viewDefinitions: Vector[org.goldenport.cncf.entity.view.ViewDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- println("org.goldenport.cncf.entity.view.ViewDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"entityName = ${_string_literal(d.entityName)},")
              _ <- println(s"viewNames = ${_string_vector_expr(d.viewNames)},")
              _ <- println(s"queries = ${_view_query_definitions_expr(d.queries)},")
              _ <- println(s"sourceEvents = ${_string_vector_expr(d.sourceEvents)},")
              _ <- println(s"rebuildable = ${_option_boolean_expr(d.rebuildable)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _operation_definitions_method(
    defs: Vector[SComponent.OperationDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def operationDefinitions: Vector[org.goldenport.cncf.operation.CmlOperationDefinition] = Vector.empty")
    } else {
      for {
        _ <- println("override def operationDefinitions: Vector[org.goldenport.cncf.operation.CmlOperationDefinition] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            val summary = d.summary.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val execution = d.execution.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val implementation = d.implementation.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val inputSummary = d.inputSummary.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val inputDescription = d.inputDescription.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val outputSummary = d.outputSummary.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val outputDescription = d.outputDescription.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val access = d.access.map { a =>
              val resource = a.resource.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val target = a.target.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val mode = a.mode.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val relation = a.relation.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val operationModel = a.operationModel.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val entityUsage = a.entityUsage.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val entityOperationKind = a.entityOperationKind.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val entityApplicationDomain = a.entityApplicationDomain.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              val condition = a.condition.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
              s"""Some(org.goldenport.cncf.operation.CmlOperationAccess(policy = ${_string_literal(a.policy)}, resource = ${resource}, target = ${target}, mode = ${mode}, relation = ${relation}, operationModel = ${operationModel}, entityUsage = ${entityUsage}, entityOperationKind = ${entityOperationKind}, entityApplicationDomain = ${entityApplicationDomain}, condition = ${condition}))"""
            }.getOrElse("None")
            val entityName = d.entityName.map(_string_literal).map(x => s"Some($x)").getOrElse("None")
            val entityNames = d.entityNames.map(_string_literal).mkString("Vector(", ", ", ")")
            for {
              _ <- println("org.goldenport.cncf.operation.CmlOperationDefinition(")
              _ <- indent
              _ <- println(s"name = ${_string_literal(d.name)},")
              _ <- println(s"kind = ${_string_literal(d.kind)},")
              _ <- println(s"summary = ${summary},")
              _ <- println(s"execution = ${execution},")
              _ <- println(s"implementation = ${implementation},")
              _ <- println(s"entityName = ${entityName},")
              _ <- println(s"entityNames = ${entityNames},")
              _ <- println(s"inputType = ${_string_literal(d.inputType)},")
              _ <- println(s"inputSummary = ${inputSummary},")
              _ <- println(s"inputDescription = ${inputDescription},")
              _ <- println(s"outputType = ${_string_literal(d.outputType)},")
              _ <- println(s"outputSummary = ${outputSummary},")
              _ <- println(s"outputDescription = ${outputDescription},")
              _ <- println(s"inputValueKind = ${_string_literal(d.inputValueKind)},")
              _ <- println(s"access = ${access},")
              _ <- println(s"parameters = ${_operation_fields_expr(d.parameters)}")
              _ <- outdent
              _ <- println(")")
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _component_definitions_method(
    defs: Vector[SComponent.ComponentDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def componentDefinitionRecords: Vector[Record] = Vector.empty")
    } else {
      for {
        _ <- println("override def componentDefinitionRecords: Vector[Record] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _component_definition_record_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _subsystem_definitions_method(
    defs: Vector[SComponent.SubsystemDefinition]
  ): GenM[Unit] =
    if (defs.isEmpty) {
      println("override def subsystemDefinitionRecords: Vector[Record] = Vector.empty")
    } else {
      for {
        _ <- println("override def subsystemDefinitionRecords: Vector[Record] = Vector(")
        _ <- indent
        _ <- defs.zipWithIndex.foldLeft(unit) { case (z, (d, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _subsystem_definition_record_expr(d)
              _ <- if (i < defs.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(")")
      } yield ()
    }

  private def _component_definition_record_expr(
    p: SComponent.ComponentDefinition
  ): GenM[Unit] = {
    val coordinates = _string_vector_expr(p.coordinates.map(_.asString))
    val componentlets = _string_vector_expr(p.componentlets)
    val extensionpoints = _string_vector_expr(p.extensionPoints)
    val domainvisions = _vision_vector_expr(p.domainVisions)
    val domaincapabilities = _capability_vector_expr(p.domainCapabilities)
    val domainqualities = _quality_vector_expr(p.domainQualities)
    val domainconstraints = _constraint_vector_expr(p.domainConstraints)
    val domainusecases = _use_case_vector_expr(p.domainUseCases)
    val usecases = _use_case_vector_expr(p.useCases)
    val extensionbindings =
      if (p.extensionBindings.isEmpty)
        "Record.empty"
      else
        "Record.data(" + p.extensionBindings.toVector.sortBy(_._1).map { case (k, v) =>
          s"${_string_literal(k)} -> ${_string_literal(v)}"
        }.mkString(", ") + ")"
    for {
      _ <- println("Record.data(")
      _ <- indent
      _ <- println(s"${_string_literal("name")} -> ${_string_literal(p.name)},")
      _ <- println(s"${_string_literal("coordinates")} -> ${coordinates},")
      _ <- println(s"${_string_literal("componentlets")} -> ${componentlets},")
      _ <- println(s"${_string_literal("extension_points")} -> ${extensionpoints},")
      _ <- println(s"${_string_literal("extension_bindings")} -> ${extensionbindings},")
      _ <- println(s"${_string_literal("domain_visions")} -> ${domainvisions},")
      _ <- println(s"${_string_literal("domain_capabilities")} -> ${domaincapabilities},")
      _ <- println(s"${_string_literal("domain_qualities")} -> ${domainqualities},")
      _ <- println(s"${_string_literal("domain_constraints")} -> ${domainconstraints},")
      _ <- println(s"${_string_literal("domain_use_cases")} -> ${domainusecases},")
      _ <- println(s"${_string_literal("use_cases")} -> ${usecases}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _subsystem_definition_record_expr(
    p: SComponent.SubsystemDefinition
  ): GenM[Unit] = {
    val components = _string_vector_expr(p.components.map(_.asString))
    val domainvisions = _vision_vector_expr(p.domainVisions)
    val domaincontexts = _context_vector_expr(p.domainContexts)
    val domainsystemcontexts = _system_context_vector_expr(p.domainSystemContexts)
    val domaincontextmaps = _context_map_vector_expr(p.domainContextMaps)
    val domaincapabilities = _capability_vector_expr(p.domainCapabilities)
    val domainqualities = _quality_vector_expr(p.domainQualities)
    val domainconstraints = _constraint_vector_expr(p.domainConstraints)
    val domainusecases = _use_case_vector_expr(p.domainUseCases)
    val extensionbindings =
      if (p.extensionBindings.isEmpty)
        "Record.empty"
      else
        "Record.data(" + p.extensionBindings.toVector.sortBy(_._1).map { case (k, v) =>
          s"${_string_literal(k)} -> ${_string_literal(v)}"
        }.mkString(", ") + ")"
    val config =
      if (p.config.isEmpty)
        "Record.empty"
      else
        "Record.data(" + p.config.toVector.sortBy(_._1).map { case (k, v) =>
          s"${_string_literal(k)} -> ${_string_literal(v)}"
        }.mkString(", ") + ")"
    for {
      _ <- println("Record.data(")
      _ <- indent
      _ <- println(s"${_string_literal("name")} -> ${_string_literal(p.name)},")
      _ <- println(s"${_string_literal("components")} -> ${components},")
      _ <- println(s"${_string_literal("extension_bindings")} -> ${extensionbindings},")
      _ <- println(s"${_string_literal("config")} -> ${config},")
      _ <- println(s"${_string_literal("domain_visions")} -> ${domainvisions},")
      _ <- println(s"${_string_literal("domain_contexts")} -> ${domaincontexts},")
      _ <- println(s"${_string_literal("domain_system_contexts")} -> ${domainsystemcontexts},")
      _ <- println(s"${_string_literal("domain_context_maps")} -> ${domaincontextmaps},")
      _ <- println(s"${_string_literal("domain_capabilities")} -> ${domaincapabilities},")
      _ <- println(s"${_string_literal("domain_qualities")} -> ${domainqualities},")
      _ <- println(s"${_string_literal("domain_constraints")} -> ${domainconstraints},")
      _ <- println(s"${_string_literal("domain_use_cases")} -> ${domainusecases}")
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _operation_fields_expr(
    p: Vector[SComponent.OperationField]
  ): String =
    if (p.isEmpty)
      "Vector.empty"
    else
      p.map { x =>
        s"""org.goldenport.cncf.operation.CmlOperationField(name = ${_string_literal(x.name)}, datatype = ${_string_literal(x.datatype)}, multiplicity = ${_string_literal(x.multiplicity)})"""
      }.mkString("Vector(", ", ", ")")

  private def _use_case_vector_expr(
    p: Vector[SComponent.UseCaseDefinition]
  ): String =
    if (p.isEmpty)
      "Vector.empty"
    else
      p.map(_use_case_record_expr).mkString("Vector(", ", ", ")")

  private def _use_case_record_expr(
    p: SComponent.UseCaseDefinition
  ): String = {
    val scenarios =
      if (p.scenarios.isEmpty)
        "Vector.empty"
      else
        p.scenarios.map(_use_case_scenario_record_expr).mkString("Vector(", ", ", ")")
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("actor")} -> ${_option_to_value_expr(p.actor)}, ${_string_literal("primary_actor")} -> ${_option_to_value_expr(p.primaryActor)}, ${_string_literal("secondary_actor")} -> ${_option_to_value_expr(p.secondaryActor)}, ${_string_literal("supporting_actor")} -> ${_option_to_value_expr(p.supportingActor)}, ${_string_literal("stakeholder")} -> ${_option_to_value_expr(p.stakeholder)}, ${_string_literal("goal")} -> ${_option_to_value_expr(p.goal)}, ${_string_literal("precondition")} -> ${_option_to_value_expr(p.precondition)}, ${_string_literal("postcondition")} -> ${_option_to_value_expr(p.postcondition)}, ${_string_literal("scenarios")} -> ${scenarios})"""
  }

  private def _capability_vector_expr(
    ps: Vector[SComponent.CapabilityDefinition]
  ): String =
    if (ps.isEmpty)
      "Vector.empty"
    else
      ps.map(_capability_record_expr).mkString("Vector(", ", ", ")")

  private def _capability_record_expr(
    p: SComponent.CapabilityDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("actor")} -> ${_option_to_value_expr(p.actor)}, ${_string_literal("primary_actor")} -> ${_option_to_value_expr(p.primaryActor)}, ${_string_literal("secondary_actor")} -> ${_option_to_value_expr(p.secondaryActor)}, ${_string_literal("supporting_actor")} -> ${_option_to_value_expr(p.supportingActor)}, ${_string_literal("stakeholder")} -> ${_option_to_value_expr(p.stakeholder)}, ${_string_literal("goal")} -> ${_option_to_value_expr(p.goal)}, ${_string_literal("precondition")} -> ${_option_to_value_expr(p.precondition)}, ${_string_literal("postcondition")} -> ${_option_to_value_expr(p.postcondition)})"""

  private def _vision_vector_expr(
    ps: Vector[SComponent.VisionDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_vision_record_expr).mkString("Vector(", ", ", ")")

  private def _vision_record_expr(
    p: SComponent.VisionDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("goal")} -> ${_option_to_value_expr(p.goal)}, ${_string_literal("precondition")} -> ${_option_to_value_expr(p.precondition)}, ${_string_literal("postcondition")} -> ${_option_to_value_expr(p.postcondition)})"""

  private def _context_vector_expr(
    ps: Vector[SComponent.ContextDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_context_record_expr).mkString("Vector(", ", ", ")")

  private def _context_record_expr(
    p: SComponent.ContextDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)})"""

  private def _system_context_vector_expr(
    ps: Vector[SComponent.SystemContextDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_system_context_record_expr).mkString("Vector(", ", ", ")")

  private def _system_context_record_expr(
    p: SComponent.SystemContextDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)})"""

  private def _context_map_vector_expr(
    ps: Vector[SComponent.ContextMapDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_context_map_record_expr).mkString("Vector(", ", ", ")")

  private def _context_map_record_expr(
    p: SComponent.ContextMapDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)})"""

  private def _quality_vector_expr(
    ps: Vector[SComponent.QualityDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_quality_record_expr).mkString("Vector(", ", ", ")")

  private def _quality_record_expr(
    p: SComponent.QualityDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("goal")} -> ${_option_to_value_expr(p.goal)}, ${_string_literal("precondition")} -> ${_option_to_value_expr(p.precondition)}, ${_string_literal("postcondition")} -> ${_option_to_value_expr(p.postcondition)})"""

  private def _constraint_vector_expr(
    ps: Vector[SComponent.ConstraintDefinition]
  ): String =
    if (ps.isEmpty) "Vector.empty" else ps.map(_constraint_record_expr).mkString("Vector(", ", ", ")")

  private def _constraint_record_expr(
    p: SComponent.ConstraintDefinition
  ): String =
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("goal")} -> ${_option_to_value_expr(p.goal)}, ${_string_literal("precondition")} -> ${_option_to_value_expr(p.precondition)}, ${_string_literal("postcondition")} -> ${_option_to_value_expr(p.postcondition)})"""

  private def _use_case_scenario_record_expr(
    p: SComponent.UseCaseScenario
  ): String = {
    val steps = _string_vector_expr(p.steps)
    val alternates = _string_vector_expr(p.alternates)
    val exceptions = _string_vector_expr(p.exceptions)
    s"""Record.data(${_string_literal("name")} -> ${_string_literal(p.name)}, ${_string_literal("summary")} -> ${_option_to_value_expr(p.summary)}, ${_string_literal("description")} -> ${_option_to_value_expr(p.description)}, ${_string_literal("steps")} -> ${steps}, ${_string_literal("alternates")} -> ${alternates}, ${_string_literal("exceptions")} -> ${exceptions})"""
  }

  private def _string_vector_expr(
    p: Vector[String]
  ): String =
    if (p.isEmpty)
      "Vector.empty"
    else
      p.map(_string_literal).mkString("Vector(", ", ", ")")

  private def _event_category_expr(
    p: String
  ): String =
    p match {
      case "ActionEvent" => "org.goldenport.cncf.event.CmlEventCategory.ActionEvent"
      case _ => "org.goldenport.cncf.event.CmlEventCategory.NonActionEvent"
    }

  private def _dispatch_route_expr(
    p: String
  ): String =
    p.toLowerCase match {
      case "multicast" => "org.goldenport.cncf.event.DispatchRoute.Multicast"
      case "broadcast" => "org.goldenport.cncf.event.DispatchRoute.Broadcast"
      case _ => "org.goldenport.cncf.event.DispatchRoute.Unicast"
    }

  private def _activation_expr(
    p: Option[String]
  ): String =
    p.map(_.toLowerCase) match {
      case Some("keepresident") =>
        "Some(org.goldenport.cncf.event.EntityActivationMode.KeepResident)"
      case Some("activateonreceive") =>
        "Some(org.goldenport.cncf.event.EntityActivationMode.ActivateOnReceive)"
      case _ =>
        "None"
    }

  private def _state_machine_rule_expr(
    p: SComponent.StateMachineTransitionRule
  ): GenM[Unit] = {
    val f = p.trigger match {
      case SComponent.TransitionTrigger.Save => "saveRule"
      case SComponent.TransitionTrigger.Update => "updateRule"
    }
    for {
      _ <- println(s"StateMachineRuleBuilder.${f}[Any](")
      _ <- indent
      _ <- println(s"collectionName = ${_string_literal(p.collectionName)},")
      _ <- println(s"eventName = ${_string_literal(p.eventName)},")
      _ <- println(s"priority = ${p.priority},")
      _ <- println(s"declarationOrder = ${p.declarationOrder},")
      _ <- println(s"guard = ${_guard_expr(p.guard)},")
      _ <- _plan_expr(p.plan)
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _plan_expr(
    p: SComponent.RulePlan
  ): GenM[Unit] = {
    for {
      _ <- println("plan = StateMachineRuleBuilder.plan[Any](")
      _ <- indent
      _ <- _action_vector_expr("exit", p.exit)
      _ <- _transition_action_expr(p.transition)
      _ <- _action_vector_expr("entry", p.entry, isLast = true)
      _ <- outdent
      _ <- println(")")
    } yield ()
  }

  private def _action_vector_expr(
    label: String,
    p: Vector[SComponent.RuleAction],
    isLast: Boolean = false
  ): GenM[Unit] = {
    val suffix = if (isLast) "" else ","
    if (p.isEmpty)
      println(s"${label} = Vector.empty${suffix}")
    else {
      for {
        _ <- println(s"${label} = Vector(")
        _ <- indent
        _ <- p.zipWithIndex.foldLeft(unit) { case (z, (x, i)) =>
          z.flatMap { _ =>
            for {
              _ <- _action_expr(x)
              _ <- if (i < p.length - 1) println(",") else unit
            } yield ()
          }
        }
        _ <- outdent
        _ <- println(s")${suffix}")
      } yield ()
    }
  }

  private def _transition_action_expr(
    p: Option[SComponent.RuleAction]
  ): GenM[Unit] =
    p match {
      case Some(s) =>
        for {
          _ <- println("transition = Some(")
          _ <- indent
          _ <- _action_expr(s)
          _ <- outdent
          _ <- println("),")
        } yield ()
      case None =>
        println("transition = None,")
    }

  private def _action_expr(
    p: SComponent.RuleAction
  ): GenM[Unit] = {
    val script = _string_literal(p.script)
    for {
      _ <- println("StateMachineRuleBuilder.action[Any] { (state, event) =>")
      _ <- indent
      _ <- println("val _ = (state, event)")
      _ <- println(s"val _script = ${script}")
      _ <- println("Consequence.unit")
      _ <- outdent
      _ <- println("}")
    } yield ()
  }

  private def _guard_expr(
    p: Option[SComponent.RuleGuard]
  ): String =
    p match {
      case None => "None"
      case Some(SComponent.RuleGuard.Ref(name)) =>
        s"Some(StateMachineRuleBuilder.guardRef[Any](${_string_literal(name)}, stateMachineGuardResolver))"
      case Some(SComponent.RuleGuard.Expression(expr)) =>
        s"""Some(StateMachineRuleBuilder.guardExpression[Any](${_string_literal(expr)})((state, event) => Map("state" -> state, "event" -> event, "ctx" -> Map.empty[String, Any])))"""
    }

  private def _string_literal(p: String): String = {
    val escaped = _escape_string(Option(p).getOrElse(""))
    "\"" + escaped + "\""
  }

  private def _option_string_literal(p: Option[String]): String =
    p.map(x => s"Some(${_string_literal(x)})").getOrElse("None")

  private def _option_boolean_expr(p: Option[Boolean]): String =
    p.map(x => s"Some(${x.toString})").getOrElse("None")

  private def _option_to_value_expr(p: Option[String]): String =
    p.map(_string_literal).getOrElse("None")

  private def _string_vector_literal(p: Vector[String]): String =
    if (p.isEmpty)
      "Vector.empty"
    else
      p.map(_string_literal).mkString("Vector(", ", ", ")")

  private def _string_map_literal(p: Map[String, String]): String =
    if (p.isEmpty)
      "Map.empty"
    else
      p.iterator.map { case (k, v) =>
        s"${_string_literal(k)} -> ${_string_literal(v)}"
      }.mkString("Map(", ", ", ")")

  private def _escape_string(p: String): String =
    p.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    }

  private def _normalize_text(p: Option[String]): Option[String] =
    p.map(_.trim).filter(_.nonEmpty)

  private def _summary_text(p: Option[String]): Option[String] =
    _normalize_text(p).
      flatMap(x => _first_sentence(x).orElse(_first_non_empty_line(x))).
      filter(_.nonEmpty)

  private def _doc_method_name(name: String): String =
    s"doc${name.capitalize}"

  private def _first_non_empty_line(p: String): Option[String] =
    p.linesIterator.map(_.trim).find(_.nonEmpty)

  private def _first_sentence(p: String): Option[String] = {
    val s = p.trim
    val idx = s.indexWhere(_ == '.')
    if (idx >= 0)
      Some(s.substring(0, idx + 1).trim).filter(_.nonEmpty)
    else
      None
  }

  private def _comment_lines(p: Option[String]): Vector[String] =
    _normalize_text(p).toVector.flatMap(_.split("\r?\n").toVector.map(_.trim).filter(_.nonEmpty))

  private def _comment(p: Option[String]): GenM[Unit] =
    _comment_lines(p).traverse_(x => println(s"// $x"))

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
      val servicedefs = services.filter(_.methods.nonEmpty).map(service_object_name)
      for {
        _ <- _comment(component.description)
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
        _ <- _aggregate_factory_methods
        _ <- separator
        _ <- actioncalldescs.setup
        _ <- outdent
        _ <- println(s"}")
        _ <- actioncalldescs.define
      } yield ()
    }

    private def _aggregate_factory_methods: GenM[Unit] = {
      val defs = component.aggregateDefinitions.groupBy(_.entityName).values.toVector.map(_.head).sortBy(_.entityName)
      if (defs.isEmpty)
        unit
      else
        for {
          _ <- println("override def create_aggregate_from_record(")
          _ <- indent
          _ <- println("entityName: String,")
          _ <- println("record: Record,")
          _ <- println("default: => Consequence[Any]")
          _ <- outdent
          _ <- println("): Consequence[Any] = entityName match {")
          _ <- indent
          _ <- defs.foldLeft(unit) { (z, d) =>
            z.flatMap(_ => println(s"""case ${_string_literal(d.entityName)} => ${_aggregate_factory_method_name(d)}(record)"""))
          }
          _ <- println("case _ => default")
          _ <- outdent
          _ <- println("}")
          _ <- separator
          _ <- intercalateTraverse_(defs, separator)(d => _aggregate_factory_method(d))
        } yield ()
    }

    private def _aggregate_factory_method(
      d: SComponent.AggregateDefinition
    ): GenM[Unit] =
      println(s"def ${_aggregate_factory_method_name(d)}(record: Record): Consequence[Any] = ${_aggregate_class_name(d)}.createC(record)")

    private def _aggregate_factory_method_name(
      d: SComponent.AggregateDefinition
    ): String =
      s"create${_aggregate_entity_class_name(d.entityName)}Aggregate"

    private def _aggregate_class_name(
      d: SComponent.AggregateDefinition
    ): String =
      s"_root_.${component.packageName.name}.entity.aggregate.${_aggregate_entity_class_name(d.entityName)}"

    private def _aggregate_entity_class_name(
      name: String
    ): String =
      name.split("[^A-Za-z0-9]+").toVector.filter(_.nonEmpty).map(make_title).mkString

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
      services.filter(_.methods.nonEmpty).traverse(_service(_)).map(ActionCallDescriptorCollection.combineAll)

    private def _service(service: SService): GenM[ActionCallDescriptorCollection] = {
      val servicename = service_name(service)
      val serviceobjectname = service_object_name(service)
      val ops = service.methods.map(operation_object_name)
      for {
        _ <- _comment(service.description)
        ds <- blockR(s"object ${serviceobjectname} extends ServiceDefinition {") {
          for {
            _ <-
              if (service.useCases.isEmpty)
                println("def useCaseRecords: Vector[Record] = Vector.empty")
              else
                block("def useCaseRecords: Vector[Record] =") {
                  println(_use_case_vector_expr(service.useCases))
                }
            _ <- block(s"""val specification = ServiceDefinition.Specification.Builder("${servicename}").""") {
              for {
                _ <- ops.toList match {
                  case Nil => println("build()")
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
              } yield ()
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
      val outputtype = _operation_output_type(op)
      for {
        _ <- _comment(op.description)
        _ <- separator
        _ <- block(s"object ${operationobject} extends OperationDefinition") {
          for {
            _ <- block(s"""val specification = OperationDefinition.Specification.Builder("$operationname").""") {
              for {
                _ <- println("copy(")
                _ <- indent
                _ <- println(s"""response = ResponseDefinition(result = List(org.goldenport.schema.DataType.Named("${outputtype}")))""")
                _ <- outdent
                _ <- println(").build()")
              } yield ()
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
      val params = _param_descriptors(op)
      for {
        _ <- separator
        _ <- block(s"final case class ${actionclassname}(") {
          for {
            _ <- if (params.nonEmpty)
              println("request: Request,")
            else
              println("request: Request")
            _ <- params.zipWithIndex.traverse { case ((paramname, paramclasstype), i) =>
              val isLast = i == params.length - 1
              if (isLast)
                println(s"${paramname}: ${typename_relative_name(paramclasstype)}")
              else
                println(s"${paramname}: ${typename_relative_name(paramclasstype)},")
            }
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
        _ <- _action_companion_object(op, params)
      } yield ()
    }

    private def _param_descriptors(op: SMethod): Vector[(String, TypeName)] =
      op.parameters.parameters.headOption match {
        case Some(s) => s.value match {
          case Some(v) => _action_descriptors(v)
          case None => Vector((s.name.name, s.typeName))
        }
        case None => Vector(("p", TypeName.parse("Record"))) // TODO
      }

    private def _param_descriptor(op: SMethod): (String, TypeName) =
      _param_descriptors(op).head

    private def _action_descriptor(action: SClassBase): (String, TypeName) =
      _action_descriptors(action).head

    private def _action_descriptors(action: SClassBase): Vector[(String, TypeName)] =
      action.parameterSequence.parameters.headOption match {
        case Some(_) =>
          action.parameterSequence.parameters.map { s =>
            s.value match {
              case Some(v) => (s.name.name, TypeName.create(v))
              case None => (s.name.name, s.typeName)
            }
          }
        case None => Vector(("p", TypeName.create(action)))
      }

    private def _action_companion_object(
      op: SMethod,
      params: Vector[(String, TypeName)]
    ): GenM[Unit] = {
      val actionclassname = action_class_name(op)
      block(s"object $actionclassname") {
        block(s"def create(request: Request): Consequence[$actionclassname] =") {
          _action_companion_object_create_body(params, actionclassname)
        }
      }
    }

    private def _action_companion_object_create_body(
      params: Vector[(String, TypeName)],
      actionclassname: String
    ): GenM[Unit] = params match {
      case Vector() =>
        println(s"Consequence.success($actionclassname(request))")
      case Vector((paramname, paramtype)) =>
        block(s"${_action_companion_object_create_expr(paramname, paramtype)}.") {
          println(s"map($actionclassname(request, _))")
        }
      case xs =>
        for {
          _ <- println("for {")
          _ <- indent
          _ <- xs.traverse { case (name, tpe) =>
            println(s"$name <- ${_action_companion_object_create_expr(name, tpe)}")
          }.map(_ => ())
          _ <- outdent
          _ <- println(s"} yield $actionclassname(request, ${xs.map(_._1).mkString(", ")})")
        } yield ()
    }

    private def _action_companion_object_create_expr(
      paramname: String,
      paramtype: TypeName
    ): String = paramtype match {
      case m if m.fullName == "org.goldenport.record.Record" =>
        "Consequence.success(request.toRecord)"
      case m if m.fullName.startsWith("org.goldenport.cncf.directive.Query") =>
        "Consequence.success(org.goldenport.cncf.directive.Query.fromRecord(request.toRecord))"
      case m if m.isPlatform =>
        s"""Consequence.successOrRecordNotFound[${paramtype.name}]("${paramname}", request.toRecord)"""
      case TypeName.Container(container, containee) if container.name == "Option" && containee.isString =>
        s"""Consequence.success(request.toRecord.getString("${paramname}"))"""
      case TypeName.Container(container, containee) => containee match {
        case mm if mm.fullName == "org.goldenport.record.Record" =>
          s"Consequence.success(${container.name}(request.toRecord))"
        case mm if mm.isPlatform =>
          s"""Consequence.successOrRecordNotFound[${containee.name}]("${paramname}", request.toRecord).map(x => ${container.name}(x))"""
        case _ =>
          s"${containee.fullName}.createC(request.toRecord).map(${container.name}(_))"
      }
      case _ =>
        s"${paramtype.fullName}.createC(request.toRecord)"
    }

    private def _action_call(op: SMethod): GenM[ActionCallDescriptor] = {
      val paramtypename = _param_type_fullname(op)
      val actionclassname = action_class_name(op)
      val actioncallclassname = action_call_class_name(op)
      val access = op.access.orElse(_operation_access(op))
      for {
        _ <- separator
        _ <- block(s"abstract class ${actioncallclassname}() extends FunctionalActionCall") {
          access match {
            case Some(x) =>
              _action_call_authorize(op, x)
            case None =>
              println("")
          }
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

    private def _action_call_authorize(
      op: SMethod,
      access: SComponent.OperationAccess
    ): GenM[Unit] =
      access.policy.trim.toLowerCase(java.util.Locale.ROOT) match {
        case "owner_or_manager" | "owner-or-manager" =>
          println("")
        case _ =>
          println("")
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
        case None if _is_echo_record_command(op) =>
          println("ExecUowM.pure(OperationResponse.create(action.request.toRecord))")
        case None if _is_sync_by_design_command(op) =>
          println("ExecUowM.pure(OperationResponse.create(action.request.toRecord))")
        case None => println("uowmNotImplemented")
      }
    }

    private def _is_echo_record_command(op: SMethod): Boolean =
      op.descriptor.kind == SMethod.Kind.Command &&
        _component.exists(_.componentCore.operationDefinitions.exists { definition =>
          val opname = _normalize_operation_marker(op.name.name)
          val defname = _normalize_operation_marker(definition.name)
          opname == defname &&
          definition.implementation.exists(_.trim.equalsIgnoreCase("echo-record"))
        })

    private def _is_sync_by_design_command(op: SMethod): Boolean =
      op.descriptor.kind == SMethod.Kind.Command &&
        _component.exists(_.componentCore.operationDefinitions.exists { definition =>
          val opname = _normalize_operation_marker(op.name.name)
          val defname = _normalize_operation_marker(definition.name)
          opname == defname &&
          definition.execution.exists(_.trim.equalsIgnoreCase("sync"))
        })

    private def _normalize_operation_marker(name: String): String =
      Option(name).getOrElse("").toLowerCase.replaceAll("[^a-z0-9]", "") match {
        case s if s.endsWith("command") => s.stripSuffix("command")
        case s if s.endsWith("operation") => s.stripSuffix("operation")
        case s => s
      }

    private def _operation_access(op: SMethod): Option[SComponent.OperationAccess] = {
      val opmarkers = Set(
        _normalize_operation_marker(op.name.name),
        _normalize_operation_marker(operation_name(op))
      ).filter(_.nonEmpty)
      _component.flatMap(_.componentCore.operationDefinitions.find { definition =>
        val defname = _normalize_operation_marker(definition.name)
        opmarkers.contains(defname)
      }).flatMap(_.access)
    }

    private def _operation_output_type(op: SMethod): String = {
      val opmarkers = Set(
        _normalize_operation_marker(op.name.name),
        _normalize_operation_marker(operation_name(op))
      ).filter(_.nonEmpty)
      _component.flatMap(_.componentCore.operationDefinitions.find { definition =>
        val defname = _normalize_operation_marker(definition.name)
        opmarkers.contains(defname)
      }).map(_.outputType).getOrElse(op.returnType.name)
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
