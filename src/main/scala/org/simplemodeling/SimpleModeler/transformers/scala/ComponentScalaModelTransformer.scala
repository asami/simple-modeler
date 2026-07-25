package org.simplemodeling.SimpleModeler.transformers.scala

import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Feb. 11, 2026
 *  version Feb. 18, 2026
 *  version May. 22, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentScalaModelTransformer() extends ScalaModelTransformer() {
  protected def accept_purposes: Vector[Purpose] = Vector(Purpose.Plain)
  protected def is_accept_object(p: MObject): Boolean = p.isInstanceOf[MComponent]
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
      val entitydescs = source match {
        case m: MComponent.Core.Holder => _to_entity_runtime_descriptors(m.entityRuntimeDescriptors)
        case _ => Vector.empty
      }
      val rules = source match {
        case m: MComponent.Core.Holder => _to_transition_rules(m.stateMachineTransitionRules)
        case _ => Vector.empty
      }
      val statedefs = source match {
        case m: MComponent.Core.Holder => _to_state_machine_definitions(m.stateMachineDefinitions)
        case _ => Vector.empty
      }
      val eventdefs = source match {
        case m: MComponent.Core.Holder => _to_event_reception_definitions(m.eventReceptionDefinitions)
        case _ => Vector.empty
      }
      val eventroutes = source match {
        case m: MComponent.Core.Holder => _to_event_routing_definitions(m.eventRoutingDefinitions)
        case _ => Vector.empty
      }
      val eventsubs = source match {
        case m: MComponent.Core.Holder => _to_event_subscription_definitions(m.eventSubscriptionDefinitions)
        case _ => Vector.empty
      }
      val aggregates = source match {
        case m: MComponent.Core.Holder => _to_aggregate_definitions(m.aggregateDefinitions)
        case _ => Vector.empty
      }
      val views = source match {
        case m: MComponent.Core.Holder => _to_view_definitions(m.viewDefinitions)
        case _ => Vector.empty
      }
      val relationships = source match {
        case m: MComponent.Core.Holder => _to_relationship_definitions(m.relationshipDefinitions)
        case _ => Vector.empty
      }
      val operations = source match {
        case m: MComponent.Core.Holder => _to_operation_definitions(m.operationDefinitions)
        case _ => Vector.empty
      }
      val components = source match {
        case m: MComponent.Core.Holder => _to_component_definitions(m.componentDefinitions)
        case _ => Vector.empty
      }
      val subsystems = source match {
        case m: MComponent.Core.Holder => _to_subsystem_definitions(m.subsystemDefinitions)
        case _ => Vector.empty
      }
      val ccore = SComponent.ComponentCore(
        componentName,
        services,
        entitydescs,
        rules,
        statedefs,
        eventdefs,
        eventroutes,
        eventsubs,
        aggregates,
        views,
        relationships,
        operations,
        components,
        subsystems,
        description = _description_text(source)
      )
      SComponent(core, ccore)
    }

    private def _to_entity_runtime_descriptors(
      ps: Vector[MComponent.EntityRuntimeDescriptor]
    ): Vector[SComponent.EntityRuntimeDescriptor] =
      ps.map { p =>
        SComponent.EntityRuntimeDescriptor(
          entityName = p.entityName,
          packageName = PackageName(p.packageName),
          entityKind = p.entityKind,
          usageKind = p.usageKind,
          operationKind = p.operationKind,
          applicationDomain = p.applicationDomain,
          viewNames = p.viewNames,
          revisionModelKind = p.revisionModelKind,
          revisionRepresentation = p.revisionRepresentation
        )
      }

    private def _to_transition_rules(
      ps: Vector[MComponent.StateMachineTransitionRule]
    ): Vector[SComponent.StateMachineTransitionRule] =
      ps.map(_to_transition_rule)

    private def _to_transition_rule(
      p: MComponent.StateMachineTransitionRule
    ): SComponent.StateMachineTransitionRule =
      SComponent.StateMachineTransitionRule(
        collectionName = p.collectionName,
        trigger = _to_transition_trigger(p.trigger),
        eventName = p.eventName,
        machineName = p.machineName,
        stateFieldName = p.stateFieldName,
        fromState = p.fromState,
        fromStateValue = p.fromStateValue,
        toState = p.toState,
        toStateValue = p.toStateValue,
        priority = p.priority,
        declarationOrder = p.declarationOrder,
        guard = p.guard.map(_to_rule_guard),
        plan = _to_rule_plan(p.plan)
      )

    private def _to_state_machine_definitions(
      ps: Vector[MComponent.StateMachineDefinition]
    ): Vector[SComponent.StateMachineDefinition] =
      ps.map(_to_state_machine_definition)

    private def _to_state_machine_definition(
      p: MComponent.StateMachineDefinition
    ): SComponent.StateMachineDefinition =
      SComponent.StateMachineDefinition(
        name = p.name,
        states = p.states,
        events = p.events
      )

    private def _to_event_reception_definitions(
      ps: Vector[MComponent.EventReceptionDefinition]
    ): Vector[SComponent.EventReceptionDefinition] =
      ps.map(_to_event_reception_definition)

    private def _to_event_reception_definition(
      p: MComponent.EventReceptionDefinition
    ): SComponent.EventReceptionDefinition =
      SComponent.EventReceptionDefinition(
        name = p.name,
        category = p.category,
        kind = p.kind,
        selectors = p.selectors,
        actionName = p.actionName,
        priority = p.priority
      )

    private def _to_event_routing_definitions(
      ps: Vector[MComponent.EventRoutingDefinition]
    ): Vector[SComponent.EventRoutingDefinition] =
      ps.map { p =>
        SComponent.EventRoutingDefinition(
          name = p.name,
          when = p.when,
          topic = p.topic,
          service = p.service,
          partition = p.partition
        )
      }

    private def _to_event_subscription_definitions(
      ps: Vector[MComponent.EventSubscriptionDefinition]
    ): Vector[SComponent.EventSubscriptionDefinition] =
      ps.map { p =>
        SComponent.EventSubscriptionDefinition(
          name = p.name,
          eventName = p.eventName,
          route = p.route,
          entityName = p.entityName,
          target = p.target,
          targets = p.targets,
          selector = p.selector,
          actionName = p.actionName,
          declaredTargetUpperBound = p.declaredTargetUpperBound,
          activation = p.activation
        )
      }

    private def _to_aggregate_definitions(
      ps: Vector[MComponent.AggregateDefinition]
    ): Vector[SComponent.AggregateDefinition] =
      ps.map { p =>
        SComponent.AggregateDefinition(
          name = p.name,
          entityName = p.entityName,
          members = p.members.map { m =>
            SComponent.AggregateMemberDefinition(
              name = m.name,
              entityName = m.entityName,
              kind = m.kind,
              boundary = m.boundary,
              join = m.join,
              joinFieldName = m.joinFieldName,
              multiplicity = m.multiplicity
            )
          },
          creates = p.creates.map { c =>
            SComponent.AggregateCreateDefinition(
              name = c.name,
              input = c.input,
              validations = c.validations,
              events = c.events,
              initialState = c.initialState,
              implementation = c.implementation
            )
          },
          commands = p.commands.map { c =>
            SComponent.AggregateCommandDefinition(
              name = c.name,
              input = c.input,
              validations = c.validations,
              events = c.events,
              newState = c.newState,
              implementation = c.implementation
            )
          },
          state = p.state.map { s =>
            SComponent.AggregateStateDefinition(
              name = s.name,
              datatype = s.datatype,
              multiplicity = s.multiplicity
            )
          },
          invariants = p.invariants.map { i =>
            SComponent.AggregateInvariantDefinition(
              name = i.name,
              expression = i.expression
            )
          }
        )
      }

    private def _to_view_definitions(
      ps: Vector[MComponent.ViewDefinition]
    ): Vector[SComponent.ViewDefinition] =
      ps.map { p =>
        SComponent.ViewDefinition(
          name = p.name,
          entityName = p.entityName,
          viewNames = p.viewNames,
          viewFields = p.viewFields,
          queries = p.queries.map(q => SComponent.ViewQueryDefinition(q.name, q.expression)),
          sourceEvents = p.sourceEvents,
          rebuildable = p.rebuildable
        )
      }

    private def _to_operation_definitions(
      ps: Vector[MComponent.OperationDefinition]
    ): Vector[SComponent.OperationDefinition] =
      ps.map { p =>
        SComponent.OperationDefinition(
          name = p.name,
          kind = p.kind,
          summary = p.summary,
          execution = p.execution,
          commandKind = p.commandKind,
          commandExecutionProperties = p.commandExecutionProperties,
          commandExecutionPolicy = p.commandExecutionPolicy,
          implementation = p.implementation,
          entityName = p.entityName,
          entityNames = p.entityNames,
          inputType = p.inputType,
          inputSummary = p.inputSummary,
          inputDescription = p.inputDescription,
          outputType = p.outputType,
          outputSummary = p.outputSummary,
          outputDescription = p.outputDescription,
          inputValueKind = p.inputValueKind,
          visibility = p.visibility,
          access = p.access.map(a =>
            SComponent.OperationAccess(
              policy = a.policy,
              resource = a.resource,
              target = a.target,
              mode = a.mode,
              relation = a.relation,
              operationModel = a.operationModel,
              entityUsage = a.entityUsage,
              entityOperationKind = a.entityOperationKind,
              entityApplicationDomain = a.entityApplicationDomain,
              condition = a.condition
            )
          ),
          operationAuthorization = p.operationAuthorization.map(a =>
            SComponent.OperationAuthorization(
              operationModes = a.operationModes,
              allowAnonymous = a.allowAnonymous,
              anonymousOperationModes = a.anonymousOperationModes
            )
          ),
          evaluation = p.evaluation.map { e =>
            SComponent.OperationEvaluation(
              corpus = e.corpus.map { c =>
                SComponent.CorpusOperationEvaluation(
                  capture = c.capture,
                  profile = c.profile,
                  admission = c.admission,
                  outcomes = c.outcomes,
                  sampling = c.sampling,
                  redaction = c.redaction
                )
              },
              experiment = e.experiment.map { x =>
                SComponent.ExperimentOperationEvaluation(
                  eligible = x.eligible,
                  purpose = x.purpose,
                  admission = x.admission,
                  variantProfile = x.variantProfile
                )
              }
            )
          },
          childEntityBindings = p.childEntityBindings.map { x =>
            SComponent.OperationChildEntityBinding(
              name = x.name,
              entityName = x.entityName,
              inputParameter = x.inputParameter,
              parentIdField = x.parentIdField,
              relationshipName = x.relationshipName,
              sourceEntityIdMode = x.sourceEntityIdMode,
              sourceEntityIdParameters = x.sourceEntityIdParameters,
              sourceEntityIdResultFields = x.sourceEntityIdResultFields,
              childIdField = x.childIdField,
              sortOrderField = x.sortOrderField,
              createsEntity = x.createsEntity,
              failurePolicy = x.failurePolicy
            )
          },
          associationBinding = p.associationBinding.map { x =>
            SComponent.OperationAssociationBinding(
              domain = x.domain,
              targetKind = x.targetKind,
              createsAssociation = x.createsAssociation,
              detachesAssociation = x.detachesAssociation,
              roles = x.roles,
              parameters = x.parameters,
              sourceEntityIdMode = x.sourceEntityIdMode,
              sourceEntityIdParameters = x.sourceEntityIdParameters,
              sourceEntityIdResultFields = x.sourceEntityIdResultFields,
              targetIdParameters = x.targetIdParameters,
              sortOrderParameters = x.sortOrderParameters
            )
          },
          parameters = p.parameters.map { x =>
            SComponent.OperationField(
              name = x.name,
              datatype = x.datatype,
              multiplicity = x.multiplicity,
              label = x.label,
              controlType = x.controlType,
              placeholder = x.placeholder,
              help = x.help,
              required = x.required,
              confidentiality = x.confidentiality,
              constraints = to_constraints(x.constraints),
              typeConstraints = to_constraints(x.typeConstraints),
              update = x.update.map { update =>
                SComponent.OperationUpdateField(
                  sourceMultiplicity = update.sourceMultiplicity,
                  nullAllowed = update.nullAllowed
                )
              }
            )
          },
          resultFields = p.resultFields.map(_to_operation_field)
        )
      }

    private def _to_operation_field(
      p: MComponent.OperationField
    ): SComponent.OperationField =
      SComponent.OperationField(
        name = p.name,
        datatype = p.datatype,
        multiplicity = p.multiplicity,
        label = p.label,
        controlType = p.controlType,
        placeholder = p.placeholder,
        help = p.help,
        required = p.required,
        confidentiality = p.confidentiality,
        constraints = to_constraints(p.constraints),
        typeConstraints = to_constraints(p.typeConstraints),
        update = p.update.map { update =>
          SComponent.OperationUpdateField(
            sourceMultiplicity = update.sourceMultiplicity,
            nullAllowed = update.nullAllowed
          )
        }
      )

    private def _to_relationship_definitions(
      ps: Vector[MComponent.RelationshipDefinition]
    ): Vector[SComponent.RelationshipDefinition] =
      ps.map { p =>
        SComponent.RelationshipDefinition(
          name = p.name,
          kind = p.kind,
          sourceEntityName = p.sourceEntityName,
          targetEntityName = p.targetEntityName,
          targetModelKind = p.targetModelKind,
          sourceRole = p.sourceRole,
          targetRole = p.targetRole,
          multiplicity = p.multiplicity,
          storageMode = p.storageMode,
          parentIdField = p.parentIdField,
          valueField = p.valueField,
          sortOrderField = p.sortOrderField,
          associationDomain = p.associationDomain,
          targetKind = p.targetKind,
          lifecyclePolicy = p.lifecyclePolicy
        )
      }

    private def _to_component_definitions(
      ps: Vector[MComponent.ComponentDefinition]
    ): Vector[SComponent.ComponentDefinition] =
      ps.map { p =>
        SComponent.ComponentDefinition(
          name = p.name,
          actors = p.actors.map(_to_actor_definition),
          coordinates = p.coordinates.map { c =>
            SComponent.ComponentCoordinate(
              group = c.group,
              artifact = c.artifact,
              version = c.version
            )
          },
          componentlets = p.componentlets,
          extensionPoints = p.extensionPoints,
          extensionBindings = p.extensionBindings,
          domainVisions = p.domainVisions.map { v =>
            SComponent.VisionDefinition(
              name = v.name,
              summary = v.summary,
              description = v.description,
              goal = v.goal,
              precondition = v.precondition,
              postcondition = v.postcondition
            )
          },
          domainCapabilities = p.domainCapabilities.map { c =>
            SComponent.CapabilityDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description,
              actor = c.actor,
              primaryActor = c.primaryActor,
              secondaryActor = c.secondaryActor,
              supportingActor = c.supportingActor,
              stakeholder = c.stakeholder,
              goal = c.goal,
              precondition = c.precondition,
              postcondition = c.postcondition
            )
          },
          domainQualities = p.domainQualities.map { q =>
            SComponent.QualityDefinition(
              name = q.name,
              summary = q.summary,
              description = q.description,
              goal = q.goal,
              precondition = q.precondition,
              postcondition = q.postcondition
            )
          },
          domainConstraints = p.domainConstraints.map { c =>
            SComponent.ConstraintDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description,
              goal = c.goal,
              precondition = c.precondition,
              postcondition = c.postcondition
            )
          },
          domainUseCases = p.domainUseCases.map(_to_use_case_definition),
          useCases = p.useCases.map(_to_use_case_definition),
          services = p.services.map { service =>
            SComponent.ComponentServiceDefinition(
              name = service.name,
              spiStandard = service.spiStandard,
              spiDirection = service.spiDirection,
              spiSocket = service.spiSocket,
              spiMultiplicity = service.spiMultiplicity,
              spiRequired = service.spiRequired,
              spiApiName = service.spiApiName,
              spiComponentApi = service.spiComponentApi
            )
          }
        )
      }

    private def _to_subsystem_definitions(
      ps: Vector[MComponent.SubsystemDefinition]
    ): Vector[SComponent.SubsystemDefinition] =
      ps.map { p =>
        SComponent.SubsystemDefinition(
          name = p.name,
          components = p.components.map { c =>
            SComponent.ComponentCoordinate(
              group = c.group,
              artifact = c.artifact,
              version = c.version
            )
          },
          extensionBindings = p.extensionBindings,
          config = p.config,
          domainVisions = p.domainVisions.map { v =>
            SComponent.VisionDefinition(
              name = v.name,
              summary = v.summary,
              description = v.description,
              goal = v.goal,
              precondition = v.precondition,
              postcondition = v.postcondition
            )
          },
          domainContexts = p.domainContexts.map { c =>
            SComponent.ContextDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description
            )
          },
          domainSystemContexts = p.domainSystemContexts.map { c =>
            SComponent.SystemContextDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description
            )
          },
          domainContextMaps = p.domainContextMaps.map { c =>
            SComponent.ContextMapDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description
            )
          },
          domainCapabilities = p.domainCapabilities.map { c =>
            SComponent.CapabilityDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description,
              actor = c.actor,
              primaryActor = c.primaryActor,
              secondaryActor = c.secondaryActor,
              supportingActor = c.supportingActor,
              stakeholder = c.stakeholder,
              goal = c.goal,
              precondition = c.precondition,
              postcondition = c.postcondition
            )
          },
          domainQualities = p.domainQualities.map { q =>
            SComponent.QualityDefinition(
              name = q.name,
              summary = q.summary,
              description = q.description,
              goal = q.goal,
              precondition = q.precondition,
              postcondition = q.postcondition
            )
          },
          domainConstraints = p.domainConstraints.map { c =>
            SComponent.ConstraintDefinition(
              name = c.name,
              summary = c.summary,
              description = c.description,
              goal = c.goal,
              precondition = c.precondition,
              postcondition = c.postcondition
            )
          },
          domainUseCases = p.domainUseCases.map(_to_use_case_definition)
        )
      }

    private def _to_transition_trigger(
      p: MComponent.TransitionTrigger
    ): SComponent.TransitionTrigger =
      p match {
        case MComponent.TransitionTrigger.Save => SComponent.TransitionTrigger.Save
        case MComponent.TransitionTrigger.Update => SComponent.TransitionTrigger.Update
      }

    private def _to_rule_guard(
      p: MComponent.RuleGuard
    ): SComponent.RuleGuard =
      p match {
        case MComponent.RuleGuard.Ref(name) => SComponent.RuleGuard.Ref(name)
        case MComponent.RuleGuard.Expression(expr) => SComponent.RuleGuard.Expression(expr)
      }

    private def _to_rule_plan(
      p: MComponent.RulePlan
    ): SComponent.RulePlan =
      SComponent.RulePlan(
        exit = p.exit.map(_to_rule_action),
        transition = p.transition.map(_to_rule_action),
        entry = p.entry.map(_to_rule_action)
      )

    private def _to_rule_action(
      p: MComponent.RuleAction
    ): SComponent.RuleAction =
      SComponent.RuleAction(p.script)

    final protected def to_services(ps: Seq[MService]): List[SService] =
      ps.map(to_service).toList

    private def _description_text(p: MElement): Option[String] =
      Option(p.description.content.toText).map(_.trim).filter(_.nonEmpty)

    final protected def to_service(p: MService): SService = {
      val name = p.name
      val (ops, actions) = to_actions(p.operations)
      SService(
        p.packageName,
        name,
        ops,
        actions,
        _description_text(p),
        p.serviceCore.useCases.map(_to_use_case_definition)
      )
    }

    private def _to_actor_definition(p: MComponent.ActorDefinition): SComponent.ActorDefinition =
      SComponent.ActorDefinition(p.name, p.kind, p.summary, p.description)

    private def _to_actor_reference(p: MComponent.ActorReference): SComponent.ActorReference =
      SComponent.ActorReference(p.name, p.role, p.targetKind)

    private def _to_use_case_definition(p: MComponent.UseCaseDefinition): SComponent.UseCaseDefinition =
      SComponent.UseCaseDefinition(
        name = p.name,
        id = p.id,
        summary = p.summary,
        description = p.description,
        actor = p.actor,
        primaryActor = p.primaryActor,
        secondaryActor = p.secondaryActor,
        supportingActor = p.supportingActor,
        stakeholder = p.stakeholder,
        goal = p.goal,
        precondition = p.precondition,
        postcondition = p.postcondition,
        trigger = p.trigger,
        priority = p.priority,
        status = p.status,
        actorReferences = p.actorReferences.map(_to_actor_reference),
        scenarios = p.scenarios.map(_to_use_case_scenario)
      )

    private def _to_use_case_scenario(p: MComponent.UseCaseScenario): SComponent.UseCaseScenario =
      SComponent.UseCaseScenario(
        name = p.name,
        kind = p.kind,
        summary = p.summary,
        description = p.description,
        steps = p.steps,
        alternates = p.alternates,
        exceptions = p.exceptions
      )

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
      val descriptor = to_descriptor(p.descriptor)
      val rtype = to_result(p.result)
      val ap = Parameter.create("action", action)
      val aps = ParameterSequence(Vector(ap))
      val method = SMethod(
        MethodName(p.name),
        descriptor,
        aps,
        rtype,
        p.body,
        _description_text(p),
        p.access.map(a =>
          SComponent.OperationAccess(
            policy = a.policy,
            resource = a.resource,
            target = a.target,
            mode = a.mode,
            relation = a.relation,
            operationModel = a.operationModel,
            entityUsage = a.entityUsage,
            entityOperationKind = a.entityOperationKind,
            entityApplicationDomain = a.entityApplicationDomain,
            condition = a.condition
          )
        )
      )
      (method, Vector(action))
    }

    private def _create_action(p: MOperation): SCaseClass = {
      val pkgname = componentPackageName // TODO
      val name = p.name
      val actionkind = p.descriptor.kind match {
        case MOperation.Kind.Command => "command"
        case MOperation.Kind.Query => "query"
      }
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
      val a = sub_package_name.fold("")(x => "." + x)
      PackageName(p.packageName.name + a + ".impl")
    }
  }
}
