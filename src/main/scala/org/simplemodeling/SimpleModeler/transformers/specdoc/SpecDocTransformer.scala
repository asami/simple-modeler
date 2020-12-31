package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.simplemodeling.SimpleModeler.entity._
// import org.simplemodeling.SimpleModeler.entity.business._
// import org.simplemodeling.SimpleModeler.entity.domain._
// import org.simplemodeling.SimpleModeler.entity.requirement._
// import org.simplemodeling.SimpleModeler.entity.flow._
// import org.simplemodeling.SimpleModeler.sdoc.SMTermRef
// import org.goldenport.entities.smartdoc.block.SBBinaryContentFigure
// import org.simplemodeling.SimpleModeler.generators.uml._
// import org.goldenport.value._
// import org.goldenport.sdoc._
// import org.goldenport.sdoc.inline.{ SIAnchor, SElementRef }
// import org.goldenport.sdoc.block._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.dsl._
// import org.simplemodeling.dsl.domain._

import scala.collection.mutable
import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.value._
import org.smartdox.{Dox, Text, Fragment, Ul, Li}
import org.smartdox.builder.DoxBuilder
import org.smartdox.specdoc._
import org.simplemodeling.model._
import org.simplemodeling.model.business._
import org.simplemodeling.model.requirement._
import org.simplemodeling.model.domain._
import org.simplemodeling.SimpleModeler.transformer.maker._
import org.simplemodeling.SimpleModeler.generators.uml._

/*
 * Derived from SimpleModel2SpecDocTransformer
 *
 * @since   Sep. 13, 2008
 *  version Apr. 17, 2011
 *  version Nov. 25, 2012
 *  version Jun. 21, 2020
 *  version Jul. 28, 2020
 *  version Aug. 10, 2020
 *  version Sep. 13, 2020
 *  version Oct. 11, 2020
 *  version Nov. 19, 2020
 * @version Dec. 21, 2020
 * @author  ASAMI, Tomoharu
 */
class SpecDocTransformer(
  val context: PContext,
  val config: SpecDocTransformer.Config,
  val model: SimpleModel,
  val pmodel: PModel
    //  val simpleModel: SimpleModelEntity
) extends ObjectPart with FlowPart with GlossaryPart {
  import SpecDocTransformer._

  private def _dox_context = context.doxContext
  private val _builder = SpecDoc.Builder.create("TITLE")

  def drawDiagram = config.drawDiagram
  def classClassDiagramThemes = config.classClassDiagramThemes
  def packageClassDiagramThemes = config.packageClassDiagramThemes
  //
  val Category_Overview = new OverviewCategory
  val Category_Conclusion = new ConclusionCategory
  val Category_Glossary = new GlossaryCategory
  //
  val Category_Component = new ComponentCategory
  val Category_Entity = new EntityCategory
  val Category_Value = new ValueCategory
  val Category_Voucher = new VoucherCategory
  val Category_Service = new ServiceCategory
  val Category_Rule = new RuleCategory
  val Category_Powertype = new PowertypeCategory
  val Category_Datatype = new DatatypeCategory
  val Category_BusinessUsecase = new BusinessUsecaseCategory
  val Category_BusinessTask = new BusinessTaskCategory
  val Category_RequirementUsecase = new RequirementUsecaseCategory
  val Category_RequirementTask = new RequirementTaskCategory
  val Category_Flow = new FlowCategory
  //
  val Category_RoleRelationship = new RoleRelationshipCategory
  val Category_ServiceRelationship = new ServiceRelationshipCategory
  val Category_RuleRelationship = new RuleRelationshipCategory
  val Category_PowertypeRelationship = new PowertypeRelationshipCategory
  val Category_Attribute = new AttributeCategory
  val Category_Association = new AssociationCategory
  val Category_Operation = new OperationCategory
  val Category_StateMachine = new StateMachineCategory
  val Category_StateTransition = new StateTransitionCategory
  val Category_FlowMachine = new FlowMachineCategory
  val Category_Use = new UseCategory
  val Category_Participation = new ParticipationCategory
  val Category_ScriptFlow = new ScriptFlowCategory
  //  val Category_BasicFlow = new BasicFlowCategory
  //  val Category_AlternateFlow = new AlternateFlowCategory
  //  val Category_ExceptionFlow = new ExceptionFlowCategory

  val packageCategories = List(
    Category_Overview,
    Category_Component,
    Category_Entity,
    Category_Value,
    Category_Voucher,
    Category_Service,
    Category_Rule,
    Category_Powertype,
    Category_Datatype,
    Category_BusinessUsecase,
    Category_BusinessTask,
    Category_RequirementUsecase,
    Category_RequirementTask,
    Category_Flow,
    Category_Conclusion
  )
  val entityCategories = List(
    Category_Overview,
    Category_RoleRelationship,
    Category_ServiceRelationship,
    Category_RuleRelationship,
    Category_PowertypeRelationship,
    Category_Attribute,
    Category_Association,
    Category_Operation,
    Category_StateMachine,
    Category_StateTransition,
    Category_FlowMachine,
    Category_Use,
    Category_Participation,
    Category_ScriptFlow,
    Category_Conclusion
  )

  protected val generator = new ClassDiagramGenerator(context.context, model)

  def transform(): SpecDoc = {
    println(s"SpecDocTransformer: ${pmodel}")
    pmodel.packages.foreach(build_package)
    _builder()
  }

  protected def build_package(p: PPackage): Unit = {
    println(s"SpecDocTransformer#build_package $p")
    val name = if (p.name.isEmpty) "BASIC" else p.name // TODO
    println(s"SpecDocTransformer#build_package name: $name")
    val mpkg = p.model(model)
    val bpkg = _builder.addUpPackage(name)

    def add_overview {
      if (!drawDiagram) return
      if (packageClassDiagramThemes.isEmpty) return
//      val views = Dox.Builder()

      def build_hilight {
        val binary = generator.makeClassDiagramPng(mpkg, HilightPerspective)
        val src = "ClassDiagram" + p.name + "Hilight.png"
        val title = "概要"
        bpkg.addFigure(binary, src, title)
      }

      def build_perspective {
        val binary = generator.makeClassDiagramPng(mpkg, OverviewPerspective)
        val src = "ClassDiagram" + p.name + "Perspective.png"
        val title = "全体像"
        bpkg.addFigure(binary, src, title)
      }

      def build_detail {
        val binary = generator.makeClassDiagramPng(mpkg, DetailPerspective)
        val src = "ClassDiagram" + p.name + "Detail.png"
        val title = "詳細"
        bpkg.addFigure(binary, src, title)
      }

      for (theme <- packageClassDiagramThemes) {
        theme match {
          case Theme.Hilight     => build_hilight
          case Theme.Perspective => build_perspective
          case Theme.Detail      => build_detail
          case _           => RAISE.noReachDefect("parameter error: " + theme)
        }
      }
    }

    def make_element(p: PElement): Option[SDEntity] = {
      println(s"SpecDocTransformer#make_element $p")
      p match {
        case m: PObject => Option(m.modelObject).map {
          case m: MDomainActor => add_actor(m)
          case m: MDomainRole => add_role(m)
          case m: MDomainResource => add_resource(m)
          case m: MDomainEvent => add_event(m)
          case m: MDomainSummary => add_summary(m)
          case m: MDomainAssociationEntity => add_associationEntity(m)
          case m: MDomainEntity => add_entity(m)
          case m: MDomainValue => add_value(m)
          case m: MDomainVoucher => add_voucher(m)
          case m: MService => add_service(m)
          case m: MComponent => add_component(m)
          case m: MRule => add_rule(m)
          case m: MPowertype => add_powertype(m)
          case m: MDatatype => add_datatype(m)
          case m: MBusinessUsecase => add_businessUsecase(m)
          case m: MBusinessTask => add_businessTask(m)
          case m: MRequirementUsecase => add_requirementUsecase(m)
          case m: MRequirementTask => add_requirementTask(m)
          case m: MFlow => add_flow(m)
        }
        case _ => None
      }
    }

    def add_actor(anActor: MDomainActor) = {
      val actor = add_object(anActor)
      actor
    }

    def add_role(aRole: MDomainRole) = {
      val role = add_object(aRole)
      role
    }

    def add_resource(aResource: MDomainResource) = {
      val resource = add_object(aResource)
      resource
    }

    def add_event(anEvent: MDomainEvent) = {
      val event = add_object(anEvent)
      event
    }

    def add_summary(aSummary: MDomainSummary) = {
      val summary = add_object(aSummary)
      summary
    }

    def add_associationEntity(aAssociationEntity: MDomainAssociationEntity) = {
      val summary = add_object(aAssociationEntity)
      summary
    }

    def add_entity(anEntity: MDomainEntity) = {
      val entity = add_object(anEntity)
      entity
    }

    // def add_id(anId: MDomainValueId) {
    //   val id = add_object(anId)
    //   id
    // }

    // def add_name(aName: MDomainValueName) {
    //   val name = add_object(aName)
    //   name
    // }

    def add_value(aValue: MDomainValue) = {
      val value = add_object(aValue)
      value
    }

    def add_voucher(aVoucher: MDomainVoucher) = {
      val voucher = add_object(aVoucher)
      voucher
    }

    def add_service(aService: MService) = {
      val service = add_object(aService)
      service
    }

    def add_component(aComponent: MComponent) = {
      val component = add_object(aComponent)
      component
    }

    def add_rule(aRule: MRule) = {
      val rule = add_object(aRule)
      rule
    }

    def add_powertype(aPowertype: MPowertype) = {
      val powertype = add_object(aPowertype)
      powertype
    }

    def add_datatype(aDatatype: MDatatype): SDEntity = {
      // val datatype = add_object(aDatatype)
      // datatype
      // TODO
      ???
    }

    def add_businessUsecase(aBusinessUsecase: MBusinessUsecase) = {
      val businessUsecase = add_object(aBusinessUsecase)
      businessUsecase.addFeature(MFeature.IncludeUsecase, aBusinessUsecase.includeUsecasesLiteral)
      businessUsecase.addFeature(MFeature.IncludeTask, aBusinessUsecase.includeTasksLiteral)
      businessUsecase.addFeature(MFeature.UserUsecase, aBusinessUsecase.userUsecasesLiteral)
      add_story(businessUsecase, aBusinessUsecase)
      businessUsecase
    }

    def add_businessTask(aBusinessTask: MBusinessTask) = {
      val businessTask = add_object(aBusinessTask)
      /* 2008-12-10
       println("add_businessTask = " + aBusinessTask)
       for (step <- aBusinessTask.basicFlow) {
       println("add_businessTask : " + step.toXml)
       }
       */
      businessTask.addFeature(MFeature.UserUsecase, aBusinessTask.userBusinessUsecasesLiteral)
      add_story(businessTask, aBusinessTask)
      businessTask
    }

    def add_requirementUsecase(aRequirementUsecase: MRequirementUsecase) = {
      val requirementUsecase = add_object(aRequirementUsecase)
      requirementUsecase.addFeature(MFeature.IncludeUsecase, aRequirementUsecase.includeUsecasesLiteral)
      requirementUsecase.addFeature(MFeature.IncludeTask, aRequirementUsecase.includeTasksLiteral)
      requirementUsecase.addFeature(MFeature.UserUsecase, aRequirementUsecase.userUsecasesLiteral)
      requirementUsecase.addFeature(MFeature.UserTask, aRequirementUsecase.userBusinessTasksLiteral)
      add_story(requirementUsecase, aRequirementUsecase)
      requirementUsecase
    }

    def add_requirementTask(aRequirementTask: MRequirementTask) = {
      val requirementTask = add_object(aRequirementTask)
      /* 2008-12-10
       println("add_requirementTask = " + aRequirementTask)
       for (step <- aRequirementTask.basicFlow) {
       println("add_requirementTask : " + step.toXml)
       }
       */
      requirementTask.addFeature(MFeature.UserUsecase, aRequirementTask.userRequirementUsecasesLiteral)
      add_story(requirementTask, aRequirementTask)
      requirementTask
    }

    def add_story(aStoryEntity: SDEntity, aStoryObject: MStoryObject) = {
      /*
       val useEntity = new SDEntity("使用")
       aStoryEntity.addSubEntity(useEntity)
       */
      def get_sequence_number(aMark: String): String = {
        aStoryObject.getStep(aMark) match {
          case Some(step: MStep) => step.sequenceNumber.toString
          case None               => "???"
        }
      }

      def step_action_text(aStep: MActionStep): Dox = {
        Text(aStep.action)
      }

      def step_execution_text(aStep: MExecutionStep): Dox =
        Fragment(aStoryObject.getPrimaryActorTerm(aStep)).
          append("は").
          append(aStep.getTargetEntityTerm).
          append("の").
          append(aStep.getVerbTerm).
          append("を実行する。")
        // val sdoc = new DoxBuilder(_dox_context)
        // sdoc.add(aStoryObject.getPrimaryActorTerm(aStep))
        // sdoc.addChild("は")
        // sdoc.addChild(aStep.getTargetEntityTerm)
        // sdoc.addChild("の")
        // sdoc.addChild(aStep.getVerbTerm)
        // sdoc.addChild("を実行する。")
        // sdoc

      def step_request_text(aStep: MInvocationStep): Dox = {
        val a = Fragment(aStoryObject.getPrimaryActorTerm(aStep)).
          append("は").
          append(aStoryObject.getSecondaryActorTerm(aStep)).
          append("に")
        val b = if (aStep.isRequestVoucher)
          a.append(aStep.getRequestVoucherTerm).append("で")
        else
          a
        b.append(aStep.getVerbTerm).append("を依頼する。")
        // val sdoc = new SFragment()
        // sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
        // sdoc.addChild("は")
        // sdoc.addChild(aStoryObject.getSecondaryActorTerm(aStep))
        // sdoc.addChild("に")
        // if (aStep.isRequestDocument) {
        //   sdoc.addChild(aStep.getRequestDocumentTerm)
        //   sdoc.addChild("で")
        // }
        // sdoc.addChild(aStep.getVerbTerm)
        // sdoc.addChild("を依頼する。")
        // sdoc
      }

      def step_response_text(aStep: MInvocationStep): Dox = {
        val a = Fragment(aStoryObject.getSecondaryActorTerm(aStep)).
          append("は").
          append(aStoryObject.getPrimaryActorTerm(aStep)).
          append("に")
        val b = if (aStep.isResponseVoucher)
          a.append(aStep.getResponseVoucherTerm).append("で")
        else
          a
        b.append(aStep.getVerbTerm).append("の結果を返す。")
        // val sdoc = new SFragment()
        // sdoc.addChild(aStoryObject.getSecondaryActorTerm(aStep))
        // sdoc.addChild("は")
        // sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
        // sdoc.addChild("に")
        // if (aStep.isResponseDocument) {
        //   sdoc.addChild(aStep.getResponseDocumentTerm)
        //   sdoc.addChild("で")
        // }
        // sdoc.addChild(aStep.getVerbTerm)
        // sdoc.addChild("の結果を返す。")
        // sdoc
      }

      def step_parameter_text(aStep: MParameterStep): Dox =
        Fragment(aStoryObject.getPrimaryActorTerm(aStep)).
          append("は").
          append(aStep.getVoucherTerm).
          append("の").
          append(aStep.getParameterTerm).
          append("を設定する。")
        // val sdoc = new SFragment()
        // sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
        // sdoc.addChild("は")
        // sdoc.addChild(aStep.getDocumentTerm)
        // sdoc.addChild("に")
        // sdoc.addChild(aStep.getParameterTerm)
        // sdoc.addChild("を設定する。")
        // sdoc

      def step_usecase_text(aStep: MUsecaseStep): Dox =
        aStep.getUsecaseTerm
        // val sdoc = new SFragment()
        // sdoc.addChild(aStep.getUsecaseTerm)
        // sdoc

      def basic_flow = {
        def story_view = {
          val xs = build_flow(aStoryObject.basicFlow)
          val ul = Ul(xs)
          SDEntity("物語", Category_ScriptFlow, ul)
        }

        def collaboration_view = ???
        def data_view = ???
        def task_view = ???

        val elements = List(
          story_view,
          collaboration_view,
          data_view,
          task_view
        )
        SDEntity("基本脚本", Category_ScriptFlow, elements)
      }

      def extension_flow = {
        val elements = List(
        )
        SDEntity("拡張脚本", Category_ScriptFlow, elements)
      }

      def alternate_flow = {
        val elements = List(
        )
        SDEntity("代替脚本", Category_ScriptFlow, elements)
      }

      def exception_flow = {
        val elements = List(
        )
        SDEntity("例外脚本", Category_ScriptFlow, elements)
      }

      val elements = List(
        basic_flow,
        extension_flow,
        alternate_flow,
        exception_flow
      )
      SDEntity("使用" /* ??? */, Category_ScriptFlow /* ??? */, elements) // TODO

      // def add_basicFlow {
      //   val basicFlowEntity = SDEntity("基本脚本")
      //   basicFlowEntity.category = Category_ScriptFlow
      //   val views = SBViews()

      //   def add_story_view {
      //     val view = SBView()
      //     view.title = "物語"
      //     views.addChild(view)
      //     val ul = SBUnorderedList()
      //     view.addChild(ul)
      //     for (task <- aStoryObject.basicFlow) {
      //       task.traverse(new GTreeVisitor[MElement] {
      //         override def enter(aNode: GTreeNode[MElement]) {
      //           aNode.content match {
      //             case step: MActionStep        => enter_action_step(step)
      //             case step: MExecutionStep     => enter_execution_step(step)
      //             case step: MInvocationStep    => enter_invocation_step(step)
      //             case step: MUsecaseStep       => enter_usecase_step(step)
      //             case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
      //           }
      //         }

      //         override def leave(aNode: GTreeNode[MElement]) {
      //           aNode.content match {
      //             case step: MActionStep        => leave_action_step(step)
      //             case step: MExecutionStep     => leave_execution_step(step)
      //             case step: MInvocationStep    => leave_invocation_step(step)
      //             case step: MUsecaseStep       => leave_usecase_step(step)
      //             case step: MExtendUsecaseStep => leave_extendUsecase_step(step)
      //           }
      //         }

      //         private def enter_action_step(aStep: MActionStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_action_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_action_step(aStep: MActionStep) {
      //         }

      //         private def enter_execution_step(aStep: MExecutionStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_execution_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_execution_step(aStep: MExecutionStep) {
      //         }

      //         private def enter_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_request_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_invocation_step(aStep: MInvocationStep) {
      //           if (!aStep.isBidirectional) return
      //           val seq = aStep.leaveSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_response_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def enter_usecase_step(aStep: MUsecaseStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_usecase_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_usecase_step(aStep: MUsecaseStep) {
      //         }

      //         private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //           def make_extend_usecase_list = {
      //             val uul = new SBUnorderedList()
      //             for ((value, usecase) <- aStep.dslExtendUsecases) {
      //               val uli = new SBListItem()
      //               uli.addChild(value + "の場合: ")
      //               uli.addChild(new MTermRef(usecase))
      //               uul.addChild(uli)
      //             }
      //             uul
      //           }

      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = new SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild("拡張点: " + aStep.extensionPointName)
      //           li.addChild(make_extend_usecase_list)
      //           ul.addChild(li)
      //         }

      //         private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }
      //       })
      //     }
      //   }

      //   def add_collaboration_view {
      //     val view = SBView()
      //     view.title = "協調"
      //     views.addChild(view)
      //     val ul = SBUnorderedList()
      //     view.addChild(ul)
      //     for (task <- aStoryObject.basicFlow) {
      //       task.traverse(new GTreeVisitor[MElement] {
      //         override def enter(aNode: GTreeNode[MElement]) {
      //           aNode.content match {
      //             case step: MActionStep        => enter_action_step(step)
      //             case step: MExecutionStep     => enter_execution_step(step)
      //             case step: MInvocationStep    => enter_invocation_step(step)
      //             case step: MUsecaseStep       => enter_usecase_step(step)
      //             case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
      //           }
      //         }

      //         override def leave(aNode: GTreeNode[MElement]) {
      //         }

      //         private def enter_action_step(aStep: MActionStep) {
      //           val seq = aStep.layeredSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_action_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_action_step(aStep: MActionStep) {
      //         }

      //         private def enter_execution_step(aStep: MExecutionStep) {
      //           val seq = aStep.layeredSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_execution_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_execution_step(aStep: MExecutionStep) {
      //         }

      //         private def enter_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.layeredSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_request_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_invocation_step(aStep: MInvocationStep) {
      //           if (!aStep.isBidirectional) return
      //           val seq = aStep.layeredSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_response_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def enter_usecase_step(aStep: MUsecaseStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_usecase_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_usecase_step(aStep: MUsecaseStep) {
      //         }

      //         private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }

      //         private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }
      //       })
      //     }
      //   }

      //   def add_data_view {
      //     val view = SBView()
      //     view.title = "データ"
      //     views.addChild(view)
      //     val ul = SBUnorderedList()
      //     view.addChild(ul)
      //     for (task <- aStoryObject.basicFlow) {
      //       task.traverse(new GTreeVisitor[MElement] {
      //         override def enter(aNode: GTreeNode[MElement]) {
      //           aNode.content match {
      //             case step: MActionStep        => enter_action_step(step)
      //             case step: MExecutionStep     => enter_execution_step(step)
      //             case step: MInvocationStep    => enter_invocation_step(step)
      //             case step: MUsecaseStep       => enter_usecase_step(step)
      //             case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
      //           }
      //         }

      //         override def leave(aNode: GTreeNode[MElement]) {
      //         }

      //         private def enter_action_step(aStep: MActionStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_action_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_action_step(aStep: MActionStep) {
      //         }

      //         private def enter_execution_step(aStep: MExecutionStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_execution_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_execution_step(aStep: MExecutionStep) {
      //         }

      //         private def enter_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_request_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_response_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def enter_usecase_step(aStep: MUsecaseStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_usecase_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_usecase_step(aStep: MUsecaseStep) {
      //         }

      //         private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }

      //         private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }
      //       })
      //     }
      //   }

      //   def add_task_view {
      //     val view = SBView()
      //     view.title = "タスク"
      //     views.addChild(view)
      //     val ul = SBUnorderedList()
      //     view.addChild(ul)
      //     for (task <- aStoryObject.basicFlow) {
      //       task.traverse(new GTreeVisitor[MElement] {
      //         override def enter(aNode: GTreeNode[MElement]) {
      //           aNode.content match {
      //             case step: MActionStep        => enter_action_step(step)
      //             case step: MExecutionStep     => enter_execution_step(step)
      //             case step: MInvocationStep    => enter_invocation_step(step)
      //             case step: MUsecaseStep       => enter_usecase_step(step)
      //             case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
      //           }
      //         }

      //         override def leave(aNode: GTreeNode[MElement]) {
      //         }

      //         private def enter_action_step(aStep: MActionStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_action_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_action_step(aStep: MActionStep) {
      //         }

      //         private def enter_execution_step(aStep: MExecutionStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_execution_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_execution_step(aStep: MExecutionStep) {
      //         }

      //         private def enter_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_request_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_invocation_step(aStep: MInvocationStep) {
      //           if (!aStep.isBidirectional) return
      //           val seq = aStep.sequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(step_response_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def enter_usecase_step(aStep: MUsecaseStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_usecase_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_usecase_step(aStep: MUsecaseStep) {
      //         }

      //         private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }

      //         private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }
      //       })
      //     }
      //   }

      //   add_story_view
      //   add_collaboration_view
      //   add_data_view
      //   add_task_view
      //   basicFlowEntity.description = views
      //   aStoryEntity.addSubEntity(basicFlowEntity)
      // }

      // def add_extensionFlow {
      //   val extensionFlowEntity = new SDEntity("拡張脚本")
      //   extensionFlowEntity.category = Category_ScriptFlow
      //   val views = SBViews()

      //   def add_story_view {
      //     val view = SBView()
      //     view.title = "物語"
      //     views.addChild(view)
      //     val segmentsUl = SBUnorderedList()
      //     view.addChild(segmentsUl)
      //     for (segment <- aStoryObject.extensionFlow) {
      //       val li = SBListItem()
      //       li.addChild(segment.name)
      //       segmentsUl.addChild(li)
      //       val ul = SBUnorderedList()
      //       segmentsUl.addChild(ul)
      //       segment.flow.traverse(new GTreeVisitor[MStep] {
      //         override def enter(aNode: GTreeNode[MStep]) {
      //           aNode.content match {
      //             case step: MBusinessTaskStep  => //
      //             case step: MActionStep        => enter_action_step(step)
      //             case step: MExecutionStep     => enter_execution_step(step)
      //             case step: MInvocationStep    => enter_invocation_step(step)
      //             case step: MParameterStep     => enter_parameter_step(step)
      //             case step: MUsecaseStep       => enter_usecase_step(step)
      //             case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
      //           }
      //         }

      //         override def leave(aNode: GTreeNode[MStep]) {
      //           aNode.content match {
      //             case step: MBusinessTaskStep  => //
      //             case step: MActionStep        => leave_action_step(step)
      //             case step: MExecutionStep     => leave_execution_step(step)
      //             case step: MInvocationStep    => leave_invocation_step(step)
      //             case step: MParameterStep     => leave_parameter_step(step)
      //             case step: MUsecaseStep       => leave_usecase_step(step)
      //             case step: MExtendUsecaseStep => leave_extendUsecase_step(step)
      //           }
      //         }

      //         private def enter_action_step(aStep: MActionStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_action_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_action_step(aStep: MActionStep) {
      //         }

      //         private def enter_execution_step(aStep: MExecutionStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_execution_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_execution_step(aStep: MExecutionStep) {
      //         }

      //         private def enter_invocation_step(aStep: MInvocationStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_request_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_invocation_step(aStep: MInvocationStep) {
      //           if (!aStep.isBidirectional) return
      //           val seq = aStep.leaveSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_response_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def enter_parameter_step(aStep: MParameterStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_parameter_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_parameter_step(aStep: MParameterStep) {
      //         }

      //         private def enter_usecase_step(aStep: MUsecaseStep) {
      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild(step_usecase_text(aStep))
      //           ul.addChild(li)
      //         }

      //         private def leave_usecase_step(aStep: MUsecaseStep) {
      //         }

      //         private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //           def make_extend_usecase_list = {
      //             val uul = new SBUnorderedList()
      //             for ((value, usecase) <- aStep.dslExtendUsecases) {
      //               val uli = new SBListItem()
      //               uli.addChild(value + "の場合: ")
      //               uli.addChild(new MTermRef(usecase))
      //               uul.addChild(uli)
      //             }
      //             uul
      //           }

      //           val seq = aStep.enterSequenceNumber.toString
      //           val li = new SBListItem()
      //           li.addChild(seq + " ")
      //           li.addChild("拡張点: " + aStep.extensionPointName)
      //           li.addChild(make_extend_usecase_list)
      //           ul.addChild(li)
      //         }

      //         private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
      //         }
      //       })
      //     }
      //   }

      //   add_story_view
      //   extensionFlowEntity.description = views
      //   aStoryEntity.addSubEntity(extensionFlowEntity)
      // }

      // def add_alternateFlow {
      //   val altFlowEntity = new SDEntity("代替脚本")
      //   altFlowEntity.category = Category_ScriptFlow
      //   val ul = SBUnorderedList()

      //   aStoryObject.alternateFlow.traverse(new GTreeVisitor[MAlternatePath] {
      //     override def enter(aNode: GTreeNode[MAlternatePath]) {
      //       val path = aNode.content
      //       val li = SBListItem(make_path_text(path))
      //       ul.addChild(li)
      //     }

      //     override def leave(aNode: GTreeNode[MAlternatePath]) {
      //     }

      //     def make_path_text(aPath: MAlternatePath): Dox = {
      //       aPath match {
      //         case path: MJumpPath      => make_path_text(path)
      //         case path: SOmitPath       => error("not implemented yet")
      //         case path: SSubstitutePath => error("not implemented yet")
      //       }
      //     }

      //     def make_path_text(aPath: MJumpPath): Dox = {
      //       val sdoc = SFragment()
      //       val markRange = aPath.markRange
      //       val mark = markRange._1 // XXX
      //       if (mark == "") {
      //         sdoc.addChild("???")
      //       } else {
      //         val seq = get_sequence_number(mark)
      //         val label = seq + ".a." // XXX
      //         sdoc.addChild(label)
      //       }
      //       sdoc.addChild(" " + aPath.condition + "の場合、")
      //       val destination = aPath.destinationStep
      //       if (mark == "") {
      //         sdoc.addChild("???")
      //       } else {
      //         sdoc.addChild(destination.sequenceNumber + "へ移る")
      //       } // XXX でもよい
      //       sdoc
      //     }
      //   })
      //   altFlowEntity.description = ul
      //   aStoryEntity.addSubEntity(altFlowEntity)
      // }

      // def add_exceptionFlow {
      //   val exceptionFlowEntity = new SDEntity("例外脚本")
      //   exceptionFlowEntity.category = Category_ScriptFlow
      //   val ul = SBUnorderedList()
      //   aStoryObject.exceptionFlow.traverse(new GTreeVisitor[MExceptionPath] {
      //     override def enter(aNode: GTreeNode[MExceptionPath]) {
      //       val path = aNode.content.asInstanceOf[MExceptionPath]
      //       val li = SBListItem(make_path_text(path))
      //       ul.addChild(li)
      //     }
      //     override def leave(aNode: GTreeNode[MExceptionPath]) {
      //     }

      //     def make_path_text(aPath: MExceptionPath): Dox = {
      //       def make_text_body = {
      //         " " + aPath.condition + "の場合、終了"
      //       }

      //       val sdoc = SFragment()
      //       val markRange = aPath.markRange
      //       val mark = markRange._1 // XXX
      //       if (mark == "") {
      //         sdoc.addChild("???")
      //       } else {
      //         val seq = get_sequence_number(mark)
      //         val label = seq + ".a." // XXX
      //         sdoc.addChild(label)
      //       }
      //       sdoc.addChild(make_text_body)
      //       sdoc
      //     }
      //   })
      //   exceptionFlowEntity.description = ul
      //   aStoryEntity.addSubEntity(exceptionFlowEntity)
      // }

      // add_basicFlow
      // add_extensionFlow
      // add_alternateFlow
      // add_exceptionFlow
      // aStoryEntity
    }

    // Flow (asakusa)
    def add_flow(flow: MFlow): SDEntity = {
      val sd = add_object(flow)
      sd
    }

    // def add_glossary() {
    //   val glossary = new SDSummary("glossary")
    //   specPackage.addSummary(glossary)
    //   glossary.title = "用語集"
    //   glossary.tableHead = List("用語", "説明", "オブジェクト", "種類")
    //   glossary.tableRow = (anEntity: SDEntity) =>
    //   Array(
    //     anEntity.feature(FeatureTerm).value,
    //     anEntity.summary,
    //     anEntity.anchor,
    //     anEntity.feature(FeatureType).value)
    // }

    val description = {
      val name = p.description.name
      if (name.isEmpty)
        p.description.withName("DEFAULT")
      else
        p.description
    }
    println(s"SpecDocTransformer#description before ${p.description}")
    println(s"SpecDocTransformer#description $description")
    bpkg.addCategories(packageCategories)
    bpkg.setDescription(description)
    bpkg.setFeatureSet(p.featureSet)
    add_overview
//    p.elements.flatMap(make_element).map(bpkg.addEntity)

    make_actors(bpkg, p.elements)
    make_resources(bpkg, p.elements)
    make_events(bpkg, p.elements)

    bpkg.addEntity(add_glossary)

//     specPackage.categories ++= packageCategories
//     specPackage.sdocTitle = Text(modelPackage.name)
//     specPackage.caption = modelPackage.caption
//     specPackage.brief = modelPackage.brief
//     specPackage.summary = modelPackage.summary
//     specPackage.description = modelPackage.description
//     specPackage.note = modelPackage.note
//     for (feature <- modelPackage.features) {
//       specPackage.addFeature(feature.key, feature.value) // XXX description_is
//     }
//     add_overview
//     modelPackage.domainActors.foreach(add_actor)
//     modelPackage.domainRoles.foreach(add_role)
//     modelPackage.domainResources.foreach(add_resource)
//     modelPackage.domainEvents.foreach(add_event)
//     modelPackage.domainSummaries.foreach(add_summary)
//     modelPackage.domainEntities.foreach(add_entity)
// //    modelPackage.domainIds.foreach(add_id)
// //    modelPackage.domainNames.foreach(add_name)
//     modelPackage.domainValues.foreach(add_value)
//     modelPackage.domainDocuments.foreach(add_document)
//     modelPackage.domainServices.foreach(add_service)
//     modelPackage.domainRules.foreach(add_rule)
//     modelPackage.domainPowertypes.foreach(add_powertype)
//     modelPackage.datatypes.foreach(add_datatype)
//     modelPackage.businessUsecases.foreach(add_businessUsecase)
//     modelPackage.businessTasks.foreach(add_businessTask)
//     modelPackage.requirementUsecases.foreach(add_requirementUsecase)
//     modelPackage.requirementTasks.foreach(add_requirementTask)
//     modelPackage.systemComponents.foreach(add_component)
//     modelPackage.flows.foreach(add_flow)
//     add_glossary()

    def make_actors(bpkg: SpecDoc.Builder, ps: Seq[PElement]): Unit =
      make_entities(bpkg, "アクター", MDomainActorStereotype, ps)

    def make_resources(bpkg: SpecDoc.Builder, ps: Seq[PElement]): Unit =
      make_entities(bpkg, "リソース", MDomainResourceStereotype, ps)

    def make_events(bpkg: SpecDoc.Builder, ps: Seq[PElement]): Unit =
      make_entities(bpkg, "イベント", MDomainEventStereotype, ps)

    def make_entities(
      bpkg: SpecDoc.Builder,
      title: String,
      stereotype: MStereotype,
      ps: Seq[PElement]
    ): Unit = {
      val xs = ps collect {
        case m: PEntity if (_is_entity(stereotype, m)) => m
      }
      if (xs.nonEmpty)
        _make_entity(bpkg, title, xs)
    }

    def _is_entity(s: MStereotype, p: PEntity) =
      p.stereotypes.contains(s)

    def _make_entity(bpkg: SpecDoc.Builder, title: String, ps: Seq[PEntity]): Unit = {
      val elements = ps.flatMap(make_element)
      val entity = SDEntity(title, Category_Entity, elements)
      println(s"_make_entity: ${entity.name}/${title}")
      bpkg.addEntity(entity)
    }
  }

// val specdoc = new SpecDocEntity(simpleModel.entityContext)

// def transform: SpecDocEntity = {
//   simpleModel.open()
//   specdoc.open()
//   specdoc.name = simpleModel.name
//   specdoc.sdocTitle = simpleModel.title
//   simpleModel.activePackages.foreach(build_package)
//   simpleModel.close()
//   specdoc ensuring (_.isOpened)
// }

//   private def build_package(modelPackage: MPackage) {
//     val specPackage = specdoc.setPackage(modelPackage.pathname)

//     def add_overview {
//       if (!drawDiagram) return
//       if (packageClassDiagramThemes.isEmpty) return
//       val generator = new ClassDiagramGenerator(simpleModel)
//       val views = SBViews()

//       def build_hilight {
//         val binary = generator.makeClassDiagramPng(modelPackage, "hilight")
//         val figure = new SBBinaryContentFigure(binary)
//         figure.src = "ClassDiagram" + modelPackage.name + "Hilight.png"
//         val view = SBView()
//         view.title = "概要"
//         view.addChild(figure)
//         views.addChild(view)
//       }

//       def build_perspective {
//         val binary = generator.makeClassDiagramPng(modelPackage, "perspective")
//         val figure = new SBBinaryContentFigure(binary)
//         figure.src = "ClassDiagram" + modelPackage.name + "Perspective.png"
//         val view = SBView()
//         view.title = "全体像"
//         view.addChild(figure)
//         views.addChild(view)
//       }

//       def build_detail {
//         val binary = generator.makeClassDiagramPng(modelPackage, "detail")
//         val figure = new SBBinaryContentFigure(binary)
//         figure.src = "ClassDiagram" + modelPackage.name + "Detail.png"
//         val view = SBView()
//         view.title = "詳細"
//         view.addChild(figure)
//         views.addChild(view)
//       }

//       for (theme <- packageClassDiagramThemes) {
//         theme match {
//           case "hilight"     => build_hilight
//           case "perspective" => build_perspective
//           case "detail"      => build_detail
//           case _             => error("parameter error: " + theme)
//         }
//       }
//       if (views.length == 1) {
//         specPackage.overview = views.getChild(0).getChild(0)
//       } else {
//         specPackage.overview = views
//       }
//     }

//     def add_actor(anActor: SMDomainActor) {
//       val actor = add_object(anActor)
//       actor
//     }

//     def add_role(aRole: SMDomainRole) {
//       val role = add_object(aRole)
//       role
//     }

//     def add_resource(aResource: SMDomainResource) {
//       val resource = add_object(aResource)
//       resource
//     }

//     def add_event(anEvent: SMDomainEvent) {
//       val event = add_object(anEvent)
//       event
//     }

//     def add_summary(aSummary: SMDomainSummary) {
//       val summary = add_object(aSummary)
//       summary
//     }

//     def add_associationEntity(aAssociationEntity: SMDomainAssociationEntity) {
//       val summary = add_object(aAssociationEntity)
//       summary
//     }

//     def add_entity(anEntity: SMDomainEntity) {
//       val entity = add_object(anEntity)
//       entity
//     }

//     def add_id(anId: SMDomainValueId) {
//       val id = add_object(anId)
//       id
//     }

//     def add_name(aName: SMDomainValueName) {
//       val name = add_object(aName)
//       name
//     }

//     def add_value(aValue: SMDomainValue) {
//       val value = add_object(aValue)
//       value
//     }

//     def add_document(aDocument: SMDomainDocument) {
//       val document = add_object(aDocument)
//       document
//     }

//     def add_service(aService: SMService) {
//       val service = add_object(aService)
//       service
//     }

//     def add_component(aComponent: SMComponent) {
//       val component = add_object(aComponent)
//       component
//     }

//     def add_rule(aRule: SMRule) {
//       val rule = add_object(aRule)
//       rule
//     }

//     def add_powertype(aPowertype: SMPowertype) {
//       val powertype = add_object(aPowertype)
//       powertype
//     }

//     def add_datatype(aDatatype: SMDatatype) {
//       val datatype = add_object(aDatatype)
//       datatype
//     }

//     def add_businessUsecase(aBusinessUsecase: SMBusinessUsecase) {
//       val businessUsecase = add_object(aBusinessUsecase)
//       businessUsecase.addFeature(FeatureIncludeUsecase, aBusinessUsecase.includeUsecasesLiteral)
//       businessUsecase.addFeature(FeatureIncludeTask, aBusinessUsecase.includeTasksLiteral)
//       businessUsecase.addFeature(FeatureUserUsecase, aBusinessUsecase.userUsecasesLiteral)
//       add_story(businessUsecase, aBusinessUsecase)
//       businessUsecase
//     }

//     def add_businessTask(aBusinessTask: SMBusinessTask) {
//       val businessTask = add_object(aBusinessTask)
//       /* 2008-12-10
//       println("add_businessTask = " + aBusinessTask)
//       for (step <- aBusinessTask.basicFlow) {
// 	println("add_businessTask : " + step.toXml)
//       }
// */
//       businessTask.addFeature(FeatureUserUsecase, aBusinessTask.userBusinessUsecasesLiteral)
//       add_story(businessTask, aBusinessTask)
//       businessTask
//     }

//     def add_requirementUsecase(aRequirementUsecase: SMRequirementUsecase) {
//       val requirementUsecase = add_object(aRequirementUsecase)
//       requirementUsecase.addFeature(FeatureIncludeUsecase, aRequirementUsecase.includeUsecasesLiteral)
//       requirementUsecase.addFeature(FeatureIncludeTask, aRequirementUsecase.includeTasksLiteral)
//       requirementUsecase.addFeature(FeatureUserUsecase, aRequirementUsecase.userUsecasesLiteral)
//       requirementUsecase.addFeature(FeatureUserTask, aRequirementUsecase.userBusinessTasksLiteral)
//       add_story(requirementUsecase, aRequirementUsecase)
//       requirementUsecase
//     }

//     def add_requirementTask(aRequirementTask: SMRequirementTask) {
//       val requirementTask = add_object(aRequirementTask)
//       /* 2008-12-10
//       println("add_requirementTask = " + aRequirementTask)
//       for (step <- aRequirementTask.basicFlow) {
// 	println("add_requirementTask : " + step.toXml)
//       }
// */
//       requirementTask.addFeature(FeatureUserUsecase, aRequirementTask.userRequirementUsecasesLiteral)
//       add_story(requirementTask, aRequirementTask)
//       requirementTask
//     }

//     def add_story(aStoryEntity: SDEntity, aStoryObject: SMStoryObject) {
//       /*
//       val useEntity = new SDEntity("使用")
//       aStoryEntity.addSubEntity(useEntity)
// */
//       def get_sequence_number(aMark: String): String = {
//         aStoryObject.getStep(aMark) match {
//           case Some(step: SMStep) => step.sequenceNumber.toString
//           case None               => "???"
//         }
//       }

//       def step_action_text(aStep: SMActionStep): SDoc = {
//         SText(aStep.action)
//       }

//       def step_execution_text(aStep: SMExecutionStep): SDoc = {
//         val sdoc = new SFragment()
//         sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
//         sdoc.addChild("は")
//         sdoc.addChild(aStep.getTargetEntityTerm)
//         sdoc.addChild("の")
//         sdoc.addChild(aStep.getVerbTerm)
//         sdoc.addChild("を実行する。")
//         sdoc
//       }

//       def step_request_text(aStep: SMInvocationStep): SDoc = {
//         val sdoc = new SFragment()
//         sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
//         sdoc.addChild("は")
//         sdoc.addChild(aStoryObject.getSecondaryActorTerm(aStep))
//         sdoc.addChild("に")
//         if (aStep.isRequestDocument) {
//           sdoc.addChild(aStep.getRequestDocumentTerm)
//           sdoc.addChild("で")
//         }
//         sdoc.addChild(aStep.getVerbTerm)
//         sdoc.addChild("を依頼する。")
//         sdoc
//       }

//       def step_response_text(aStep: SMInvocationStep): SDoc = {
//         val sdoc = new SFragment()
//         sdoc.addChild(aStoryObject.getSecondaryActorTerm(aStep))
//         sdoc.addChild("は")
//         sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
//         sdoc.addChild("に")
//         if (aStep.isResponseDocument) {
//           sdoc.addChild(aStep.getResponseDocumentTerm)
//           sdoc.addChild("で")
//         }
//         sdoc.addChild(aStep.getVerbTerm)
//         sdoc.addChild("の結果を返す。")
//         sdoc
//       }

//       def step_parameter_text(aStep: SMParameterStep): SDoc = {
//         val sdoc = new SFragment()
//         sdoc.addChild(aStoryObject.getPrimaryActorTerm(aStep))
//         sdoc.addChild("は")
//         sdoc.addChild(aStep.getDocumentTerm)
//         sdoc.addChild("に")
//         sdoc.addChild(aStep.getParameterTerm)
//         sdoc.addChild("を設定する。")
//         sdoc
//       }

//       def step_usecase_text(aStep: SMUsecaseStep): SDoc = {
//         val sdoc = new SFragment()
//         sdoc.addChild(aStep.getUsecaseTerm)
//         sdoc
//       }

//       def add_basicFlow {
//         val basicFlowEntity = new SDEntity("基本脚本")
//         basicFlowEntity.category = Category_ScriptFlow
//         val views = SBViews()

//         def add_story_view {
//           val view = SBView()
//           view.title = "物語"
//           views.addChild(view)
//           val ul = SBUnorderedList()
//           view.addChild(ul)
//           for (task <- aStoryObject.basicFlow) {
//             task.traverse(new GTreeVisitor[SMElement] {
//               override def enter(aNode: GTreeNode[SMElement]) {
//                 aNode.content match {
//                   case step: SMActionStep        => enter_action_step(step)
//                   case step: SMExecutionStep     => enter_execution_step(step)
//                   case step: SMInvocationStep    => enter_invocation_step(step)
//                   case step: SMUsecaseStep       => enter_usecase_step(step)
//                   case step: SMExtendUsecaseStep => enter_extendUsecase_step(step)
//                 }
//               }

//               override def leave(aNode: GTreeNode[SMElement]) {
//                 aNode.content match {
//                   case step: SMActionStep        => leave_action_step(step)
//                   case step: SMExecutionStep     => leave_execution_step(step)
//                   case step: SMInvocationStep    => leave_invocation_step(step)
//                   case step: SMUsecaseStep       => leave_usecase_step(step)
//                   case step: SMExtendUsecaseStep => leave_extendUsecase_step(step)
//                 }
//               }

//               private def enter_action_step(aStep: SMActionStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_action_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_action_step(aStep: SMActionStep) {
//               }

//               private def enter_execution_step(aStep: SMExecutionStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_execution_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_execution_step(aStep: SMExecutionStep) {
//               }

//               private def enter_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_request_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_invocation_step(aStep: SMInvocationStep) {
//                 if (!aStep.isBidirectional) return
//                 val seq = aStep.leaveSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_response_text(aStep))
//                 ul.addChild(li)
//               }

//               private def enter_usecase_step(aStep: SMUsecaseStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_usecase_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_usecase_step(aStep: SMUsecaseStep) {
//               }

//               private def enter_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//                 def make_extend_usecase_list = {
//                   val uul = new SBUnorderedList()
//                   for ((value, usecase) <- aStep.dslExtendUsecases) {
//                     val uli = new SBListItem()
//                     uli.addChild(value + "の場合: ")
//                     uli.addChild(new SMTermRef(usecase))
//                     uul.addChild(uli)
//                   }
//                   uul
//                 }

//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = new SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild("拡張点: " + aStep.extensionPointName)
//                 li.addChild(make_extend_usecase_list)
//                 ul.addChild(li)
//               }

//               private def leave_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }
//             })
//           }
//         }

//         def add_collaboration_view {
//           val view = SBView()
//           view.title = "協調"
//           views.addChild(view)
//           val ul = SBUnorderedList()
//           view.addChild(ul)
//           for (task <- aStoryObject.basicFlow) {
//             task.traverse(new GTreeVisitor[SMElement] {
//               override def enter(aNode: GTreeNode[SMElement]) {
//                 aNode.content match {
//                   case step: SMActionStep        => enter_action_step(step)
//                   case step: SMExecutionStep     => enter_execution_step(step)
//                   case step: SMInvocationStep    => enter_invocation_step(step)
//                   case step: SMUsecaseStep       => enter_usecase_step(step)
//                   case step: SMExtendUsecaseStep => enter_extendUsecase_step(step)
//                 }
//               }

//               override def leave(aNode: GTreeNode[SMElement]) {
//               }

//               private def enter_action_step(aStep: SMActionStep) {
//                 val seq = aStep.layeredSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_action_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_action_step(aStep: SMActionStep) {
//               }

//               private def enter_execution_step(aStep: SMExecutionStep) {
//                 val seq = aStep.layeredSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_execution_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_execution_step(aStep: SMExecutionStep) {
//               }

//               private def enter_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.layeredSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_request_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_invocation_step(aStep: SMInvocationStep) {
//                 if (!aStep.isBidirectional) return
//                 val seq = aStep.layeredSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_response_text(aStep))
//                 ul.addChild(li)
//               }

//               private def enter_usecase_step(aStep: SMUsecaseStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_usecase_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_usecase_step(aStep: SMUsecaseStep) {
//               }

//               private def enter_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }

//               private def leave_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }
//             })
//           }
//         }

//         def add_data_view {
//           val view = SBView()
//           view.title = "データ"
//           views.addChild(view)
//           val ul = SBUnorderedList()
//           view.addChild(ul)
//           for (task <- aStoryObject.basicFlow) {
//             task.traverse(new GTreeVisitor[SMElement] {
//               override def enter(aNode: GTreeNode[SMElement]) {
//                 aNode.content match {
//                   case step: SMActionStep        => enter_action_step(step)
//                   case step: SMExecutionStep     => enter_execution_step(step)
//                   case step: SMInvocationStep    => enter_invocation_step(step)
//                   case step: SMUsecaseStep       => enter_usecase_step(step)
//                   case step: SMExtendUsecaseStep => enter_extendUsecase_step(step)
//                 }
//               }

//               override def leave(aNode: GTreeNode[SMElement]) {
//               }

//               private def enter_action_step(aStep: SMActionStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_action_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_action_step(aStep: SMActionStep) {
//               }

//               private def enter_execution_step(aStep: SMExecutionStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_execution_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_execution_step(aStep: SMExecutionStep) {
//               }

//               private def enter_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_request_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_response_text(aStep))
//                 ul.addChild(li)
//               }

//               private def enter_usecase_step(aStep: SMUsecaseStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_usecase_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_usecase_step(aStep: SMUsecaseStep) {
//               }

//               private def enter_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }

//               private def leave_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }
//             })
//           }
//         }

//         def add_task_view {
//           val view = SBView()
//           view.title = "タスク"
//           views.addChild(view)
//           val ul = SBUnorderedList()
//           view.addChild(ul)
//           for (task <- aStoryObject.basicFlow) {
//             task.traverse(new GTreeVisitor[SMElement] {
//               override def enter(aNode: GTreeNode[SMElement]) {
//                 aNode.content match {
//                   case step: SMActionStep        => enter_action_step(step)
//                   case step: SMExecutionStep     => enter_execution_step(step)
//                   case step: SMInvocationStep    => enter_invocation_step(step)
//                   case step: SMUsecaseStep       => enter_usecase_step(step)
//                   case step: SMExtendUsecaseStep => enter_extendUsecase_step(step)
//                 }
//               }

//               override def leave(aNode: GTreeNode[SMElement]) {
//               }

//               private def enter_action_step(aStep: SMActionStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_action_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_action_step(aStep: SMActionStep) {
//               }

//               private def enter_execution_step(aStep: SMExecutionStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_execution_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_execution_step(aStep: SMExecutionStep) {
//               }

//               private def enter_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_request_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_invocation_step(aStep: SMInvocationStep) {
//                 if (!aStep.isBidirectional) return
//                 val seq = aStep.sequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(step_response_text(aStep))
//                 ul.addChild(li)
//               }

//               private def enter_usecase_step(aStep: SMUsecaseStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_usecase_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_usecase_step(aStep: SMUsecaseStep) {
//               }

//               private def enter_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }

//               private def leave_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }
//             })
//           }
//         }

//         add_story_view
//         add_collaboration_view
//         add_data_view
//         add_task_view
//         basicFlowEntity.description = views
//         aStoryEntity.addSubEntity(basicFlowEntity)
//       }

//       def add_extensionFlow {
//         val extensionFlowEntity = new SDEntity("拡張脚本")
//         extensionFlowEntity.category = Category_ScriptFlow
//         val views = SBViews()

//         def add_story_view {
//           val view = SBView()
//           view.title = "物語"
//           views.addChild(view)
//           val segmentsUl = SBUnorderedList()
//           view.addChild(segmentsUl)
//           for (segment <- aStoryObject.extensionFlow) {
//             val li = SBListItem()
//             li.addChild(segment.name)
//             segmentsUl.addChild(li)
//             val ul = SBUnorderedList()
//             segmentsUl.addChild(ul)
//             segment.flow.traverse(new GTreeVisitor[SMStep] {
//               override def enter(aNode: GTreeNode[SMStep]) {
//                 aNode.content match {
//                   case step: SMBusinessTaskStep  => //
//                   case step: SMActionStep        => enter_action_step(step)
//                   case step: SMExecutionStep     => enter_execution_step(step)
//                   case step: SMInvocationStep    => enter_invocation_step(step)
//                   case step: SMParameterStep     => enter_parameter_step(step)
//                   case step: SMUsecaseStep       => enter_usecase_step(step)
//                   case step: SMExtendUsecaseStep => enter_extendUsecase_step(step)
//                 }
//               }

//               override def leave(aNode: GTreeNode[SMStep]) {
//                 aNode.content match {
//                   case step: SMBusinessTaskStep  => //
//                   case step: SMActionStep        => leave_action_step(step)
//                   case step: SMExecutionStep     => leave_execution_step(step)
//                   case step: SMInvocationStep    => leave_invocation_step(step)
//                   case step: SMParameterStep     => leave_parameter_step(step)
//                   case step: SMUsecaseStep       => leave_usecase_step(step)
//                   case step: SMExtendUsecaseStep => leave_extendUsecase_step(step)
//                 }
//               }

//               private def enter_action_step(aStep: SMActionStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_action_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_action_step(aStep: SMActionStep) {
//               }

//               private def enter_execution_step(aStep: SMExecutionStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_execution_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_execution_step(aStep: SMExecutionStep) {
//               }

//               private def enter_invocation_step(aStep: SMInvocationStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_request_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_invocation_step(aStep: SMInvocationStep) {
//                 if (!aStep.isBidirectional) return
//                 val seq = aStep.leaveSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_response_text(aStep))
//                 ul.addChild(li)
//               }

//               private def enter_parameter_step(aStep: SMParameterStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_parameter_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_parameter_step(aStep: SMParameterStep) {
//               }

//               private def enter_usecase_step(aStep: SMUsecaseStep) {
//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild(step_usecase_text(aStep))
//                 ul.addChild(li)
//               }

//               private def leave_usecase_step(aStep: SMUsecaseStep) {
//               }

//               private def enter_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//                 def make_extend_usecase_list = {
//                   val uul = new SBUnorderedList()
//                   for ((value, usecase) <- aStep.dslExtendUsecases) {
//                     val uli = new SBListItem()
//                     uli.addChild(value + "の場合: ")
//                     uli.addChild(new SMTermRef(usecase))
//                     uul.addChild(uli)
//                   }
//                   uul
//                 }

//                 val seq = aStep.enterSequenceNumber.toString
//                 val li = new SBListItem()
//                 li.addChild(seq + " ")
//                 li.addChild("拡張点: " + aStep.extensionPointName)
//                 li.addChild(make_extend_usecase_list)
//                 ul.addChild(li)
//               }

//               private def leave_extendUsecase_step(aStep: SMExtendUsecaseStep) {
//               }
//             })
//           }
//         }

//         add_story_view
//         extensionFlowEntity.description = views
//         aStoryEntity.addSubEntity(extensionFlowEntity)
//       }

//       def add_alternateFlow {
//         val altFlowEntity = new SDEntity("代替脚本")
//         altFlowEntity.category = Category_ScriptFlow
//         val ul = SBUnorderedList()

//         aStoryObject.alternateFlow.traverse(new GTreeVisitor[SMAlternatePath] {
//           override def enter(aNode: GTreeNode[SMAlternatePath]) {
//             val path = aNode.content
//             val li = SBListItem(make_path_text(path))
//             ul.addChild(li)
//           }

//           override def leave(aNode: GTreeNode[SMAlternatePath]) {
//           }

//           def make_path_text(aPath: SMAlternatePath): SDoc = {
//             aPath match {
//               case path: SMJumpPath      => make_path_text(path)
//               case path: SOmitPath       => error("not implemented yet")
//               case path: SSubstitutePath => error("not implemented yet")
//             }
//           }

//           def make_path_text(aPath: SMJumpPath): SDoc = {
//             val sdoc = SFragment()
//             val markRange = aPath.markRange
//             val mark = markRange._1 // XXX
//             if (mark == "") {
//               sdoc.addChild("???")
//             } else {
//               val seq = get_sequence_number(mark)
//               val label = seq + ".a." // XXX
//               sdoc.addChild(label)
//             }
//             sdoc.addChild(" " + aPath.condition + "の場合、")
//             val destination = aPath.destinationStep
//             if (mark == "") {
//               sdoc.addChild("???")
//             } else {
//               sdoc.addChild(destination.sequenceNumber + "へ移る")
//             } // XXX でもよい
//             sdoc
//           }
//         })
//         altFlowEntity.description = ul
//         aStoryEntity.addSubEntity(altFlowEntity)
//       }

//       def add_exceptionFlow {
//         val exceptionFlowEntity = new SDEntity("例外脚本")
//         exceptionFlowEntity.category = Category_ScriptFlow
//         val ul = SBUnorderedList()
//         aStoryObject.exceptionFlow.traverse(new GTreeVisitor[SMExceptionPath] {
//           override def enter(aNode: GTreeNode[SMExceptionPath]) {
//             val path = aNode.content.asInstanceOf[SMExceptionPath]
//             val li = SBListItem(make_path_text(path))
//             ul.addChild(li)
//           }
//           override def leave(aNode: GTreeNode[SMExceptionPath]) {
//           }

//           def make_path_text(aPath: SMExceptionPath): SDoc = {
//             def make_text_body = {
//               " " + aPath.condition + "の場合、終了"
//             }

//             val sdoc = SFragment()
//             val markRange = aPath.markRange
//             val mark = markRange._1 // XXX
//             if (mark == "") {
//               sdoc.addChild("???")
//             } else {
//               val seq = get_sequence_number(mark)
//               val label = seq + ".a." // XXX
//               sdoc.addChild(label)
//             }
//             sdoc.addChild(make_text_body)
//             sdoc
//           }
//         })
//         exceptionFlowEntity.description = ul
//         aStoryEntity.addSubEntity(exceptionFlowEntity)
//       }

//       add_basicFlow
//       add_extensionFlow
//       add_alternateFlow
//       add_exceptionFlow
//       aStoryEntity
//     }

//     // Flow (asakusa)
//     def add_flow(flow: SMFlow) {
//       val sd = add_object(flow)
//       sd
//     }

//     // Common
//     def add_object(anObject: SMObject): SDEntity = {
//       val objEntity = new SDEntity(anObject.name)

//       def add_overview {
//         if (!drawDiagram) return
//         if (classClassDiagramThemes.isEmpty) return
//         val generator = new ClassDiagramGenerator(simpleModel)
//         val views = SBViews()

//         def build_hilight {
//           val binary = generator.makeClassDiagramPng(anObject, "hilight")
//           val figure = new SBBinaryContentFigure(binary)
//           figure.src = "ClassDiagram" + anObject.name + "Hilight.png"
//           val view = SBView()
//           view.title = "概要"
//           view.addChild(figure)
//           views.addChild(view)
//         }

//         def build_perspective {
//           val binary = generator.makeClassDiagramPng(anObject, "perspective")
//           val figure = new SBBinaryContentFigure(binary)
//           figure.src = "ClassDiagram" + anObject.name + "Perspective.png"
//           val view = SBView()
//           view.title = "詳細"
//           view.addChild(figure)
//           views.addChild(view)
//         }

//         for (theme <- classClassDiagramThemes) {
//           theme match {
//             case "hilight"     => build_hilight
//             case "perspective" => build_perspective
//             case _             => error("parameter error: " + theme)
//           }
//         }
//         if (views.length == 1) {
//           objEntity.overview = views.getChild(0).getChild(0)
//         } else {
//           objEntity.overview = views
//         }
//       }

//       def add_relationships(aCategory: SDCategory)(aCollect: SMObject => Seq[SMRelationship])(aSetup: (SDEntity, SMRelationship) => Unit) {
//         def add_relationship(aRel: SMRelationship): SDEntity = {
//           val relEntity = new SDEntity(aRel.name)
//           relEntity.category = aCategory
//           relEntity.resume.copyIn(aRel.resume)
//           relEntity.description = aRel.description
//           relEntity.note = aRel.note
//           for (feature <- aRel.features) {
//             relEntity.addFeature(feature.key, feature.value) // XXX description_is
//           }
//           aSetup(relEntity, aRel)
//           objEntity.addSubEntity(relEntity)
//           relEntity
//         }

//         def add_derived_relationship(aRel: SMRelationship, anOwner: SMObject) {
//           val relEntity = add_relationship(aRel)
//           relEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//         }

//         def add_derived_relationships {
//           var mayParent = anObject.getBaseObject
//           while (mayParent.isDefined) {
//             val parent = mayParent.get
//             aCollect(parent).foreach(add_derived_relationship(_, parent))
//             mayParent = parent.getBaseObject
//           }
//         }

//         def add_own_relationships {
//           aCollect(anObject).foreach(add_relationship)
//         }

//         add_own_relationships
//         add_derived_relationships
//       }

//       // role
//       def add_roles {
//         add_relationships(Category_RoleRelationship) {
//           (anObj: SMObject) => anObj.roles
//         } {
//           (anEntity: SDEntity, aRel: SMRelationship) =>
//             val role = aRel.asInstanceOf[SMRoleRelationship]
//             anEntity.addFeature(FeatureType, role.targetTypeLiteral)
//         }
//       }

//       // service
//       def add_services {
//         add_relationships(Category_ServiceRelationship) {
//           (anObj: SMObject) => anObj.services
//         } {
//           (anEntity: SDEntity, aRel: SMRelationship) =>
//             val service = aRel.asInstanceOf[SMServiceRelationship]
//             anEntity.addFeature(FeatureType, service.targetTypeLiteral)
//         }
//       }

//       // rule
//       def add_rules {
//         add_relationships(Category_RuleRelationship) {
//           (anObj: SMObject) => anObj.rules
//         } {
//           (anEntity: SDEntity, aRel: SMRelationship) =>
//             val rule = aRel.asInstanceOf[SMRuleRelationship]
//             anEntity.addFeature(FeatureType, rule.targetTypeLiteral)
//         }
//       }

//       // powertype
//       def add_powertypes {
//         add_relationships(Category_PowertypeRelationship) {
//           (anObj: SMObject) => anObj.powertypes
//         } {
//           (anEntity: SDEntity, aRel: SMRelationship) =>
//             val powertype = aRel.asInstanceOf[SMPowertypeRelationship]
//             anEntity.addFeature(FeatureType, powertype.targetTypeLiteral)
//         }
//       }

//       // attribute
//       def add_attribute(anAttr: SMAttribute): SDEntity = {
//         val attrEntity = new SDEntity(anAttr.name)
//         attrEntity.category = Category_Attribute
//         attrEntity.resume.copyIn(anAttr.resume)
//         attrEntity.description = anAttr.description
//         attrEntity.note = anAttr.note
//         for (feature <- anAttr.features) {
//           attrEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(attrEntity)
//         attrEntity
//       }

//       def add_derived_attribute(anAttr: SMAttribute, anOwner: SMObject) {
//         val attrEntity = add_attribute(anAttr)
//         attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_attributes {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.attributes.foreach(add_derived_attribute(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_attributes {
//         anObject.attributes.foreach(add_attribute)
//       }

//       // association
//       def add_association(anAssoc: SMAssociation): SDEntity = {
//         val assocEntity = new SDEntity(anAssoc.name)
//         assocEntity.category = Category_Association
//         assocEntity.resume.copyIn(anAssoc.resume)
//         assocEntity.description = anAssoc.description
//         assocEntity.note = anAssoc.note
//         for (feature <- anAssoc.features) {
//           assocEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(assocEntity)
//         assocEntity
//       }

//       def add_derived_association(anAssoc: SMAssociation, anOwner: SMObject) {
//         val attrEntity = add_association(anAssoc)
//         attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_associations {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.associations.foreach(add_derived_association(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_associations {
//         anObject.associations.foreach(add_association)
//       }

//       // operation
//       def add_operation(anOperation: SMOperation): SDEntity = {
//         val operationEntity = new SDEntity(anOperation.name)
//         operationEntity.category = Category_Operation
//         operationEntity.resume.copyIn(anOperation.resume)
//         operationEntity.description = anOperation.description
//         operationEntity.note = anOperation.note
//         for (feature <- anOperation.features) {
//           operationEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(operationEntity)
//         operationEntity
//       }

//       def add_derived_operation(anOper: SMOperation, anOwner: SMObject) {
//         val attrEntity = add_operation(anOper)
//         attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_operations {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.operations.foreach(add_derived_operation(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_operations {
//         anObject.operations.foreach(add_operation)
//       }

//       // stateMachine
//       def add_stateMachine(aStateMachine: SMStateMachineRelationship): SDEntity = {
//         val sm = aStateMachine.statemachine
//         val stateMachineEntity = new SDEntity(aStateMachine.name)

//         def add_states {
//           val spec = new SBDivision()
//           if (drawDiagram) {
//             val generator = new StateMachineDiagramGenerator(simpleModel)
//             val binary = generator.makeStateMachineDiagramPng(aStateMachine.statemachine)
//             val figure = new SBBinaryContentFigure(binary)
// //            figure.src = "StateMachineDiagram" + aStateMachine.ownerObject.name + aStateMachine.name + ".png"
//             figure.src = "StateMachineDiagram" + sm.name + ".png"
//             spec.addChild(figure)
//           }
//           //
//           val table = SBTable()
//           table.title = "状態遷移表"
//           sm.buildStateTransitionTable(table.table)
//           spec.addChild(table)
//           //
//           stateMachineEntity.specification = spec
//         }

//         stateMachineEntity.category = Category_StateMachine
//         stateMachineEntity.resume.copyIn(aStateMachine.resume)
//         stateMachineEntity.description = aStateMachine.description
//         stateMachineEntity.note = aStateMachine.note
//         for (feature <- aStateMachine.features) {
//           stateMachineEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(stateMachineEntity)
//         add_states
//         stateMachineEntity
//       }

//       def add_derived_stateMachine(aStateMachine: SMStateMachineRelationship, anOwner: SMObject) {
//         val smEntity = add_stateMachine(aStateMachine)
//         smEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_stateMachines {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.stateMachines.foreach(add_derived_stateMachine(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_stateMachines {
//         anObject.stateMachines.foreach(add_stateMachine)
//       }

//       // transition
//       def add_transition(aTransition: SMTransition): SDEntity = {
//         val transitionEntity = new SDEntity(aTransition.name)
//         transitionEntity.category = Category_StateTransition
//         transitionEntity.resume.copyIn(aTransition.resume)
//         transitionEntity.description = aTransition.description
//         transitionEntity.note = aTransition.note
//         transitionEntity.addFeature(FeatureType, aTransition.resourceLiteral)
//         transitionEntity.addFeature(FeaturePreState, aTransition.preStateLiteral)
//         transitionEntity.addFeature(FeatureGuard, aTransition.guardLiteral)
//         transitionEntity.addFeature(FeaturePostState, aTransition.postStateLiteral)
//         for (feature <- aTransition.features) {
//           transitionEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         objEntity.addSubEntity(transitionEntity)
//         transitionEntity
//       }

//       def add_derived_transition(aTransition: SMTransition, anOwner: SMObject) {
//         val transEntity = add_transition(aTransition)
//         transEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_transitions {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           mayParent.get match {
//             case parent: SMDomainEvent => {
//               parent.resourceTransitions.foreach(add_derived_transition(_, parent))
//               mayParent = parent.getBaseObject
//             }
//             case _ => mayParent = None
//           }
//         }
//       }

//       def add_own_transitions {
//         anObject match {
//           case event: SMDomainEvent => event.resourceTransitions.foreach(add_transition)
//           case _                    => //
//         }
//       }

//       // flowMachine
//       def add_flowMachine(aFlowMachine: SMFlowMachine): SDEntity = {
//         val flowMachineEntity = new SDEntity(aFlowMachine.name)

//         def add_flows {
//           val spec = new SBDivision()
//           if (drawDiagram) {
//             val generator = new FlowMachineDiagramGenerator(simpleModel)
//             val binary = generator.makeFlowMachineDiagramPng(aFlowMachine)
//             val figure = new SBBinaryContentFigure(binary)
//             figure.src = "FlowMachineDiagram" + aFlowMachine.ownerObject.name + aFlowMachine.name + ".png"
//             spec.addChild(figure)
//           }
//           //
//           flowMachineEntity.specification = spec
//         }

//         flowMachineEntity.category = Category_FlowMachine
//         flowMachineEntity.resume.copyIn(aFlowMachine.resume)
//         flowMachineEntity.description = aFlowMachine.description
//         flowMachineEntity.note = aFlowMachine.note
//         for (feature <- aFlowMachine.features) {
//           flowMachineEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(flowMachineEntity)
//         add_flows
//         flowMachineEntity
//       }

//       def add_flow {
//         anObject match {
//           case flow: SMFlow => {
//             add_flowMachine(flow.flowMachine)
//           }
//           case _ => ;
//         }
//       }

//       // use
//       def add_use(anUse: SMUse): SDEntity = {
//         val useEntity = new SDEntity(anUse.name)
//         useEntity.category = Category_Use
//         useEntity.resume.copyIn(anUse.resume)
//         useEntity.description = anUse.description
//         useEntity.note = anUse.note
//         useEntity.addFeature(FeatureName, anUse.name)
//         useEntity.addFeature(FeatureClass, anUse.elementLiteral)
//         useEntity.addFeature(FeatureUseKind, anUse.useKindLiteral)
//         useEntity.addFeature(FeatureUserClass, anUse.userLiteral)
//         useEntity.addFeature(FeatureReceiverClass, anUse.receiverLiteral)
//         for (feature <- anUse.features) {
//           useEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(useEntity)
//         useEntity
//       }

//       def add_derived_use(aUse: SMUse, anOwner: SMObject) {
//         val useEntity = add_use(aUse)
//         useEntity.addFeature(FeatureOwnerClass, object_literal(anOwner)) // XXX
//       }

//       def add_derived_uses {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.uses.foreach(add_derived_use(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_uses {
//         anObject.uses.foreach(add_use)
//       }

//       // participation
//       def add_participation(aParticipation: SMParticipation): SDEntity = {
//         val participationEntity = new SDEntity(aParticipation.name)
//         participationEntity.category = Category_Participation
//         participationEntity.resume.copyIn(aParticipation.resume)
//         participationEntity.description = aParticipation.description
//         participationEntity.note = aParticipation.note
//         participationEntity.addFeature(FeatureName, aParticipation.name)
//         participationEntity.addFeature(FeatureElement, aParticipation.elementLiteral)
//         participationEntity.addFeature(FeatureType, aParticipation.elementTypeLiteral)
//         participationEntity.addFeature(FeatureKind, aParticipation.elementKindLiteral)
//         participationEntity.addFeature(FeatureRoleName, aParticipation.roleName)
//         participationEntity.addFeature(FeatureRoleType, aParticipation.roleTypeLiteral)
//         for (feature <- aParticipation.features) {
//           participationEntity.addFeature(feature.key, feature.value) // XXX description_is
//         }
//         // XXX
//         objEntity.addSubEntity(participationEntity)
//         participationEntity
//       }

//       def add_derived_participation(aParticipation: SMParticipation, anOwner: SMObject) {
//         val participationEntity = add_participation(aParticipation)
//         participationEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
//       }

//       def add_derived_participations {
//         var mayParent = anObject.getBaseObject
//         while (mayParent.isDefined) {
//           val parent = mayParent.get
//           parent.participations.foreach(add_derived_participation(_, parent))
//           mayParent = parent.getBaseObject
//         }
//       }

//       def add_own_participations {
//         anObject.participations.foreach(add_participation)
//       }

//       def object_literal(anObj: SMObject) = {
//         new SIAnchor(SText(anObj.name)) unresolvedRef_is new SElementRef(anObj.packageName, anObj.name)
//       }

//       def object_category = {
//         if (anObject.isInstanceOf[SMComponent]) Category_Component
//         else if (anObject.isInstanceOf[SMEntity]) Category_Entity
//         else if (anObject.isInstanceOf[SMValue]) Category_Value
//         else if (anObject.isInstanceOf[SMDocument]) Category_Document
//         else if (anObject.isInstanceOf[SMService]) Category_Service
//         else if (anObject.isInstanceOf[SMRule]) Category_Rule
//         else if (anObject.isInstanceOf[SMPowertype]) Category_Powertype
//         else if (anObject.isInstanceOf[SMDatatype]) Category_Datatype
//         else if (anObject.isInstanceOf[SMBusinessUsecase]) Category_BusinessUsecase
//         else if (anObject.isInstanceOf[SMBusinessTask]) Category_BusinessTask
//         else if (anObject.isInstanceOf[SMRequirementUsecase]) Category_RequirementUsecase
//         else if (anObject.isInstanceOf[SMRequirementTask]) Category_RequirementTask
//         else if (anObject.isInstanceOf[SMTransition]) Category_StateTransition
//         else if (anObject.isInstanceOf[SMFlow]) Category_Flow
//         else error("Unknown category = " + anObject)
//       }

//       specPackage.addEntity(objEntity)
//       objEntity.category = object_category
//       objEntity.categories ++= entityCategories // XXX move individual settings
//       objEntity.sdocTitle = SText(anObject.name)
//       objEntity.caption = anObject.caption
//       objEntity.brief = anObject.brief
//       objEntity.summary = anObject.summary
//       objEntity.description = anObject.description
//       objEntity.note = anObject.note
//       for (feature <- anObject.features) {
//         objEntity.addFeature(feature.key, feature.value) // XXX description_is
//       }
//       add_overview
//       add_roles
//       add_services
//       add_rules
//       add_powertypes
//       add_derived_attributes
//       add_own_attributes
//       add_derived_associations
//       add_own_associations
//       add_derived_operations
//       add_own_operations
//       add_derived_stateMachines
//       add_own_stateMachines
//       add_derived_transitions
//       add_own_transitions
//       add_flow
//       add_derived_uses
//       add_own_uses
//       add_derived_participations
//       add_own_participations
//       objEntity.history.copyIn(anObject.history)
//       objEntity
//     }

//     def add_glossary() {
//       val glossary = new SDSummary("glossary")
//       specPackage.addSummary(glossary)
//       glossary.title = "用語集"
//       glossary.tableHead = List("用語", "説明", "オブジェクト", "種類")
//       glossary.tableRow = (anEntity: SDEntity) =>
//         Array(
//           anEntity.feature(FeatureTerm).value,
//           anEntity.summary,
//           anEntity.anchor,
//           anEntity.feature(FeatureType).value)
//     }

//     specPackage.categories ++= packageCategories
//     specPackage.sdocTitle = SText(modelPackage.name)
//     specPackage.caption = modelPackage.caption
//     specPackage.brief = modelPackage.brief
//     specPackage.summary = modelPackage.summary
//     specPackage.description = modelPackage.description
//     specPackage.note = modelPackage.note
//     for (feature <- modelPackage.features) {
//       specPackage.addFeature(feature.key, feature.value) // XXX description_is
//     }
//     add_overview
//     modelPackage.domainActors.foreach(add_actor)
//     modelPackage.domainRoles.foreach(add_role)
//     modelPackage.domainResources.foreach(add_resource)
//     modelPackage.domainEvents.foreach(add_event)
//     modelPackage.domainSummaries.foreach(add_summary)
//     modelPackage.domainEntities.foreach(add_entity)
//     modelPackage.domainIds.foreach(add_id)
//     modelPackage.domainNames.foreach(add_name)
//     modelPackage.domainValues.foreach(add_value)
//     modelPackage.domainDocuments.foreach(add_document)
//     modelPackage.domainServices.foreach(add_service)
//     modelPackage.domainRules.foreach(add_rule)
//     modelPackage.domainPowertypes.foreach(add_powertype)
//     modelPackage.datatypes.foreach(add_datatype)
//     modelPackage.businessUsecases.foreach(add_businessUsecase)
//     modelPackage.businessTasks.foreach(add_businessTask)
//     modelPackage.requirementUsecases.foreach(add_requirementUsecase)
//     modelPackage.requirementTasks.foreach(add_requirementTask)
//     modelPackage.systemComponents.foreach(add_component)
//     modelPackage.flows.foreach(add_flow)
//     add_glossary()
//   }
}

object SpecDocTransformer {
  case class Config(
    drawDiagram: Boolean = true,
    classClassDiagramThemes: List[Theme] = List(Theme.Hilight, Theme.Perspective),
    packageClassDiagramThemes: List[Theme] = List(Theme.Hilight, Theme.Detail)
  )

  sealed trait Theme extends NamedValueInstance
  object Theme extends EnumerationClass[Theme] {
    val elements = Vector(Hilight, Perspective, Detail)
    case object Hilight extends Theme {
      val name = "hilight"
    }
    case object Perspective extends Theme {
      val name = "perspective"
    }
    case object Detail extends Theme {
      val name = "detail"
    }
  }
}
