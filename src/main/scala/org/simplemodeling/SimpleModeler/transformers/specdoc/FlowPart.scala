package org.simplemodeling.SimpleModeler.transformers.specdoc

import org.smartdox.{Dox, Text, Fragment, Ul, Li}
import org.simplemodeling.model._

/*
 * @since   Jul. 27, 2020
 * @version Jul. 27, 2020
 * @author  ASAMI, Tomoharu
 */
trait FlowPart { SpecDocTransformer =>
  protected final def build_flow(ps: Seq[MStep]): List[Li] = {
    ???
        //   for (task <- aStoryObject.basicFlow) {
        //     task.traverse(new GTreeVisitor[MElement] {
        //       override def enter(aNode: GTreeNode[MElement]) {
        //         aNode.content match {
        //           case step: MActionStep        => enter_action_step(step)
        //           case step: MExecutionStep     => enter_execution_step(step)
        //           case step: MInvocationStep    => enter_invocation_step(step)
        //           case step: MUsecaseStep       => enter_usecase_step(step)
        //           case step: MExtendUsecaseStep => enter_extendUsecase_step(step)
        //         }
        //       }

        //       override def leave(aNode: GTreeNode[MElement]) {
        //         aNode.content match {
        //           case step: MActionStep        => leave_action_step(step)
        //           case step: MExecutionStep     => leave_execution_step(step)
        //           case step: MInvocationStep    => leave_invocation_step(step)
        //           case step: MUsecaseStep       => leave_usecase_step(step)
        //           case step: MExtendUsecaseStep => leave_extendUsecase_step(step)
        //         }
        //       }

        //       private def enter_action_step(aStep: MActionStep) {
        //         val seq = aStep.enterSequenceNumber.toString
        //         val li = SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild(step_action_text(aStep))
        //         xs.add(li)
        //       }

        //       private def leave_action_step(aStep: MActionStep) {
        //       }

        //       private def enter_execution_step(aStep: MExecutionStep) {
        //         val seq = aStep.enterSequenceNumber.toString
        //         val li = SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild(step_execution_text(aStep))
        //         ul.addChild(li)
        //       }

        //       private def leave_execution_step(aStep: MExecutionStep) {
        //       }

        //       private def enter_invocation_step(aStep: MInvocationStep) {
        //         val seq = aStep.enterSequenceNumber.toString
        //         val li = SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild(step_request_text(aStep))
        //         ul.addChild(li)
        //       }

        //       private def leave_invocation_step(aStep: MInvocationStep) {
        //         if (!aStep.isBidirectional) return
        //         val seq = aStep.leaveSequenceNumber.toString
        //         val li = SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild(step_response_text(aStep))
        //         ul.addChild(li)
        //       }

        //       private def enter_usecase_step(aStep: MUsecaseStep) {
        //         val seq = aStep.enterSequenceNumber.toString
        //         val li = SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild(step_usecase_text(aStep))
        //         ul.addChild(li)
        //       }

        //       private def leave_usecase_step(aStep: MUsecaseStep) {
        //       }

        //       private def enter_extendUsecase_step(aStep: MExtendUsecaseStep) {
        //         def make_extend_usecase_list = {
        //           val uul = new SBUnorderedList()
        //           for ((value, usecase) <- aStep.dslExtendUsecases) {
        //             val uli = new SBListItem()
        //             uli.addChild(value + "の場合: ")
        //             uli.addChild(new MTermRef(usecase))
        //             uul.addChild(uli)
        //           }
        //           uul
        //         }

        //         val seq = aStep.enterSequenceNumber.toString
        //         val li = new SBListItem()
        //         li.addChild(seq + " ")
        //         li.addChild("拡張点: " + aStep.extensionPointName)
        //         li.addChild(make_extend_usecase_list)
        //         ul.addChild(li)
        //       }

        //       private def leave_extendUsecase_step(aStep: MExtendUsecaseStep) {
        //       }
        //     })
        //   }
        // }
  }
}
