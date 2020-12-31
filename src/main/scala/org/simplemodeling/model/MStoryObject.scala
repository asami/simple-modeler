package org.simplemodeling.model

import org.smartdox._

/*
 * Derived from SStoryObject and SMStoryObject.
 * 
 * @since   Nov.  6, 2008
 *  version Dec.  7, 2008
 *  version Jan. 18, 2009
 *  version Apr. 17, 2011
 *  version Nov.  4, 2011
 *  version Nov. 18, 2012
 *  version May.  6, 2020
 * @version Jul. 27, 2020
 * @author  ASAMI, Tomoharu
 */
trait MStoryObject extends MObject {
  def basicFlow: List[MStep] = ???
  def includedStories(implicit sm: SimpleModel): List[MStoryObject] = ???
  def usedEntities(implicit sm: SimpleModel): List[MEntity] = ???

  final def getStep(aMark: String): Option[MStep] = {
    // _stepByMark.get(aMark)
    ???
  }

  final def getPrimaryActorTerm(aStep: MStep): Dox = {
    // if (aStep.isPrimaryActor) {
    //   aStep.getPrimaryActorTerm
    // } else {
    //   aStep.primaryActorKind match {
    //     case BusinessClientActorKind => _business_client_term()
    //     case BusinessWorkerActorKind => _business_worker_term()
    //     case SystemClientActorKind => _system_client_term()
    //     case SystemUnderDiscussionActorKind => _system_under_discussion_term()
    //     case _ => "Unknown"
    //   }
    // }
    ???
  }

  final def getSecondaryActorTerm(aStep: MStep): Dox = {
    // if (aStep.isSecondaryActor) {
    //   aStep.getSecondaryActorTerm
    // } else {
    //   aStep.secondaryActorKind match {
    //     case BusinessClientActorKind => _business_client_term()
    //     case BusinessWorkerActorKind => _business_worker_term()
    //           case SystemClientActorKind => _system_client_term()
    //           case SystemUnderDiscussionActorKind => _system_under_discussion_term()
    //           case _ => "Unknown"
    //   }
    // }
    ???
  }

  def traverse() {
    ???
  }
}
