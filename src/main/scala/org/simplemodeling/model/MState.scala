package org.simplemodeling.model

import scala.collection.mutable.LinkedHashMap
import org.smartdox.Description
import org.goldenport.i18n.I18NString
import org.goldenport.values.Designation

/*
 * Derived from SState and SMState.
 * 
 * @since   Dec. 20, 2008
 *  version Mar. 19, 2009
 *  version Nov. 26, 2012
 *  version Dec.  6, 2012
 *  version Feb.  6, 2013
 *  version Jan.  5, 2020
 *  version Mar.  3, 2020
 *  version Apr. 25, 2020
 *  version May.  6, 2020
 *  version Jun. 13, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
case class MState(
  designation: Designation,
  ownerStateMachine: MStateMachine,
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = None
//   val transitions = dslState.transitions.map(new SMTransition(_, ownerStateMachine))
  val transitions: List[MTransition] = ???
  val subStateMap = new LinkedHashMap[String, MState]
  lazy val subStates = subStateMap.values.toList
//   for ((name, dslSubState) <- dslState.subStateMap) {
//     val qName = dslSubState.qualifiedName
//     val refinedDslSubState = SStateRepository.getState(qName)
//     val state = new SMState(refinedDslSubState, ownerStateMachine)
//     ownerStateMachine.wholeStateMap += (qName -> state)
//     subStateMap += (name -> state) // owner composition state
//   }

  val value: Either[String, Int] = ???

//   val value: Either[String, Int] = {
//     val v = dslState.value | name
//     val r = if (StringUtils.isBlank(v)) {
//       name.left
//     } else {
//       v.parseInt.fold(_ => name.left, _.right)
//     }
// //    println("SMState: " + dslState + "/" + r)
//     r
//   }

//   def lifecycle: Option[String] = dslState.lifecycle

//   override protected def qualified_Name = {
// //     println("state qname = " + dslState.qualifiedName) 2009-03-19
//     Some(dslState.qualifiedName)
//   }

  final def isTerminal = {
    ??? // dslState.transitions.isEmpty
  }

  final def isComposite = {
    !subStateMap.isEmpty
  }

//   override def toString() = {
//     "SMState(%s/%s/%s)".format(name, value, lifecycle)
//   }

// /*
//   final def getEventsWithGuardActions: LinkedHashMap[SMObject, Seq[(SMGuard, SMAction)]] = {
//     val eventMap = new LinkedHashMap[SMObject, ArrayBuffer[(SMGuard, SMAction)]]
//     for (transition <- transitions) {
//       val event = transition.event
//       eventMap.get(event) match {
// 	case Some(guardActions: ArrayBuffer[(SMGuard, SMAction)]) => {
// 	  guardActions += ((transition.guard, transition.action))
// 	}
// 	case None => {
// 	  val buf = new ArrayBuffer[(SMGuard, SMAction)]
// 	  buf += ((transition.guard, transition.action))
// 	  eventMap.put(event, buf)
// 	}
//       }
//     }
//     eventMap.asInstanceOf[LinkedHashMap[SMObject, Seq[(SMGuard, SMAction)]]]
//   }

//   final def getEventGuardActions: Seq[(SMObject, SMGuard, SMAction)] = {
//     for ((event, guardActions) <- getEventsWithGuardActions.elements.toList;
// 	 (guard, action) <- guardActions) yield (event, guard, action)
//   }
// */
}
