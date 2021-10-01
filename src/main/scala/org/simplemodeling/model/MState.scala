package org.simplemodeling.model

import org.smartdox.Description
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.collection.VectorMap
import org.goldenport.values.Designation
import org.goldenport.values.PathName

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
 *  version Aug.  1, 2020
 *  version Jun. 20, 2021
 *  version Jul. 11, 2021
 * @version Aug.  2, 2021
 * @author  ASAMI, Tomoharu
 */
class MState(
  val description: Description,
  val ownerStateMachine: MStateMachine,
  val parentState: Option[MState] = None
) extends MElement {
  def getAffiliation = None
//   val transitions = dslState.transitions.map(new SMTransition(_, ownerStateMachine))
  var transitions: List[MTransition] = Nil
  var subStateMap = VectorMap.empty[String, MState]
  lazy val subStates = subStateMap.values.toList

  var historyState: Option[MState] = None

//   for ((name, dslSubState) <- dslState.subStateMap) {
//     val qName = dslSubState.qualifiedName
//     val refinedDslSubState = SStateRepository.getState(qName)
//     val state = new SMState(refinedDslSubState, ownerStateMachine)
//     ownerStateMachine.wholeStateMap += (qName -> state)
//     subStateMap += (name -> state) // owner composition state
//   }

  val value: Either[String, Int] = Right(0) // TODO

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

  final def isTerminal = transitions.isEmpty || isComposite // TODO more precise

  final def isComposite = {
    !subStateMap.isEmpty
  }

  // TODO path
  def getSubStateRecursive(name: String): Option[MState] =
    subStateMap.get(name) orElse _get_sub_state_recursive(PathName(name))

  private def _get_sub_state_recursive(pn: PathName): Option[MState] = {
    val x = pn.components match {
      case Nil => None
      case x :: Nil => None // already checked
      case xs => pn.components.lastOption.flatMap(subStateMap.get)
    }
    x orElse  subStateMap.values.toStream.flatMap(_.getSubStateRecursive(name)).headOption
  }

  def getSubStateRecursive(pn: PathName): Option[MState] =
    subStateMap.get(pn.v) orElse _get_sub_state_recursive(PathName(name))

  def createHistoryState: MState = {
    val hs = MState.historyState(ownerStateMachine, this)
    historyState = Some(hs)
    hs
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

object MState {
  val initStateName = "init"
  val finalStateName = "final"
  val historyStateName = "history"

  def initState(sm: MStateMachine) = create(sm, initStateName)
  def finalState(sm: MStateMachine) = create(sm, finalStateName)
  def historyState(sm: MStateMachine, parent: MState) = {
    val desc = Description.name(historyStateName)
    new MState(desc, sm, Some(parent))
  }

  def create(sm: MStateMachine, name: String): MState = {
    val desc = Description.name(name)
    new MState(desc, sm)
  }
}
