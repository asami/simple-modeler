package org.simplemodeling.SimpleModeler.generators.uml

// import scala.collection.mutable.{ArrayBuffer, HashMap}
// import org.goldenport.entity.content._
// import org.goldenport.entity.GEntityContext
// import org.goldenport.entities.graphviz._
// import org.simplemodeling.SimpleModeler.entity._

import scala.collection.mutable.HashMap
import org.goldenport.RAISE
import org.goldenport.graphviz._
import org.goldenport.bag.ChunkBag
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.Context
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * https://www.colordic.org/w
 * 
 * @since   Jan. 28, 2009
 *  version Mar. 19, 2009
 *  version Sep. 18, 2012
 *  version May. 10, 2020
 *  version Jul. 12, 2021
 * @version Aug.  2, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachineDiagramGenerator(
  context: Context,
  implicit val sm: SimpleModel
) extends DiagramGeneratorBase(context, sm) {
//  final def makeStateMachineDiagramPng(anObject: SMObject): BinaryContent = {
//    make_statemachine_diagram_png(makeStateMachineDiagramDot(anObject))
//  }

  final def makeStateMachineDiagramPng(aStateMachine: MStateMachine): ChunkBag = {
    make_statemachine_diagram_png(makeStateMachineDiagramDot(aStateMachine))
  }

  final def makeStateMachineDiagramSvg(aStateMachine: MStateMachine): ChunkBag = {
    make_statemachine_diagram_svg(makeStateMachineDiagramDot(aStateMachine))
  }

  final def makeStateMachineDiagramWebp(aStateMachine: MStateMachine): ChunkBag = {
    make_statemachine_diagram_webp(makeStateMachineDiagramDot(aStateMachine))
  }

/*
  def stream2Bytes(in: java.io.InputStream) = { 2009-03-18
    val out = new java.io.ByteArrayOutputStream
    try {
      val buffer = new Array[byte](8192)
      var done = false
      while (!done) {
        println("before read")
        val size = in.read(buffer)
        println("size = " + size)
        if (size == -1) done = true
        else {
          out.write(buffer, 0, size)
        }
      }
      out.flush()
      out.toByteArray()
    } finally {
      out.close()
    }
  }
*/

  private def make_statemachine_diagram_png(text: String): ChunkBag = {
    make_diagram_png(text)
  }

  private def make_statemachine_diagram_svg(text: String): ChunkBag = {
    make_diagram_svg(text)
  }

  private def make_statemachine_diagram_webp(text: String): ChunkBag = {
    make_diagram_webp(text)
  }
/*
  private def make_statemachine_diagram_png(text: StringContent): BinaryContent = {
    val dot: Process = context.executeCommand("dot -Tpng -Kdot -q")
    val in = dot.getInputStream()
    val out = dot.getOutputStream()
//    println("start process = " + dot)
    try {
//      println("dot = " + text.string)
      text.write(out)
      out.flush
      out.close
//      val bytes = stream2Bytes(in) 2009-03-18
//      println("bytes = " + bytes.length)
//      new BinaryContent(bytes, context)
      new BinaryContent(in, context)
    } finally {
      in.close
//      err.close
//      println("finish process = " + dot)
    }
  }
*/

  final def makeStateMachineDiagramDot(aStateMachine: MStateMachine): String = {
    val graphviz = new Graphviz()
    // graphviz.open
    val graph = new GraphStateMachine(graphviz.graph, context, sm)
    var counter = 1
    val stateIds = new HashMap[MState, String]

    def make_id = {
      val id = "state" + counter
      counter += 1
      id
    }

    def make_subgraph_id = {
      val id = "cluster_state" + counter
      counter += 1
      id
    }

    def get_state_id(aState: MState) = {
      if (aState.isComposite) {
        stateIds(aState) + "_start"
      } else {
        stateIds(aState)
      }
    }

    def get_state_id_from(aState: MState) = {
      if (aState.isComposite) {
        stateIds(aState) + "_end"
      } else {
        stateIds(aState)
      }
    }

    def get_state_id_to(aState: MState) = {
      if (aState.isComposite) {
        stateIds(aState) + "_start"
      } else if (aState.name == "history") {
        s"${stateIds(aState.parentState.get)}_history"
      } else {
        stateIds(aState)
      }
    }

    def build_composite_state(aState: MState, aCompositeState: GraphCompositeState) {
      require (aState.isComposite)
      val usehistory = aState.historyState.isDefined
      val subStates = aState.subStates
      if (subStates.isEmpty) return
      val startId = aCompositeState.startId // XXX
      val endId = aCompositeState.endId // XXX
      val historyId = aCompositeState.historyId // XXX
      aCompositeState.addStart
      if (usehistory)
        aCompositeState.addHistory
      for (state <- subStates) {
        if (state.isComposite) {
          val id = make_subgraph_id
          stateIds += (state -> id)
          val compositeState = aCompositeState.addCompositeState(state, id)
          build_composite_state(state, compositeState)
        } else {
          val id = make_id
          stateIds += (state -> id)
          aCompositeState.addState(state, id)
        }
      }
      aCompositeState.addPseudoTransition(startId, get_state_id_to(subStates(0)))
      if (usehistory)
        aCompositeState.addPseudoTransition(historyId, get_state_id_to(subStates(0)))
      for (state <- subStates) {
        if (state.isTerminal) {
          aCompositeState.addPseudoTransition(get_state_id_from(state), endId)
        }
      }
      aCompositeState.addEnd
    }

    def build_composite_state_transitions(aState: MState) {
      for (state <- aState.subStates) {
        for (transition <- state.transitions) {
          graph.addTransition(transition, get_state_id_from(transition.preState), get_state_id_to(transition.postState))
        }
        build_composite_state_transitions(state)
      }
    }

    try {
//      println("makeStateMachineDiagramDot start") 2009-03-01
      graph.addStart("start")
      for (state <- aStateMachine.states) {
        if (state.isComposite) {
          val id = make_subgraph_id
          stateIds += (state -> id)
          val compositeState = graph.addCompositeState(state, id)
          build_composite_state(state, compositeState)
        } else {
          val id = make_id
          stateIds += (state -> id)
          graph.addState(state, id)
        }
      }
      if (counter > 1) {
        // graph.addPseudoTransition("start", "state1")
        graph.graph.elements(1) match {
          case m: GVNode => graph.addPseudoTransition("start", m.id)
          case m: GVSubgraph => graph.addPseudoTransition("start", m.id, s"${m.id}_start")
          case m: GVGraph => RAISE.noReachDefect
        }
      }
      for (transition <- aStateMachine.transitions) {
        graph.addTransition(transition, get_state_id_from(transition.preState), get_state_id_to(transition.postState))
      }
      for (state <- aStateMachine.states if state.isComposite) {
        build_composite_state_transitions(state)
      }
      for (state <- aStateMachine.states if state.isTerminal) {
        graph.addPseudoTransition(get_state_id_from(state), "end")
      }
      graph.addEnd("end")
      val r = graphviz.toDotText
      println(r)
      r
    } finally {
      // graphviz.close
//      println("makeStateMachineDiagramDot end")
    }
  }
}

abstract class GraphBase {
  protected def add_Edge(p: GVEdge): Unit

  protected final def is_cluster_start(id: String) = {
    id.startsWith("cluster_") && id.endsWith("_start")
  }

  protected final def is_cluster_end(id: String) = {
    id.startsWith("cluster_") && id.endsWith("_end")
  }

  protected final def get_cluster_name(id: String) = {
    require (id.startsWith("cluster_"))
    if (id.endsWith("_start")) {
      id.substring(0, id.length - "_start".length)
    } else if (id.endsWith("_end")) {
      id.substring(0, id.length - "_end".length)
    } else {
      error ("illigal id = " + id)
    }
  }

  final def addPseudoTransition(aSourceId: String, aTargetId: String) {
    val edge = new GVEdge(aSourceId, "", aTargetId, "")
    edge.arrowhead = "normal"
    edge.arrowtail = "none"
    edge.color = "#2b2b2b" // 蝋色 ろういろ
    _composite_arrow(edge, aSourceId, aTargetId)
    add_Edge(edge)
  }

  final def addPseudoTransition(aSourceId: String, aTargetId: String, ankderid: String) {
    val edge = new GVEdge(aSourceId, "", aTargetId, "", Some(ankderid))
    edge.arrowhead = "normal"
    edge.arrowtail = "none"
    edge.color = "#2b2b2b" // 蝋色 ろういろ
    _composite_arrow(edge, aSourceId, ankderid)
    add_Edge(edge)
  }

  private def _composite_arrow(edge: GVEdge, aSourceId: String, aTargetId: String) {
    if (is_cluster_end(aSourceId)) {
      edge.ltail = get_cluster_name(aSourceId)
    }
    if (is_cluster_start(aTargetId)) {
      edge.lhead = get_cluster_name(aTargetId)
    }
  }
}

class GraphStateMachine(
  val graph: GVDigraph,
  val context: Context,
  implicit val sm: SimpleModel
) extends GraphBase {
  val windows_normal_font = "msmincho.ttf"
  val windows_bold_font = "msgothic.ttf"

//  graph.defaultGraphAttributes.layout = "circo"
  graph.defaultGraphAttributes.shape = "box"
  if (context.isPlatformWindows) {
    graph.defaultGraphAttributes.fontname = windows_normal_font
  }
  graph.defaultGraphAttributes.fontsize = "10"
  graph.defaultGraphAttributes.compound = "true"
  graph.defaultNodeAttributes.shape = "box"
  if (context.isPlatformWindows) {
    graph.defaultNodeAttributes.fontname = windows_normal_font
  }
  graph.defaultNodeAttributes.fontsize = "10"
  if (context.isPlatformWindows) {
    graph.defaultEdgeAttributes.fontname = windows_normal_font
  }
  graph.defaultEdgeAttributes.fontsize = "10"
  graph.defaultEdgeAttributes.fontcolor = "#192f60" // 紺青 こんじょう

  protected def add_Edge(p: GVEdge) = graph.edges += p

  final def addStart(aId: String) {
    val node = new GVNode(aId)
    node.label = " "
    node.shape = "circle"
    node.style = "filled"
    node.fixedsize = "true"
    node.width = "0.3"
    node.fillcolor = "#0d0015" // 漆黒 しっこく
    graph.elements += node
    node
  }

  final def addEnd(aId: String) {
    val node = new GVNode(aId)
    node.label = " "
    node.shape = "doublecircle"
    node.style = "filled"
    node.fixedsize = "true"
    node.width = "0.25"
    node.fillcolor = "#0d0015" // 漆黒 しっこく
    graph.elements += node
    node
  }

  final def addState(aState: MState, aId: String): GVNode = {
    require (!aState.isComposite)
    val node = new GVNode(aId)
    node.label = aState.name
    node.style = "rounded,filled"
    node.fillcolor = "#fcc800" // 向日葵色 ひまわりいろ
    graph.elements += node
    node
  }

  final def addCompositeState(aState: MState, aId: String): GraphCompositeState = {
    require (aState.isComposite)
    val node = new GVSubgraph(aId)
    node.label = aState.name
    node.style = "rounded,filled"
    node.fillcolor = "#fef263" // 黄檗色 きはだいろ
    graph.elements += node
    new GraphCompositeState(node, context, sm)
  }

  final def addTransition(aTransition: MTransition, aSourceId: String, aTargetId: String) {
    def make_label: Option[String] =
      aTransition.event.map(evt =>
        aTransition.guard.map {
          case MGuard.empty => evt.name
          case guard: MGuard => evt.name + "\\n" + "[" + guard.mark + "]"
        }.getOrElse(evt.name)
      )

    val edge = new GVEdge(aSourceId, "", aTargetId, "")
    edge.arrowhead = "normal"
    edge.arrowtail = "none"
    make_label.foreach(x => edge.label = x)
    edge.labelfontcolor = "#640125" // 葡萄色 えびいろ
    edge.color = "#e2041b" // 猩々緋 しょうじょうひ
    if (is_cluster_end(aSourceId)) {
      edge.ltail = get_cluster_name(aSourceId)
    }
    if (is_cluster_start(aTargetId)) {
      edge.lhead = get_cluster_name(aTargetId)
    }
    graph.edges += edge
  }
}

class GraphCompositeState(
  val graph: GVSubgraph,
  val context: Context,
  implicit val sm: SimpleModel
) extends GraphBase {
  final def startId = graph.id + "_start"
  final def endId = graph.id + "_end"
  final def historyId = graph.id + "_history"

  protected def add_Edge(p: GVEdge) = graph.edges += p

  final def addStart = {
    val node = new GVNode(startId)
    node.label = " "
    node.shape = "circle"
    node.style = "filled"
    node.fixedsize = "true"
    node.width = "0.3"
    node.fillcolor = "#0d0015" // 漆黒 しっこく
    graph.elements += node
    node
  }

  final def addEnd = {
    val node = new GVNode(endId)
    node.label = " "
    node.shape = "doublecircle"
    node.style = "filled"
    node.fixedsize = "true"
    node.width = "0.25"
    node.fillcolor = "#0d0015" // 漆黒 しっこく
    graph.elements += node
    node
  }

  final def addHistory = {
    val node = new GVNode(historyId)
    node.label = " "
    node.shape = "circle"
    node.style = "filled"
    node.fixedsize = "true"
    node.width = "0.3"
    node.label = "H"
    node.fillcolor = "#89c3eb" // 勿忘草色 わすれなぐさいろ
    graph.elements += node
    node
  }

  final def addState(aState: MState, aId: String): GVNode = {
    require (!aState.isComposite)
    val node = new GVNode(aId)
    node.label = aState.name
    node.style = "rounded,filled"
    node.fillcolor = "#fcc800" // 向日葵色 ひまわりいろ
    graph.elements += node
    node
  }

  final def addCompositeState(aState: MState, aId: String): GraphCompositeState = {
    require (aState.isComposite)
    val node = new GVSubgraph(aId)
    node.label = aState.name
    node.style = "rounded,filled"
    node.fillcolor = "#fef263" // 黄檗色 きはだいろ
    graph.elements += node
    new GraphCompositeState(node, context, sm)
  }

  final def addTransition(aTransition: MTransition, aSourceId: String, aTargetId: String) {
    def make_label: Option[String] =
      aTransition.event.map(evt =>
        aTransition.guard.map {
          case MGuard.empty => evt.name
          case guard: MGuard => evt.name + "\\n" + "[" + guard.mark + "]"
        }.getOrElse(evt.name)
      )

    val edge = new GVEdge(aSourceId, "", aTargetId, "")
    edge.arrowhead = "normal"
    edge.arrowtail = "none"
    make_label.foreach(x => edge.label = x)
    edge.labelfontcolor = "#640125" // 葡萄色 えびいろ
    edge.color = "#e2041b" // 猩々緋 しょうじょうひ
    if (is_cluster_end(aSourceId)) {
      edge.ltail = get_cluster_name(aSourceId)
    }
    if (is_cluster_start(aTargetId)) {
      edge.lhead = get_cluster_name(aTargetId)
    }
    graph.edges += edge
  }
}
