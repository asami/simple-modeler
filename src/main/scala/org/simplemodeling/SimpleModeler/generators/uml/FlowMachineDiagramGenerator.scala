package org.simplemodeling.SimpleModeler.generators.uml

// import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
// import org.goldenport.entity.content._
// import org.goldenport.entity.GEntityContext
// import org.goldenport.entities.graphviz._
// import org.simplemodeling.SimpleModeler.entity._
// import org.simplemodeling.SimpleModeler.entity.flow._

import scala.collection.mutable.{HashMap, HashSet}
import org.goldenport.graphviz._
import org.goldenport.bag.ChunkBag
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.Context
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Mar. 21, 2011
 * @version May. 10, 2020
 * @author  ASAMI, Tomoharu
 */
class FlowMachineDiagramGenerator(
  context: Context,
  implicit val sm: SimpleModel
) extends DiagramGeneratorBase(context, sm) {
  final def makeFlowMachineDiagramPng(flow: MFlowMachine): ChunkBag = {
    make_diagram_png(_make_flow_diagram_dot(flow))
  }

  private def _make_flow_diagram_dot(flow: MFlowMachine): String = {
    val graphviz = new Graphviz()
//    graphviz.open()
    val graph = new FlowDiagramGraph(graphviz.graph, context, sm)
    val registeredentity = new HashSet[String]

    def add_port(port: MPort) {
        if (!registeredentity.contains(port.entityType.qualifiedName)) {
          registeredentity += port.entityType.qualifiedName
          graph.addClassSimple(port.entityType.typeObject)
        }
    }

    def add_step(step: MFlowStep) {
      graph.addStep(step)
      for (et <- step.entities) {
        if (!registeredentity.contains(et.qualifiedName)) {
          registeredentity += et.qualifiedName
          record_trace("add_step = " + et.typeObject)
          et.typeObject match {
            case ds: MDataSource => graph.addDataSource(ds)
            case entity => graph.addClassSimple(entity)
          }
        }
      }
      for (in <- step.inputs) {
        graph.addFlowInput(in.typeObject, step)
      }
      for (out <- step.outputs) {
        graph.addFlowOutput(step, out.typeObject)
      }
    }

    try {
//      flow.ports.foreach(add_port)
      flow.steps.foreach(add_step)
      graphviz.toDotText
    } finally {
//      graphviz.close()
    }
  }
}

class FlowDiagramGraph(
  g: GVDigraph,
  c: Context,
  sm: SimpleModel
) extends DigraphBase(g, c, sm) {
  def addPort(port: MPort) {
    val id = make_id(port)
    val node = new GVNode(id)
    node.label = port.name
    node.style = "rounded,filled"
    node.fillcolor = "#fcc800" // 向日葵色 ひまわりいろ
    graph.elements += node
    node
  }

  def addStep(step: MFlowStep) {
    val id = make_id(step)
    val node = new GVNode(id)
    node.label = step.name
    node.style = "rounded,filled"
//    node.fillcolor = "#f6ad49" // 柑子色こうじいろ
//    node.fillcolor = "#59b9c6" // 新橋色しんばしいろ
    node.fillcolor = "#ee836f" // 珊瑚朱色さんごしゅいろ
    graph.elements += node
    node
  }

  def addDataSource(ds: MDataSource) = {
    val id = make_subgraph_id(ds)
    val node = new GVSubgraph(id)
    graph.elements += node
    val sg = new FlowDataSourceGraph(node, context, ds, sm)
    set_input_id(ds, sg.inputId)
    set_output_id(ds, sg.outputId)
  }

  def addFlowInput(src: MObject, dest: MFlowStep) {
    val ltailid = src match {
      case _: MDataSource => Some(ids(src))
      case _ => None
    }
    graph.edges += new GVEdge(get_output_id(src), "p", ids(dest), "p") {
      arrowhead = "normal"
      for (id <- ltailid) {
        ltail = id
      }
    }
  }

  def addFlowOutput(src: MFlowStep, dest: MObject) {
    val lheadid = dest match {
      case _: MDataSource => Some(ids(dest))
      case _ => None
    }
//    record_trace("lheadid = " + lheadid)
    graph.edges += new GVEdge(ids(src), "p", get_input_id(dest), "p") {
      arrowhead = "normal"
      for (id <- lheadid) {
        lhead = id
      }
    }
  }
}

class FlowDataSourceGraph(
  g: GVSubgraph,
  c: Context,
  val datasource: MDataSource,
  sm: SimpleModel
) extends SubgraphBase(g, c, sm) {
  private implicit def _sm = sm

  graph.label = datasource.name
  graph.style = "filled"
  graph.fillcolor = "#d4dcda" // 薄雲鼠うすくもねず
//  record_trace("FlowDataSourceGraph = " + datasource.entities.length)
  for (et <- datasource.entities) {
//    record_trace("FlowDataSourceGraph = " + et)
    addClassSimple(et.typeObject)
  }
}
