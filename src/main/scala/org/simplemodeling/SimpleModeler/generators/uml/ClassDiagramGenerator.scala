package org.simplemodeling.SimpleModeler.generators.uml

// import java.io.{InputStream, OutputStream, IOException}
// import scala.collection.mutable.{ArrayBuffer, HashMap}
// import org.goldenport.entity.content._
// import org.goldenport.entity.GEntityContext
// import org.goldenport.entities.graphviz._
// import org.simplemodeling.SimpleModeler.entity._
// import org.simplemodeling.SimpleModeler.entity.flow._
// import org.simplemodeling.SimpleModeler.entity.business._
// import org.simplemodeling.dsl.SExecutionStep
// import org.simplemodeling.dsl.SStep
// import org.goldenport.Strings

import scala.collection.mutable.HashMap
import org.goldenport.RAISE
import org.goldenport.graphviz._
import org.goldenport.bag.ChunkBag
import org.simplemodeling.model._
import org.simplemodeling.model.business._
import org.simplemodeling.model.domain._
import org.simplemodeling.SimpleModeler.Context
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   Jan. 15, 2009
 *  version Nov. 20, 2011
 *  version Sep. 18, 2012
 *  version Oct. 23, 2012
 *  version Nov. 30, 2012
 *  version Dec. 17, 2012
 *  version May. 24, 2020
 * @version Sep. 17, 2023
 * @author  ASAMI, Tomoharu
 */
class ClassDiagramGenerator(
  context: Context,
  implicit val sm: SimpleModel
) extends DiagramGeneratorBase(context, sm) {
  final def makeClassDiagramPng(aPackage: MPackage, aTheme: Perspective): ChunkBag = {
    make_class_diagram_png(makeClassDiagramDot(aPackage, aTheme))
  }

  final def makeClassDiagramPng(anObject: MObject, aTheme: Perspective): ChunkBag = {
    make_class_diagram_png(makeClassDiagramDot(anObject, aTheme))
  }

  private def make_class_diagram_png(text: String): ChunkBag = {
    make_diagram_png(text, Some("ClassDiagram.png"))
  }

  final def makeClassDiagramSvg(aPackage: MPackage, aTheme: Perspective): ChunkBag = {
    make_class_diagram_svg(makeClassDiagramDot(aPackage, aTheme))
  }

  final def makeClassDiagramSvg(anObject: MObject, aTheme: Perspective): ChunkBag = {
    make_class_diagram_svg(makeClassDiagramDot(anObject, aTheme))
  }

  private def make_class_diagram_svg(text: String): ChunkBag = {
    make_diagram_svg(text, Some("ClassDiagram.svg"))
  }

/*
  private def make_class_diagram_png(text: StringContent): BinaryContent = {
    val layout = "dot"
    var in: InputStream = null
    var out: OutputStream = null
    try {
      val dot: Process = context.executeCommand("dot -Tpng -K%s -q".format(layout))
      in = dot.getInputStream()
      out = dot.getOutputStream()
//      println("dot(class diagram) = " + text.string)
      text.write(out)
      out.flush
      out.close
      //      val b = com.asamioffice.goldenport.io.UIO.stream2Bytes(in)
      //      println("b size = " + b.length)
      BinaryContent(in, context, "classdiagram.png", Strings.mimetype.image_png)
    } catch {
      case e: IOException => {
        // Cannot run program "dot": error=2, No such file or directory
        throw new IOException("graphvizのdotコマンドが動作しませんでした。graphvizについてはhttp://www.graphviz.org/を参照してください。(詳細エラー:%s)".format(e.getMessage))
      }
    } finally {
      if (in != null) {
        in.close
      }
      //      println("finish process = " + dot)
    }
  }
*/

  final def makeClassDiagramDot(aPackage: MPackage, aTheme: Perspective): String = {
    val graphviz = new Graphviz()
    // graphviz.open
    try {
      val graph = new Graph(graphviz.graph, context, sm)
      val classes: Seq[MObject] = aPackage.elements.collect {
        case entity: MEntity       => entity
        case tr: MTrait => tr
//        case rule: SMRule if !rule.associations.isEmpty => rule
        case rule: MRule => rule
        case powertype: MPowertype => powertype
        case sm: MStateMachine     => sm
        case service: MService     => service
        case flow: MFlow           => flow
        case uc: MUsecase          => uc
        case t: MTask              => t
//        case x => sys.error("unknown object = " + x)
      }
      record_debug("ClassDiagramGenerator#makeClassDiagramDot: " + classes.map(x => x.name + ": " + x))
      //        filter(child => child.isInstanceOf[SMEntity] || child.isInstanceOf[SMRule] || child.isInstanceOf[SMPowertype] || child.isInstanceOf[SMService]).map(_.asInstanceOf[SMObject])

      var counter = 1
      val ids = new HashMap[MObject, String]

      def get_id(anObject: MObject): String = {
        try {
          ids.get(anObject).get
        } catch {
          case e: NoSuchElementException => {
            context.trace("No id = " + anObject.packageName + "/" + anObject.name)
            throw e
          }
          case e => throw e
        }
      }

      def add_object(anObject: MObject) {
//        println("add_object: " + anObject.name + "/" + anObject.getClass + "/" + anObject.traits)
        val id = "class" + counter
        counter += 1
        ids.put(anObject, id)
        anObject match {
          case assoc: MAssociationEntity => {
            if (assoc.attributes.isEmpty) {
              graph.addClassSimple(anObject, id)
            } else {
              aTheme match {
                case OverviewPerspective => graph.addClassSimple(anObject, id)
                case HilightPerspective     => graph.addClassSimple(anObject, id)
                case DetailPerspective      => graph.addClassFull(anObject, id)
                case _             => sys.error("illegal theme: " + aTheme)
              }
            }
          }
          case powertype: MPowertype => {
            aTheme match {
              case OverviewPerspective => graph.addPowertypeSimple(powertype, id)
              case HilightPerspective     => graph.addPowertypeSimple(powertype, id)
              case DetailPerspective      => graph.addPowertypeFull(powertype, id)
              case _             => sys.error("illegal theme: " + aTheme)
            }
          }
          case sm: MStateMachine => {
            aTheme match {
              case OverviewPerspective => graph.addStateMachineSimple(sm, id)
              case HilightPerspective     => graph.addStateMachineSimple(sm, id)
              case DetailPerspective      => graph.addStateMachineFull(sm, id)
              case _             => sys.error("illegal theme: " + aTheme)
            }
          }
          case business: MBusinessEntity => {
            graph.addClassSimple(anObject, id)
          }
          case _ => {
            aTheme match {
              case OverviewPerspective => graph.addClassSimple(anObject, id)
              case HilightPerspective     => graph.addClassSimple(anObject, id)
              case DetailPerspective      => graph.addClassFull(anObject, id)
              case _             => sys.error("illegal theme: " + aTheme)
            }
          }
        }
      }

      def add_relationship(anObject: MObject) {
        def add_generalization_relationships(obj: MObject) {
          obj.getBaseObject match {
            case Some(parent) => {
              val childId = ids.get(obj).get
              val parentId = ids.get(parent).get
              graph.addGeneralization(childId, parentId)
            }
            case None => //
          }
        }

        def add_trait_relationships(obj: MObject) {
          for (rel <- obj.traits) {
            val target = rel.mixinTrait
//            println("add_trait_relationships: " + ids + " / " + target)
            val sourceId = get_id(obj)
            val targetId = get_id(target)
            aTheme match {
              case OverviewPerspective => graph.addSimpleTraitRelationship(sourceId, targetId, rel)
              case HilightPerspective     => graph.addPlainTraitRelationship(sourceId, targetId, rel)
              case DetailPerspective      => graph.addTraitRelationship(sourceId, targetId, rel)
              case _             => graph.addSimpleTraitRelationship(sourceId, targetId, rel)
            }
          }
        }

        def add_powertype_relationships(aSource: MObject) {
          for (rel <- aSource.powertypes) {
            val target = rel.powertype
            val sourceId = ids.get(aSource).get
            val targetId = ids.get(target).get
            aTheme match {
              case OverviewPerspective => graph.addSimplePowertypeRelationship(sourceId, targetId, rel)
              case HilightPerspective     => graph.addPlainPowertypeRelationship(sourceId, targetId, rel)
              case DetailPerspective      => graph.addPowertypeRelationship(sourceId, targetId, rel)
              case _             => graph.addSimplePowertypeRelationship(sourceId, targetId, rel)
            }
          }
        }

        def add_statemachine_relationships(aSource: MObject) {
          for (rel <- aSource.stateMachines) {
            val target = rel.statemachine
            val sourceId = ids.get(aSource).get
            val targetId = ids.get(target).get
            aTheme match {
              case OverviewPerspective => graph.addSimpleStateMachineRelationship(sourceId, targetId, rel)
              case HilightPerspective     => graph.addPlainStateMachineRelationship(sourceId, targetId, rel)
              case DetailPerspective      => graph.addStateMachineRelationship(sourceId, targetId, rel)
              case _             => graph.addSimpleStateMachineRelationship(sourceId, targetId, rel)
            }
          }
        }

        def add_role_relationships(aSource: MObject) {
          for (rel <- aSource.roles) {
            val target = rel.role
            val sourceId = ids.get(aSource).get
            val targetId = ids.get(target).get
            aTheme match {
              case OverviewPerspective => graph.addSimpleRoleRelationship(sourceId, targetId, rel)
              case HilightPerspective     => graph.addPlainRoleRelationship(sourceId, targetId, rel)
              case DetailPerspective      => graph.addRoleRelationship(sourceId, targetId, rel)
              case _             => graph.addSimpleRoleRelationship(sourceId, targetId, rel)
            }
          }
        }

        def add_association_relationships(aSource: MObject) {
          for (assoc <- aSource.associations) {
            val target = assoc.associationType.typeObject
            val sourceId = get_id(aSource)
            val targetId = get_id(target)
            aTheme match {
              case OverviewPerspective => graph.addSimpleAssociation(sourceId, targetId, assoc)
              case HilightPerspective     => graph.addPlainAssociation(sourceId, targetId, assoc)
              case DetailPerspective      => graph.addAssociation(sourceId, targetId, assoc)
              case _             => graph.addSimpleAssociation(sourceId, targetId, assoc)
            }
          }
        }

        def add_usecase_association_relationships(aSource: MObject) {
          def rolerelationship(sid: String, tid: String, name: String) {
            aTheme match {
              case OverviewPerspective => graph.addUsecaseRoleRelationship(sid, tid, name)
              case HilightPerspective     => graph.addUsecaseRoleRelationship(sid, tid, name)
              case DetailPerspective      => graph.addUsecaseRoleRelationship(sid, tid, name)
              case _             => graph.addUsecaseRoleRelationship(sid, tid, name)
            }
          }
          def associationrelationship(sid: String, tid: String, assoc: MAssociation) {
            aTheme match {
              case OverviewPerspective => graph.addSimpleAssociation(sid, tid, assoc)
              case HilightPerspective     => graph.addPlainAssociation(sid, tid, assoc)
              case DetailPerspective      => graph.addAssociation(sid, tid, assoc)
              case _             => graph.addSimpleAssociation(sid, tid, assoc)
            }
          }

          for (assoc <- aSource.associations) {
            val target = assoc.associationType.typeObject
//            println("ClassDiagramGenerator#add_usecase_association_relationships: " + target)
            val sourceId = get_id(aSource)
            val targetId = get_id(target)
            target match {
              case _: MEntity => rolerelationship(sourceId, targetId, assoc.name)
              case _: MUsecase => associationrelationship(sourceId, targetId, assoc)
            }
          }
        }

        def add_usecase_relationships(aSource: MObject) {
          aSource match {
            case usecase: MStoryObject => {
//              for (step <- usecase.basicFlow;
//                   entity <- step.entities) {
              for (entity <- usecase.usedEntities) {
                val sourceid = get_id(aSource)
                val targetid = get_id(entity)
                aTheme match {
                  case OverviewPerspective => graph.addUsecaseRoleRelationship(sourceid, targetid, entity.term)
                  case HilightPerspective     => graph.addUsecaseRoleRelationship(sourceid, targetid, entity.term)
                  case DetailPerspective      => graph.addUsecaseRoleRelationship(sourceid, targetid, entity.term)
                  case _             => graph.addUsecaseRoleRelationship(sourceid, targetid, entity.term)
                }
              }
              // collect steps primary / secondary actors.
              // be supporting actors in case of not usecases primary actors or secondary actors, 
/*
	      basicFlow
	      extensionFlow
	      alternateFlow
	      exceptionFlow
*/
              for (step <- usecase.basicFlow) {
//                println("ClassDiagramGenerator step (%s) = %s".format(usecase.name, step))
//                println("children = " + step.children)
              }
              for (story <- usecase.includedStories) {
                val sourceid = get_id(aSource)
                val targetid = get_id(story)
                aTheme match {
                  case OverviewPerspective => graph.addUsecaseIncludeRelationship(sourceid, targetid)
                  case HilightPerspective     => graph.addUsecaseIncludeRelationship(sourceid, targetid)
                  case DetailPerspective      => graph.addUsecaseIncludeRelationship(sourceid, targetid)
                  case _             => graph.addUsecaseIncludeRelationship(sourceid, targetid)
                }
              }
            }
            case _ => //
          }
        }

        def add_association_class_relationships(aSource: MAssociationEntity) {
          val sourceId = get_id(aSource)
          val targets = aSource.associations.map(x => {
            (get_id(x.associationType.typeObject), x)
          })
          aTheme match {
            case OverviewPerspective => graph.addSimpleAssociationClassRelationship(sourceId, targets)
            case HilightPerspective     => graph.addPlainAssociationClassRelationship(sourceId, targets)
            case DetailPerspective      => graph.addAssociationClassRelationship(sourceId, targets)
            case _             => graph.addSimpleAssociationClassRelationship(sourceId, targets)
          }
        }

        def add_rule_relationships(aSource: MObject) {
          for (rel <- aSource.rules) {
            val target = rel.rule
            val sourceId = ids.get(aSource).get
            val targetId = ids.get(target).get
            aTheme match {
              case OverviewPerspective => graph.addDependencyRelationship(sourceId, targetId)
              case HilightPerspective     => graph.addDependencyRelationship(sourceId, targetId)
              case DetailPerspective      => graph.addDependencyRelationship(sourceId, targetId)
              case _             => graph.addDependencyRelationship(sourceId, targetId)
            }
          }
        }

        def add_service_relationships(aSource: MObject) {
          for (rel <- aSource.services) {
            val target = rel.service
            val sourceId = ids.get(aSource).get
            val targetId = ids.get(target).get
            aTheme match {
              case OverviewPerspective => graph.addDependencyRelationship(sourceId, targetId)
              case HilightPerspective     => graph.addDependencyRelationship(sourceId, targetId)
              case DetailPerspective      => graph.addDependencyRelationship(sourceId, targetId)
              case _             => graph.addDependencyRelationship(sourceId, targetId)
            }
          }
        }

        add_generalization_relationships(anObject)
        add_trait_relationships(anObject)
        add_powertype_relationships(anObject)
        add_statemachine_relationships(anObject)
        add_role_relationships(anObject)
        anObject match {
          case u: MUsecase => add_usecase_association_relationships(u)
          case t: MTask => add_usecase_association_relationships(t)
          case a: MAssociationEntity => add_association_class_relationships(a)
          case p: MPartRef => add_association_relationships(anObject)
          case _ => add_association_relationships(anObject)
        }
        add_usecase_relationships(anObject)
        add_rule_relationships(anObject)
        add_service_relationships(anObject)
      }

      classes.foreach(add_object)
      classes.foreach(add_relationship)
      graphviz.toDotText
    } finally {
      // graphviz.close
    }
  }

  final def makeClassDiagramDot(anObject: MObject, aTheme: Perspective): String = {
    val graphviz = new Graphviz()
    // graphviz.open
    try {
      val graph = new Graph(graphviz.graph, context, sm)
      aTheme match {
        case HilightPerspective => graph.showDerivedAttribute = true
        case _         => //
      }

      def add_parent_objects(anObject: MObject, aChildId: String, aDepth: Int) {
        def add_parent_with_association(aParent: MObject, aParentId: String) {
          graph.addClassFull(aParent, aParentId)
          add_association_objects(aParent, aParentId)
        }

        anObject.getBaseObject match {
          case Some(parent) => {
            val parentId = "parent" + aDepth
            aTheme match {
              case HilightPerspective     => graph.addClassSimple(parent, parentId)
              case OverviewPerspective => add_parent_with_association(parent, parentId)
              case DetailPerspective      => add_parent_with_association(parent, parentId)
              case _             => add_parent_with_association(parent, parentId)
            }
            add_parent_objects(parent, parentId, aDepth + 1)
            graph.addGeneralization(aChildId, parentId)
          }
          case None => //
        }
      }

      def add_child_objects(anObject: MObject, aParentId: String) {
        var counter = 1
        for (child <- anObject.derivedObjects) {
          val childId = "child" + counter
          counter += 1
          graph.addClassSimple(child, childId)
          add_child_objects(child, childId)
          graph.addGeneralization(childId, aParentId)
        }
      }

      def add_powertype_objects(anObject: MObject, aObjectId: String) {
        var counter = 1

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          add_derivation_powertype_objects(aSource, aSourceId)
          for (rel <- aSource.powertypes) {
            val targetId = "powertype" + counter
            counter += 1
            val target = rel.powertype
            graph.addPowertypeFull(target, targetId)
            if (aDerived)
              graph.addDerivedPowertypeRelationship(aSourceId, targetId, rel)
            else
              graph.addPowertypeRelationship(aSourceId, targetId, rel)
          }
        }

        def add_derivation_powertype_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      def add_role_objects(anObject: MObject, aObjectId: String) {
        var counter = 1

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          add_derivation_role_objects(aSource, aSourceId)
          for (rel <- aSource.roles) {
            val targetId = "role" + counter
            counter += 1
            val target = rel.role
            graph.addClassSimple(target, targetId)
            if (aDerived)
              graph.addDerivedRoleRelationship(aSourceId, targetId, rel)
            else
              graph.addRoleRelationship(aSourceId, targetId, rel)
          }
        }

        def add_derivation_role_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      def add_association_objects(anObject: MObject, aObjectId: String) {
        var counter = 1

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          def add_relate_stateMachines(anTarget: MObject, aTargetId: String) {
            def process_object(aSmOwner: MObject, aSmOwnerId: String, aDerived: Boolean) {
              add_derivation_statemachine_objects(aSmOwner, aSmOwnerId)
              for (stateMachine <- aSmOwner.stateMachines) {
                val smc = stateMachine.statemachine
                if (smc.isReceiveEvent(aSource)) {
                  val smId = aSmOwnerId + "association" + "statemachine" + counter
                  counter += 1
                  aTheme match {
                    case HilightPerspective => graph.addStateMachineSimple(smc, smId)
                    case OverviewPerspective => {
                      graph.addStateMachineFull(smc, smId)
                    }
                    case DetailPerspective => {
                      graph.addStateMachineFull(smc, smId)
                    }
                    case NonePerspective => RAISE.notImplementedYetDefect
                  }
                  if (aDerived)
                    graph.addDerivedStateMachineRelationship(aSmOwnerId, smId)
                  else
                    graph.addStateMachineRelationship(aSmOwnerId, smId)
                }
              }
            }

            def add_derivation_statemachine_objects(aSmOwner: MObject, aSmOwnerId: String) {
              aSmOwner.getBaseObject match {
                case Some(parent) => process_object(parent, aSmOwnerId, true)
                case None         => //
              }
            }

            process_object(anTarget, aTargetId, false)
          }

          aTheme match {
            case HilightPerspective => add_derivation_association_objects(aSource, aSourceId)
            case _         => //
          }
          for (assoc <- aSource.associations) {
            val targetId = aSourceId + "association" + counter
            counter += 1
            val target = assoc.associationType.typeObject
            graph.addAssociationClass(target, targetId, assoc)
            add_relate_stateMachines(target, targetId)
            if (aSource.isInstanceOf[MUsecase]) {
              graph.addUsecaseRoleRelationship(aSourceId, targetId, assoc.name)
            } else {
              if (aDerived)
                graph.addDerivedAssociation(aSourceId, targetId, assoc)
              else
                graph.addAssociation(aSourceId, targetId, assoc)
            }
            if (assoc.isComposition) {
              process_object(target, targetId, aDerived)
            }
          }
        }

        def add_derivation_association_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      /* 2009-01-20
      def add_association_objects(anObject: SMObject, aSourceId: String) {
	var counter = 1

	def add_derivation_association_objects(aSource: SMObject) {
	  aSource.getBaseObject match {
	    case Some(parent) => {
	      add_derivation_association_objects(parent)
	      for (assoc <- parent.associations) {
		val targetId = "association" + counter
		counter += 1
		val target = assoc.associationType.typeObject
		graph.addAssociationClass(target, targetId, assoc)
		graph.addDerivedAssociation(aSourceId, targetId, assoc)
		if (assoc.isComposition) {
		  for (assoc2 <- target.associations) {
		    val targetId2 = "association" + counter
		    counter += 1
		    val target2 = assoc2.associationType.typeObject
		    graph.addAssociationClass(target2, targetId2, assoc2)
		    graph.addDerivedAssociation(targetId, targetId2, assoc2)
		  }
		}
	      }
	    }
	    case None => //
	  }
	}

	add_derivation_association_objects(anObject)
	for (assoc <- anObject.associations) {
	  val targetId = "association" + counter
	  counter += 1
	  val target = assoc.associationType.typeObject
	  graph.addAssociationClass(target, targetId, assoc)
	  graph.addAssociation(aSourceId, targetId, assoc)
	  if (assoc.isComposition) {
	    for (assoc2 <- target.associations) {
	      val targetId2 = "association" + counter
	      counter += 1
	      val target2 = assoc2.associationType.typeObject
	      graph.addAssociationClass(target2, targetId2, assoc2)
	      graph.addAssociation(targetId, targetId2, assoc2)
	    }
	  }
	}
      }
*/
      def add_stateMachines(anObject: MObject, aObjectId: String) {
        var counter = 1

        def add_income_events(aStateMachine: MStateMachine, aSourceId: String) {
          for (event <- aStateMachine.getEvents) {
            val targetId = "statemachine" + counter
            counter += 1
            graph.addClassSimple(event, targetId)
            graph.addSimpleStateMachineRelationship(aSourceId, targetId)
          }
        }

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          add_derivation_statemachine_objects(aSource, aSourceId)
          for (stateMachine <- aSource.stateMachines) {
            val targetId = "statemachine" + counter
            counter += 1
            val target = stateMachine.statemachine
            aTheme match {
              case HilightPerspective => graph.addStateMachineSimple(target, targetId)
              case OverviewPerspective => {
                graph.addStateMachineFull(target, targetId)
                add_income_events(target, targetId)
              }
              case DetailPerspective => {
                graph.addStateMachineFull(target, targetId)
                add_income_events(target, targetId)
              }
              case NonePerspective => RAISE.notImplementedYetDefect
            }
            if (aDerived)
              graph.addDerivedStateMachineRelationship(aSourceId, targetId)
            else
              graph.addStateMachineRelationship(aSourceId, targetId)
          }
        }

        def add_derivation_statemachine_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      def add_voucher_objects(anObject: MObject, aObjectId: String) {
        var counter = 1

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          add_derivation_voucher_objects(aSource, aSourceId)
          for (rel <- aSource.vouchers) {
            val targetId = "voucher" + counter
            counter += 1
            val target = rel.voucher
            aTheme match {
              case HilightPerspective => graph.addClassSimple(target, targetId)
              case OverviewPerspective => {
                graph.addClassFull(target, targetId)
              }
              case DetailPerspective => {
                graph.addClassFull(target, targetId)
              }
              case NonePerspective => RAISE.notImplementedYetDefect
            }
            if (aDerived)
              graph.addDerivedVoucherRelationship(aSourceId, targetId, rel)
            else
              graph.addVoucherRelationship(aSourceId, targetId, rel)
          }
        }

        def add_derivation_voucher_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      def add_port_objects(anObject: MObject, aObjectId: String) {
        var counter = 1

        def process_object(aSource: MObject, aSourceId: String, aDerived: Boolean) {
          add_derivation_voucher_objects(aSource, aSourceId)
          for (port <- aSource.ports) {
            val targetId = "port" + counter
            counter += 1
            val target = port.entityType.typeObject;
            aTheme match {
              case HilightPerspective => graph.addClassSimple(target, targetId)
              case OverviewPerspective => {
                graph.addClassFull(target, targetId)
              }
              case DetailPerspective => {
                graph.addClassFull(target, targetId)
              }
              case NonePerspective => RAISE.notImplementedYetDefect
            }
            if (aDerived)
              graph.addDerivedPortRelationship(aSourceId, targetId, port)
            else
              graph.addPortRelationship(aSourceId, targetId, port)
          }
        }

        def add_derivation_voucher_objects(aSource: MObject, aSourceId: String) {
          aSource.getBaseObject match {
            case Some(parent) => process_object(parent, aSourceId, true)
            case None         => //
          }
        }

        process_object(anObject, aObjectId, false)
      }

      graph.addClassRoot(anObject, "primary")
      add_parent_objects(anObject, "primary", 1)
      add_child_objects(anObject, "primary")
      add_powertype_objects(anObject, "primary")
      add_role_objects(anObject, "primary")
      add_association_objects(anObject, "primary")
      add_stateMachines(anObject, "primary")
      add_voucher_objects(anObject, "primary")
      add_port_objects(anObject, "primary")
      graphviz.toDotText
    } finally {
      // graphviz.close
    }
  }
}

class Graph(g: GVDigraph, c: Context, sm: SimpleModel) extends DigraphBase(g, c, sm) {
}
