package org.simplemodeling.SimpleModeler.transformers.specdoc

import org.goldenport.RAISE
import org.smartdox._
import org.smartdox.builder._
import org.smartdox.specdoc._
import org.simplemodeling.model._
import org.simplemodeling.model.business._
import org.simplemodeling.model.requirement._
import org.simplemodeling.model.domain._
import org.simplemodeling.SimpleModeler.generators.uml._

/*
 * @since   Jul. 27, 2020
 *  version Aug. 17, 2020
 *  version Oct.  4, 2020
 *  version Nov. 21, 2020
 * @version Dec. 27, 2020
 * @author  ASAMI, Tomoharu
 */
trait ObjectPart { self: SpecDocTransformer =>
  import SpecDocTransformer._

  protected final def add_object(anObject: MObject): SDEntity = {
    println(s"ObjectPart#add_object $anObject")
    val category: SDCategory = _category(anObject)
    val description = anObject.description
    val elements = Vector(
      make_summary(anObject),
      make_overview(anObject),
      make_specification(anObject),
      make_description(anObject),
      make_roles(anObject),
      make_services(anObject),
      make_rules(anObject),
      make_powertypes(anObject),
      make_derived_attributes(anObject),
      make_own_attributes(anObject),
      make_derived_associations(anObject),
      make_own_associations(anObject),
      make_derived_operations(anObject),
      make_own_operations(anObject),
      make_derived_stateMachines(anObject),
      make_own_stateMachines(anObject),
      make_derived_transitions(anObject),
      make_own_transitions(anObject),
      make_flow(anObject),
      make_derived_uses(anObject),
      make_own_uses(anObject),
      make_derived_participations(anObject),
      make_own_participations(anObject),
      make_history(anObject)
    ).flatten
    SDEntity(anObject.designation, category, description, elements)
  }

  private def _category(p: MObject) = p match {
    case _: MComponent => Category_Component
    case _: MEntity => Category_Entity
    case _: MValue => Category_Value
    case _: MVoucher => Category_Voucher
    case _: MService => Category_Service
    case _: MRule => Category_Rule
    case _: MPowertype => Category_Powertype
    case _: MDatatype => Category_Datatype
    case _: MBusinessUsecase => Category_BusinessUsecase
    case _: MRequirementUsecase => Category_RequirementUsecase
    case _: MRequirementTask => Category_RequirementTask
//    case _: MStateTransition => Category_StateTransition
    case _: MFlow => Category_Flow
    case _ => RAISE.noReachDefect("Unknown category = " + p)
  }

  protected final def make_summary(p: MObject): Option[SDEntity] = None // TODO

  protected final def make_overview(p: MObject): Option[SDEntity] =
    if (drawDiagram || classClassDiagramThemes.isEmpty)
      None
    else
      Some(_make_overview(p))

  private def _make_overview(p: MObject) = {
    def build_hilight: Dox = {
      val binary = generator.makeClassDiagramPng(p, HilightPerspective)
      val src = "ClassDiagram" + p.name + "Hilight.png"
      val title = "概要"
      FigureIngredient(binary, src, title)
    }

    def build_perspective: Dox = {
      val binary = generator.makeClassDiagramPng(p, OverviewPerspective)
      val src = "ClassDiagram" + p.name + "Perspective.png"
      val title = "全体像"
      FigureIngredient(binary, src, title)
    }

    val figures: Dox = classClassDiagramThemes.map {
      case Theme.Hilight => build_hilight
      case Theme.Perspective => build_perspective
      case Theme.Detail => Dox.empty
    }
    val summary: Dox = p.description.effectiveSummary // ??? before 概要
    val spec: Dox = {
      val features = Table.empty // TODO
      val properties = Table.empty // TODO
      Fragment(features, properties)
    }
    val content: Dox = p.description.content
    val elements = List(summary, figures, spec, content)
    SDEntity("概要", Category_Overview, Dox.toDox(elements))
  }

  protected final def make_specification(p: MObject): Option[SDEntity] = {
    val fst = _make_features(p)
    val attrst = _make_attributes(p)
    val assocst = _make_associations(p)
    if (List(fst, attrst, assocst).exists(_.nonEmpty))
      Some(_make_spec(fst, attrst, assocst))
    else
      None
  }

  private def _make_features(p: MObject): Table = {
    val fs = Table.Builder.captionHeaderString("特性", List("項目", "値", "備考"))
    fs.append("名前", p.name)
    fs.append("ラベル", p.label)
    fs.apply()
  }

  private def _make_attributes(p: MObject): Table = {
    val attrs = Table.Builder.captionHeaderString("属性", List("名前", "型", "多重度", "ラベル", "カラム", "データ型", "備考"))
    for (x <- p.attributes) {
      attrs.append(x.name)
    }
    attrs.apply()
  }

  private def _make_associations(p: MObject): Table = {
    val assocs = Table.Builder.captionHeaderString("関連", List("名前", "型", "多重度", "ラベル", "カラム", "データ型", "備考"))
    for (x <- p.associations) {
      assocs.append(x.name)
    }
    assocs.apply()
  }

  private def _make_spec(features: Table, attrs: Table, assocs: Table) = {
    val desc: Dox = Fragment(features, attrs, assocs)
    val cat: SDCategory = Category_Overview // TODO
    SDEntity("仕様", cat, desc)
  }

  protected final def make_description(p: MObject): Option[SDEntity] = {
    None // TODO
  }

  protected final def make_roles(p: MObject): Option[SDEntity] =
    _make_relationships(Category_RoleRelationship, p.roles)

  protected final def make_services(p: MObject): Option[SDEntity] =
    _make_relationships(Category_ServiceRelationship, p.services)

  protected final def make_rules(p: MObject): Option[SDEntity] =
    _make_relationships(Category_RuleRelationship, p.rules)

  protected final def make_powertypes(p: MObject): Option[SDEntity] =
    _make_relationships(Category_PowertypeRelationship, p.powertypes)

  protected final def make_derived_attributes(p: MObject): Option[SDEntity] =
    p.derivedObjects(model) match {
      case Nil => None
      case xs => _make_derived_attributes(Category_Attribute, xs)
    }
  // protected final def make_derived_attributes(p: MObject): Option[SDEntity] =
  //   p.derivedObjects(model) match {
  //     case Nil => None
  //     case xs => _make_derived_attributes_with(Category_Attribute, xs)((owner: MObject, x: MAttribute) =>
  //       List(SDFeature(MFeature.OwnerClass, object_literal(owner)))
  //     )
  //   }

  protected final def make_own_attributes(p: MObject): Option[SDEntity] =
    _make_attributes(Category_Attribute, p.attributes)

  protected final def make_derived_associations(p: MObject): Option[SDEntity] =
    p.derivedObjects(model) match {
      case Nil => None
      case xs => _make_derived_relationships(Category_Association, xs, (_: MObject).associations)
    }
  // protected final def make_derived_associations(p: MObject): Option[SDEntity] =
  //   p.derivedObjects(model) match {
  //     case Nil => None
  //     case xs => _make_derived_relationships_with(Category_Association, xs, (_: MObject).associations)((owner: MObject, x: MAssociation) =>
  //       List(SDFeature(MFeature.OwnerClass, object_literal(owner)))
  //     )
  //   }

  protected final def make_own_associations(p: MObject): Option[SDEntity] =
    _make_relationships(Category_Association, p.associations)

  protected final def make_derived_operations(p: MObject): Option[SDEntity] =
    p.derivedObjects(model) match {
      case Nil => None
      case xs => _make_derived_operations(Category_Operation, xs)
    }
  // protected final def make_derived_operations(p: MObject): Option[SDEntity] =
  //   p.derivedObjects(model) match {
  //     case Nil => None
  //     case xs => _make_derived_operations_with(Category_Operation, xs)((owner: MObject, x: MOperation) =>
  //       List(SDFeature(MFeature.OwnerClass, object_literal(owner)))
  //     )
  //   }

  protected final def make_own_operations(p: MObject): Option[SDEntity] = {
    _make_operations(Category_Attribute, p.operations)
  }

  protected final def make_derived_stateMachines(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_StateMachine, p.stateMachines)
  }

  protected final def make_own_stateMachines(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_StateMachine, p.stateMachines)
  }

  protected final def make_derived_transitions(p: MObject): Option[SDEntity] = {
    None // TODO
  }

  protected final def make_own_transitions(p: MObject): Option[SDEntity] = {
    None // TODO
  }

  protected final def make_flow(p: MObject): Option[SDEntity] = {
    None // TODO
  }

  protected final def make_derived_uses(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_Use, model.ownUses(p))
  }

  protected final def make_own_uses(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_Use, model.derivedUses(p))
  }

  protected final def make_derived_participations(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_Participation, model.ownParticipations(p))
  }

  protected final def make_own_participations(p: MObject): Option[SDEntity] = {
    _make_relationships(Category_Participation, model.derivedParticipations(p))
  }

  protected final def make_history(p: MObject): Option[SDEntity] = {
    None // TODO
  }

  private def _make_relationships[T <: MRelationship](
    category: SDCategory,
    xs: Seq[T]
  ): Option[SDEntity] = _make_relationships_with(category, xs)(_ => Nil)

  private def _make_relationships_with[T <: MRelationship](
    category: SDCategory,
    xs: Seq[T]
  )(f: T => Seq[SDFeature]): Option[SDEntity] =
    if (xs.isEmpty) {
      None
    } else {
      val elements = xs.map(_make_relationship(category, _)(f))
      Some(SDEntity(category.label, category, elements))
    }

  private def _make_relationship[T <: MRelationship](
    category: SDCategory,
    p: T
  )(f: T => Seq[SDFeature]): SDEntity = {
    val desc = p.description
    // val note = p.note
    val features = p.features.map(_.toSDFeature) ++ f(p)
    SDEntity(p.designation, category, features, desc)
  }

  private def _make_derived_relationships[T <: MRelationship](
    category: SDCategory,
    xs: List[MObject],
    elementsf: MObject => Seq[T]
  ): Option[SDEntity] =
    _make_derived_relationships_with(category, xs, elementsf)((_, _) => Nil)

  private def _make_derived_relationships_with[T <: MRelationship](
    category: SDCategory,
    xs: List[MObject],
    elementsf: MObject => Seq[T]
  )(f: (MObject, T) => Seq[SDFeature]): Option[SDEntity] = 
    _make_derived_elements_with(category, xs, elementsf)(f)

  private def _make_attributes(
    category: SDCategory,
    xs: Seq[MAttribute]
  ): Option[SDEntity] = _make_attributes_with(category, xs)(_ => Nil)

  private def _make_attributes_with(
    category: SDCategory,
    xs: Seq[MAttribute]
  )(f: MAttribute => Seq[SDFeature]): Option[SDEntity] = 
    if (xs.isEmpty) {
      None
    } else {
      val elements = xs.map(_make_attribute_with(category, _)(f))
      Some(SDEntity(category.label, category, elements))
    }

  // private def _make_derived_associations_with(
  //   category: SDCategory,
  //   xs: List[MObject]
  // )(f: (MObject, MAssociation) => Seq[SDFeature]): Option[SDEntity] = {
  //   val elements = xs.flatMap(_make_derived_associations_with(category, _)(f))
  //   if (elements.isEmpty) {
  //     None
  //   } else {
  //     Some(SDEntity("???", category, elements))
  //   }
  // }

  // private def _make_derived_associations_with(
  //   category: SDCategory,
  //   p: MObject
  // )(f: (MObject, MAssociation) => Seq[SDFeature]): List[SDEntity] = {
  //   p.associations.map(_make_association_with(category, _)(f(p, _)))
  // }

  private def _make_attribute_with(
    category: SDCategory,
    p: MAttribute
  )(f: MAttribute => Seq[SDFeature]): SDEntity = {
    val desc = p.description
    // val note = p.note
    val features = p.features.map(_.toSDFeature) ++ f(p)
    SDEntity(p.designation, category, features, desc)
  }

  private def _make_derived_attributes(
    category: SDCategory,
    xs: List[MObject]
  ): Option[SDEntity] =
    _make_derived_attributes_with(category, xs)((_, _) => Nil)

  private def _make_derived_attributes_with(
    category: SDCategory,
    xs: List[MObject]
  )(f: (MObject, MAttribute) => Seq[SDFeature]): Option[SDEntity] = 
    _make_derived_elements_with(category, xs, (_: MObject).attributes)(f)

  private def _make_operations(
    category: SDCategory,
    xs: Seq[MOperation]
  ): Option[SDEntity] = _make_operations_with(category, xs)(_ => Nil)

  private def _make_operations_with(
    category: SDCategory,
    xs: Seq[MOperation]
  )(f: MOperation => Seq[SDFeature]): Option[SDEntity] =
    if (xs.isEmpty) {
      None
    } else {
      val elements = xs.map(_make_operation(category, _)(f))
      Some(SDEntity(category.label, category, elements))
    }

  private def _make_operation(
    category: SDCategory,
    p: MOperation
  )(f: MOperation => Seq[SDFeature]): SDEntity = {
    val desc = p.description
    // val note = p.note
    val features = p.features.map(_.toSDFeature) ++ f(p)
    SDEntity(p.designation, category, features, desc)
  }

  private def _make_derived_operations(
    category: SDCategory,
    xs: List[MObject]
  ): Option[SDEntity] =
    _make_derived_operations_with(category, xs)((_, _) => Nil)

  private def _make_derived_operations_with(
    category: SDCategory,
    xs: List[MObject]
  )(f: (MObject, MOperation) => Seq[SDFeature]): Option[SDEntity] = 
    _make_derived_elements_with(category, xs, (_: MObject).operations)(f)

  private def _make_elements[T <: MElement](
    category: SDCategory,
    xs: Seq[T]
  ): Option[SDEntity] = _make_elements_with(category, xs)(_ => Nil)

  private def _make_elements_with[T <: MElement](
    category: SDCategory,
    xs: Seq[T]
  )(f: T => Seq[SDFeature]): Option[SDEntity] =
    if (xs.isEmpty) {
      None
    } else {
      val elements = xs.map(_make_element_with(category, _)(f))
      Some(SDEntity(category.label, category, elements))
    }

  private def _make_element_with[T <: MElement](
    category: SDCategory,
    p: T
  )(f: T => Seq[SDFeature]): SDEntity = {
    val desc = p.description
    // val note = p.note
    val features = p.features.map(_.toSDFeature) ++ f(p)
    SDEntity(p.designation, category, features, desc)
  }

  private def _make_derived_elements_with[T <: MElement](
    category: SDCategory,
    xs: List[MObject],
    elementsf: MObject => Seq[T]
  )(f: (MObject, T) => Seq[SDFeature]): Option[SDEntity] = {
    val elements = xs.flatMap(_make_derived_elements_with(category, _, elementsf)(f))
    if (elements.isEmpty) {
      None
    } else {
      Some(SDEntity(category.label, category, elements)) // XXX derived
    }
  }

  private def _make_derived_elements_with[T <: MElement](
    category: SDCategory,
    p: MObject,
    elementsf: MObject => Seq[T]
  )(f: (MObject, T) => Seq[SDFeature]): List[SDEntity] = {
    elementsf(p).map(_make_element_with(category, _)(_derived_features(p, _, f))).toList
  }

  private def _derived_features[T <: MElement](o: MObject, p: T, f: (MObject, T) => Seq[SDFeature]): Seq[SDFeature] =
    List(SDFeature(MFeature.OwnerClass, object_literal(o))) ++ f(o, p)

  protected final def object_literal(p: MObject): Dox = ???
}
/*
    // Common
    def add_object(anObject: MObject): SDEntity = {
      val objEntity = new SDEntity(anObject.name)

      def add_overview {
        if (!drawDiagram) return
        if (classClassDiagramThemes.isEmpty) return
        val generator = new ClassDiagramGenerator(simpleModel)
        val views = SBViews()

        def build_hilight {
          val binary = generator.makeClassDiagramPng(anObject, "hilight")
          val figure = new SBBinaryContentFigure(binary)
          figure.src = "ClassDiagram" + anObject.name + "Hilight.png"
          val view = SBView()
          view.title = "概要"
          view.addChild(figure)
          views.addChild(view)
        }

        def build_perspective {
          val binary = generator.makeClassDiagramPng(anObject, "perspective")
          val figure = new SBBinaryContentFigure(binary)
          figure.src = "ClassDiagram" + anObject.name + "Perspective.png"
          val view = SBView()
          view.title = "詳細"
          view.addChild(figure)
          views.addChild(view)
        }

        for (theme <- classClassDiagramThemes) {
          theme match {
            case "hilight"     => build_hilight
            case "perspective" => build_perspective
            case _             => error("parameter error: " + theme)
          }
        }
        if (views.length == 1) {
          objEntity.overview = views.getChild(0).getChild(0)
        } else {
          objEntity.overview = views
        }
      }

      def add_relationships(aCategory: SDCategory)(aCollect: MObject => Seq[MRelationship])(aSetup: (SDEntity, MRelationship) => Unit) {
        def add_relationship(aRel: MRelationship): SDEntity = {
          val relEntity = new SDEntity(aRel.name)
          relEntity.category = aCategory
          relEntity.resume.copyIn(aRel.resume)
          relEntity.description = aRel.description
          relEntity.note = aRel.note
          for (feature <- aRel.features) {
            relEntity.addFeature(feature.key, feature.value) // XXX description_is
          }
          aSetup(relEntity, aRel)
          objEntity.addSubEntity(relEntity)
          relEntity
        }

        def add_derived_relationship(aRel: MRelationship, anOwner: MObject) {
          val relEntity = add_relationship(aRel)
          relEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
        }

        def add_derived_relationships {
          var mayParent = anObject.getBaseObject
          while (mayParent.isDefined) {
            val parent = mayParent.get
            aCollect(parent).foreach(add_derived_relationship(_, parent))
            mayParent = parent.getBaseObject
          }
        }

        def add_own_relationships {
          aCollect(anObject).foreach(add_relationship)
        }

        add_own_relationships
        add_derived_relationships
      }

      // role
      def add_roles {
        add_relationships(Category_RoleRelationship) {
          (anObj: MObject) => anObj.roles
        } {
          (anEntity: SDEntity, aRel: MRelationship) =>
          val role = aRel.asInstanceOf[MRoleRelationship]
          anEntity.addFeature(FeatureType, role.targetTypeLiteral)
        }
      }

      // service
      def add_services {
        add_relationships(Category_ServiceRelationship) {
          (anObj: MObject) => anObj.services
        } {
          (anEntity: SDEntity, aRel: MRelationship) =>
          val service = aRel.asInstanceOf[MServiceRelationship]
          anEntity.addFeature(FeatureType, service.targetTypeLiteral)
        }
      }

      // rule
      def add_rules {
        add_relationships(Category_RuleRelationship) {
          (anObj: MObject) => anObj.rules
        } {
          (anEntity: SDEntity, aRel: MRelationship) =>
          val rule = aRel.asInstanceOf[MRuleRelationship]
          anEntity.addFeature(FeatureType, rule.targetTypeLiteral)
        }
      }

      // powertype
      def add_powertypes {
        add_relationships(Category_PowertypeRelationship) {
          (anObj: MObject) => anObj.powertypes
        } {
          (anEntity: SDEntity, aRel: MRelationship) =>
          val powertype = aRel.asInstanceOf[MPowertypeRelationship]
          anEntity.addFeature(FeatureType, powertype.targetTypeLiteral)
        }
      }

      // attribute
      def add_attribute(anAttr: MAttribute): SDEntity = {
        val attrEntity = new SDEntity(anAttr.name)
        attrEntity.category = Category_Attribute
        attrEntity.resume.copyIn(anAttr.resume)
        attrEntity.description = anAttr.description
        attrEntity.note = anAttr.note
        for (feature <- anAttr.features) {
          attrEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(attrEntity)
        attrEntity
      }

      def add_derived_attribute(anAttr: MAttribute, anOwner: MObject) {
        val attrEntity = add_attribute(anAttr)
        attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_attributes {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.attributes.foreach(add_derived_attribute(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_attributes {
        anObject.attributes.foreach(add_attribute)
      }

      // association
      def add_association(anAssoc: MAssociation): SDEntity = {
        val assocEntity = new SDEntity(anAssoc.name)
        assocEntity.category = Category_Association
        assocEntity.resume.copyIn(anAssoc.resume)
        assocEntity.description = anAssoc.description
        assocEntity.note = anAssoc.note
        for (feature <- anAssoc.features) {
          assocEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(assocEntity)
        assocEntity
      }

      def add_derived_association(anAssoc: MAssociation, anOwner: MObject) {
        val attrEntity = add_association(anAssoc)
        attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_associations {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.associations.foreach(add_derived_association(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_associations {
        anObject.associations.foreach(add_association)
      }

      // operation
      def add_operation(anOperation: MOperation): SDEntity = {
        val operationEntity = new SDEntity(anOperation.name)
        operationEntity.category = Category_Operation
        operationEntity.resume.copyIn(anOperation.resume)
        operationEntity.description = anOperation.description
        operationEntity.note = anOperation.note
        for (feature <- anOperation.features) {
          operationEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(operationEntity)
        operationEntity
      }

      def add_derived_operation(anOper: MOperation, anOwner: MObject) {
        val attrEntity = add_operation(anOper)
        attrEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_operations {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.operations.foreach(add_derived_operation(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_operations {
        anObject.operations.foreach(add_operation)
      }

      // stateMachine
      def add_stateMachine(aStateMachine: MStateMachineRelationship): SDEntity = {
        val sm = aStateMachine.statemachine
        val stateMachineEntity = new SDEntity(aStateMachine.name)

        def add_states {
          val spec = new SBDivision()
          if (drawDiagram) {
            val generator = new StateMachineDiagramGenerator(simpleModel)
            val binary = generator.makeStateMachineDiagramPng(aStateMachine.statemachine)
            val figure = new SBBinaryContentFigure(binary)
            //            figure.src = "StateMachineDiagram" + aStateMachine.ownerObject.name + aStateMachine.name + ".png"
            figure.src = "StateMachineDiagram" + sm.name + ".png"
            spec.addChild(figure)
          }
          //
          val table = SBTable()
          table.title = "状態遷移表"
          sm.buildStateTransitionTable(table.table)
          spec.addChild(table)
          //
          stateMachineEntity.specification = spec
        }

        stateMachineEntity.category = Category_StateMachine
        stateMachineEntity.resume.copyIn(aStateMachine.resume)
        stateMachineEntity.description = aStateMachine.description
        stateMachineEntity.note = aStateMachine.note
        for (feature <- aStateMachine.features) {
          stateMachineEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(stateMachineEntity)
        add_states
        stateMachineEntity
      }

      def add_derived_stateMachine(aStateMachine: MStateMachineRelationship, anOwner: MObject) {
        val smEntity = add_stateMachine(aStateMachine)
        smEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_stateMachines {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.stateMachines.foreach(add_derived_stateMachine(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_stateMachines {
        anObject.stateMachines.foreach(add_stateMachine)
      }

      // transition
      def add_transition(aTransition: MTransition): SDEntity = {
        val transitionEntity = new SDEntity(aTransition.name)
        transitionEntity.category = Category_StateTransition
        transitionEntity.resume.copyIn(aTransition.resume)
        transitionEntity.description = aTransition.description
        transitionEntity.note = aTransition.note
        transitionEntity.addFeature(FeatureType, aTransition.resourceLiteral)
        transitionEntity.addFeature(FeaturePreState, aTransition.preStateLiteral)
        transitionEntity.addFeature(FeatureGuard, aTransition.guardLiteral)
        transitionEntity.addFeature(FeaturePostState, aTransition.postStateLiteral)
        for (feature <- aTransition.features) {
          transitionEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        objEntity.addSubEntity(transitionEntity)
        transitionEntity
      }

      def add_derived_transition(aTransition: MTransition, anOwner: MObject) {
        val transEntity = add_transition(aTransition)
        transEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_transitions {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          mayParent.get match {
            case parent: MDomainEvent => {
              parent.resourceTransitions.foreach(add_derived_transition(_, parent))
              mayParent = parent.getBaseObject
            }
            case _ => mayParent = None
          }
        }
      }

      def add_own_transitions {
        anObject match {
          case event: MDomainEvent => event.resourceTransitions.foreach(add_transition)
          case _                    => //
        }
      }

      // flowMachine
      def add_flowMachine(aFlowMachine: MFlowMachine): SDEntity = {
        val flowMachineEntity = new SDEntity(aFlowMachine.name)

        def add_flows {
          val spec = new SBDivision()
          if (drawDiagram) {
            val generator = new FlowMachineDiagramGenerator(simpleModel)
            val binary = generator.makeFlowMachineDiagramPng(aFlowMachine)
            val figure = new SBBinaryContentFigure(binary)
            figure.src = "FlowMachineDiagram" + aFlowMachine.ownerObject.name + aFlowMachine.name + ".png"
            spec.addChild(figure)
          }
          //
          flowMachineEntity.specification = spec
        }

        flowMachineEntity.category = Category_FlowMachine
        flowMachineEntity.resume.copyIn(aFlowMachine.resume)
        flowMachineEntity.description = aFlowMachine.description
        flowMachineEntity.note = aFlowMachine.note
        for (feature <- aFlowMachine.features) {
          flowMachineEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(flowMachineEntity)
        add_flows
        flowMachineEntity
      }

      def add_flow {
        anObject match {
          case flow: MFlow => {
            add_flowMachine(flow.flowMachine)
          }
          case _ => ;
        }
      }

      // use
      def add_use(anUse: MUse): SDEntity = {
        val useEntity = new SDEntity(anUse.name)
        useEntity.category = Category_Use
        useEntity.resume.copyIn(anUse.resume)
        useEntity.description = anUse.description
        useEntity.note = anUse.note
        useEntity.addFeature(FeatureName, anUse.name)
        useEntity.addFeature(FeatureClass, anUse.elementLiteral)
        useEntity.addFeature(FeatureUseKind, anUse.useKindLiteral)
        useEntity.addFeature(FeatureUserClass, anUse.userLiteral)
        useEntity.addFeature(FeatureReceiverClass, anUse.receiverLiteral)
        for (feature <- anUse.features) {
          useEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(useEntity)
        useEntity
      }

      def add_derived_use(aUse: MUse, anOwner: MObject) {
        val useEntity = add_use(aUse)
        useEntity.addFeature(FeatureOwnerClass, object_literal(anOwner)) // XXX
      }

      def add_derived_uses {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.uses.foreach(add_derived_use(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_uses {
        anObject.uses.foreach(add_use)
      }

      // participation
      def add_participation(aParticipation: MParticipation): SDEntity = {
        val participationEntity = new SDEntity(aParticipation.name)
        participationEntity.category = Category_Participation
        participationEntity.resume.copyIn(aParticipation.resume)
        participationEntity.description = aParticipation.description
        participationEntity.note = aParticipation.note
        participationEntity.addFeature(FeatureName, aParticipation.name)
        participationEntity.addFeature(FeatureElement, aParticipation.elementLiteral)
        participationEntity.addFeature(FeatureType, aParticipation.elementTypeLiteral)
        participationEntity.addFeature(FeatureKind, aParticipation.elementKindLiteral)
        participationEntity.addFeature(FeatureRoleName, aParticipation.roleName)
        participationEntity.addFeature(FeatureRoleType, aParticipation.roleTypeLiteral)
        for (feature <- aParticipation.features) {
          participationEntity.addFeature(feature.key, feature.value) // XXX description_is
        }
        // XXX
        objEntity.addSubEntity(participationEntity)
        participationEntity
      }

      def add_derived_participation(aParticipation: MParticipation, anOwner: MObject) {
        val participationEntity = add_participation(aParticipation)
        participationEntity.addFeature(FeatureOwnerClass, object_literal(anOwner))
      }

      def add_derived_participations {
        var mayParent = anObject.getBaseObject
        while (mayParent.isDefined) {
          val parent = mayParent.get
          parent.participations.foreach(add_derived_participation(_, parent))
          mayParent = parent.getBaseObject
        }
      }

      def add_own_participations {
        anObject.participations.foreach(add_participation)
      }

      def object_literal(anObj: MObject) = {
        new SIAnchor(Text(anObj.name)) unresolvedRef_is new SElementRef(anObj.packageName, anObj.name)
      }

      def object_category = {
        if (anObject.isInstanceOf[MComponent]) Category_Component
        else if (anObject.isInstanceOf[MEntity]) Category_Entity
        else if (anObject.isInstanceOf[MValue]) Category_Value
        else if (anObject.isInstanceOf[MDocument]) Category_Document
        else if (anObject.isInstanceOf[MService]) Category_Service
        else if (anObject.isInstanceOf[MRule]) Category_Rule
        else if (anObject.isInstanceOf[MPowertype]) Category_Powertype
        else if (anObject.isInstanceOf[MDatatype]) Category_Datatype
        else if (anObject.isInstanceOf[MBusinessUsecase]) Category_BusinessUsecase
        else if (anObject.isInstanceOf[MBusinessTask]) Category_BusinessTask
        else if (anObject.isInstanceOf[MRequirementUsecase]) Category_RequirementUsecase
        else if (anObject.isInstanceOf[MRequirementTask]) Category_RequirementTask
        else if (anObject.isInstanceOf[MTransition]) Category_StateTransition
        else if (anObject.isInstanceOf[MFlow]) Category_Flow
        else error("Unknown category = " + anObject)
      }

      specPackage.addEntity(objEntity)
      objEntity.category = object_category
      objEntity.categories ++= entityCategories // XXX move individual settings
      objEntity.sdocTitle = Text(anObject.name)
      objEntity.caption = anObject.caption
      objEntity.brief = anObject.brief
      objEntity.summary = anObject.summary
      objEntity.description = anObject.description
      objEntity.note = anObject.note
      for (feature <- anObject.features) {
        objEntity.addFeature(feature.key, feature.value) // XXX description_is
      }
      add_overview
      add_roles
      add_services
      add_rules
      add_powertypes
      add_derived_attributes
      add_own_attributes
      add_derived_associations
      add_own_associations
      add_derived_operations
      add_own_operations
      add_derived_stateMachines
      add_own_stateMachines
      add_derived_transitions
      add_own_transitions
      add_flow
      add_derived_uses
      add_own_uses
      add_derived_participations
      add_own_participations
      objEntity.history.copyIn(anObject.history)
      objEntity
    }
 */
