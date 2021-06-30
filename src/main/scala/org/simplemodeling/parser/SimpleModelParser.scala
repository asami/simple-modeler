package org.simplemodeling.parser

import scalaz._, Scalaz._
import java.util.Locale
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.I18NString
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser._
import org.goldenport.values.Designation
import org.goldenport.record.v3.{Table => _, _}
import org.simplemodeling.model._
import org.simplemodeling.model.domain._

/*
 * @since   Aug.  8, 2019
 *  version Aug. 10, 2019
 *  version Nov.  4, 2019
 *  version Dec. 15, 2019
 *  version Feb. 16, 2020
 *  version Mar.  1, 2020
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 *  version Nov. 19, 2020
 *  version Dec. 27, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class SimpleModelParser(config: SimpleModelParser.Config) {
  import SimpleModelParser._

  def apply(p: String): SimpleModel = {
    val bconfig = LogicalBlocks.Config.default
    val blocks = LogicalBlocks.parse(bconfig, p)
    val builder = Builder(config, MPackageRef.default)
    builder.apply(blocks)
  }
}

object SimpleModelParser {
  case class Config(
    locale: Locale,
    blockKindMap: Map[BlockKind, NonEmptyVector[String]],
    featureTableNames: NonEmptyVector[String],
    propertyTableNames: NonEmptyVector[String],
    statemachineTableNames: NonEmptyVector[String],
    packageNames: NonEmptyVector[String],
    baseClassNames: NonEmptyVector[String],
    traitNames: NonEmptyVector[String],
    attributeKindNames: NonEmptyVector[String],
    attributeNames: NonEmptyVector[String],
    powertypeNames: NonEmptyVector[String],
    associationNames: NonEmptyVector[String],
    nameNames: NonEmptyVector[String],
    typeNames: NonEmptyVector[String],
    datatypeNames: NonEmptyVector[String],
    multiplicityNames: NonEmptyVector[String],
    labelNames: NonEmptyVector[String],
    constraintNames: NonEmptyVector[String]
  ) {
    lazy val _block_kind_vector = blockKindMap.toVector

    def blockKind(p: I18NString): BlockKind = {
      _block_kind_vector.find {
        case (k, vs) => p.containsKey(vs.vector)
      }.map(_._1).getOrElse(UnknownBlock)
    }

    def isFeatureTable(p: Table): Boolean = p.getCaptionName.map(x => featureTableNames.vector.exists(_.equalsIgnoreCase(x))).getOrElse(false)

    def isPropertyTable(p: Table): Boolean = p.getCaptionName.map(x => propertyTableNames.vector.exists(_.equalsIgnoreCase(x))).getOrElse(false)

    def isStatemachineTable(p: Table): Boolean = p.getCaptionName.map(x => statemachineTableNames.vector.exists(_.equalsIgnoreCase(x))).getOrElse(false)
  }
  object Config {
    val default = Config(
      Locale.JAPAN,
      _block_kind_map,
      _feature_table_names,
      _property_table_names,
      _statemachine_table_names,
      _package_names,
      _base_class_names,
      _trait_names,
      _attribute_kind_names,
      _attribute_names,
      _powertype_names,
      _association_names,
      _name_names,
      _type_names,
      _datatype_names,
      _multiplicity_names,
      _label_names,
      _constraint_names
    )
  }

  private val _block_kind_map: Map[BlockKind, NonEmptyVector[String]] = Map(
    ResourceBlock -> NonEmptyVector("resource")
  )

  private val _feature_table_names = NonEmptyVector("性質一覧")

  private val _property_table_names = NonEmptyVector("特性一覧")

  private val _statemachine_table_names = NonEmptyVector("状態機械")

  private val _package_names = NonEmptyVector("パッケージ")

  private val _base_class_names = NonEmptyVector("既定クラス")

  private val _trait_names = NonEmptyVector("トレイト")

  private val _attribute_kind_names = NonEmptyVector("特性")

  private val _attribute_names = NonEmptyVector("属性")

  private val _powertype_names = NonEmptyVector("区分")

  private val _association_names = NonEmptyVector("関連")

  private val _name_names = NonEmptyVector("名前")

  private val _type_names = NonEmptyVector.create("型")

  private val _datatype_names = NonEmptyVector.create("データ型", "型")

  private val _multiplicity_names = NonEmptyVector("多重度")

  private val _label_names = NonEmptyVector("ラベル")

  private val _constraint_names = NonEmptyVector("制約")

  private val _spec_table_names: Set[String] = (_feature_table_names.vector ++ _property_table_names.vector).map(_.toLowerCase).toSet

  sealed trait BlockKind {
  }
  case object ResourceBlock extends BlockKind {
  }
  case object EventBlock extends BlockKind {
  }
  case object ActorBlock extends BlockKind {
  }
  case object TraitBlock extends BlockKind {
  }
  case object UnknownBlock extends BlockKind {
  }

  case class CreateObjectCommand(
    description: Description,
    affiliation: MPackageRef,
    stereotype: MDomainStereotype,
    base: Option[MObjectRef],
    traits: List[MTraitRef],
    powertypes: List[MPowertypeRef],
    attributes: List[MAttribute],
    associations: List[MAssociation],
    operations: List[MOperation],
    stateMachines: List[MStateMachineRef],
    properties: Record = Record.empty
  )

  // TODO migrate SmartDox
  def toRecords(p: Table): List[Record] = p.head.map { head =>
    val columns = head.columns
    p.body.records.map { x =>
      val fields = columns.zip(x.fields).flatMap {
        case (k, v) =>
          if (Strings.blankp(k))
            None
          else
            Some(Field(k, _to_field_value(v.contents)))
      }
      Record(fields)
    }
  }.getOrElse {
    p.body.records.map { x =>
      val fields = x.fields.zipWithIndex.map {
        case (v, k) => Field(s"${k + 1}", _to_field_value(v.contents))
      }
      Record(fields)
    }
  }

  // TODO migrate SmartDox
  def toPropertyRecord(p: Table): Record = {
    val (nameindex, valueindex) = {
      p.head.map { head =>
        (0, 1) // TODO
      }.getOrElse((0, 1))
    }
    val xs = p.body.records.flatMap { x =>
      (x.fields.lift(nameindex), x.fields.lift(valueindex)) match {
        case (Some(n), Some(v)) =>
          val key: Symbol = _to_symbol(n.contents)
          val value = _to_field_value(v.contents)
          Some(Field(key, value))
        case _ => None
      }
    }
    Record(xs)
  }

  private def _to_symbol(ps: List[Dox]): Symbol = Symbol(ps.map(_.toText).mkString)

  private def _to_field_value(ps: List[Dox]): FieldValue = ps match {
    case Nil => EmptyValue
    case x :: Nil => x match {
      case m: Text => SingleValue(m.toText)
      case m => SingleValue(m)
    }
    case xs => SingleValue(Fragment(xs))
  }

  case class Builder(
    config: SimpleModelParser.Config,
    pkg: MPackageRef
  ) {
    def apply(p: LogicalBlocks): SimpleModel = {
      val xs = p.blocks.flatMap(_parse_by_kind)
      case class Z(elements: Vector[MElement] = Vector.empty) {
        def r = SimpleModel(elements)

        def +(rhs: ValidationNel[ParseMessage, MElement]) = rhs match {
          case Success(m) => copy(elements = elements :+ m)
          case Failure(e) => RAISE.syntaxErrorFault("???")
        }
      }
      xs./:(Z())(_+_).r
    }

    private def _parse_by_kind(p: LogicalBlock): Vector[ValidationNel[ParseMessage, MElement]] = {
      // TODO prologue contents
      _block_kind(p) match {
        case ResourceBlock => _parse_resources(p)
        case EventBlock => _parse_events(p)
        case ActorBlock => _parse_actors(p)
        case TraitBlock => _parse_traits(p)
        //      case RoleBlock => _parse_roles(p)
        case UnknownBlock => Vector.empty
      }
    }

    private def _block_kind(p: LogicalBlock) = p match {
      case m: LogicalSection => config.blockKind(m.title.toI18NString)
      case _ => UnknownBlock
    }

    private def _parse_resources(p: LogicalBlock) = _parse_entities(p, MDomainResourceStereotype)

    private def _parse_events(p: LogicalBlock) = _parse_entities(p, MDomainEventStereotype)

    private def _parse_actors(p: LogicalBlock) = _parse_entities(p, MDomainActorStereotype)

    private def _parse_traits(p: LogicalBlock) = _parse_elements(p, _parse_trait)

    private def _parse_vouchers(p: LogicalBlock) = _parse_elements(p, _parse_voucher)

    private def _parse_entities(p: LogicalBlock, st: MDomainStereotype) = p match {
      case m: LogicalSection => m.blocks.blocks.map(_parse_entity(_, st))
      case _ => Vector.empty
    }

    private def _parse_elements(p: LogicalBlock, f: LogicalBlock => ValidationNel[ParseMessage, MElement]) = p match {
      case m: LogicalSection => m.blocks.blocks.map(f)
      case _ => Vector.empty
    }

    private def _create_command(p: LogicalBlock, st: MDomainStereotype) = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, p)
      val contents = _distill_contents(dox)
      val specs = _distill_specs(contents)
      val ft = _get_feature_table(dox)
      val features = ft.map(toPropertyRecord)
      val pt = _get_property_table(dox)
      println(s"SimpleModelParser#Builder#_create_command DOX: ${dox}")
      println(s"SimpleModelParser#Builder#_create_command Property Table: ${pt}")
      val properites = pt.map(toRecords).getOrElse(Nil)
      val name = _name(dox)
      name.map { n =>
        val label = _label(features)
        val affiliation = _affiliation(features)
        val base = _base(features)
        val traits = _traits(features)
        val powertypes = _powertypes(properites)
        println(s"SimpleModelParser#Builder#_create_command Properties: ${properites}")
        val attributes = _attributes(properites)
        val associations = _associations(properites)
        val operations = Nil
        val smt = _get_statemachine_tables(dox)
        val statemachines = _statemachines(attributes, smt)
        val description = _make_description(contents, n, label)
        CreateObjectCommand(
          description, // Designation.nameLabel(n, label),
          affiliation,
          st,
          base,
          traits,
          powertypes,
          attributes,
          associations,
          operations,
          statemachines
        )
      }
    }

    private def _distill_contents(p: Dox): Dox = p match {
      case m: Document => _distill_contents(m.body)
      case m: Body => _remove_specs(m.sections.head)
      case m: Section => _remove_specs(m)
      case m => Fragment(m)
    }

    private def _remove_specs(p: Section): Section = p.copy(
      contents = p.contents.filterNot(_is_spec_table)
    )

    private def _distill_specs(p: Dox): List[Table] = p.elements.collect {
      case m: Table if _is_spec_table(m) => m
    }

    private def _is_spec_table(p: Dox): Boolean = p match {
      case m: Table => m.getKey.map(_spec_table_names.contains).getOrElse(false)
      case m => false
    }

    private def _make_description(dox: Dox, name: String, label: Option[I18NString]): Description = {
      sealed trait State {
      }
      case object Init extends State {
      }
      case object Resumed extends State {
      }
      case object Content extends State {
      }

      case class Z(
        state: State = Init,
        designation: Option[Designation] = Some(Designation.nameLabel(name, label)),
        title: Option[Dox] = None,
        resume: Resume = Resume.empty,
        specs: Vector[Table] = Vector.empty,
        content: Dox = EmptyDox
      ) {
        def r: Description = {
          Description(designation getOrElse Designation.empty, title, resume, content)
        }

        def +(rhs: Dox) = {
          println(s"SimpleModelParser#_make_description.Z#+ $rhs")
          state match {
            case Init => rhs match {
              case m: Paragraph => _add_resume(m)
              case m: Table => _add_spec(m)
              case m => _add_content(rhs)
            }
            case Resumed => _add_content(rhs)
            case Content => _add_content(rhs)
          }
        }

        private def _add_resume(p: Paragraph) = copy(
          state = Resumed,
          resume = Resume.summary(p)
        )

        private def _add_spec(p: Table) = copy(
          state = Resumed,
          specs = specs :+ p
        )

        private def _add_content(p: Dox) = copy(
          state = Content,
          content = Dox.addContent(content, p)
        )
      }
      println(s"SimpleModelParser#_make_description $dox")
      dox.elements./:(Z() )(_+_).r
    }

    private def _parse_entity(p: LogicalBlock, st: MDomainStereotype) =
      _create_command(p, st).map(MDomainEntity.apply)

    private def _parse_trait(p: LogicalBlock) =
      _create_command(p, MDomainTraitStereotype).map(MDomainTrait.apply)

    private def _parse_voucher(p: LogicalBlock) =
      _create_command(p, MDomainVoucherStereotype).map(MDomainVoucher.apply)

    private def _get_feature_table(p: Dox): Option[Table] = p match {
      case m: Document => _get_feature_table(m.body)
      case m: Body => m.elements.toStream.flatMap(_get_feature_table).headOption
      case m: Section => _get_feature_table_in_section(m)
      case _ => None
    }

    private def _get_feature_table_in_section(p: Section): Option[Table] = {
      println(s"SimpleModelParser#Builder#_get_feature_table_in_section Secion: $p")
      // println(s"SimpleModelParser#Builder#_get_feature_table_in_section table: ${p.tableList}")
      p.tableList match {
        case Nil => None
        case x :: Nil => Vector(x).find(config.isFeatureTable)
        case xs => xs.find(config.isFeatureTable)
      }
    }

    // private def _get_feature_table(p: Section): Option[Table] = p.collectFirst {
    //   case m: Table if config.isFeatureTable(m) => m
    //     // p match {
    //     //   case m: Table => ???
    //     //   case m: Section =>
    //     //     if (m.titleName.toLowerCase == "???")
    //     //       _get_feature_table(m)
    //     //     else
    //     //       None
    //     //   case m => _get_feature_table(m)
    //     // }
    // }

    private def _get_property_table(p: Dox): Option[Table] = p match {
      case m: Document => _get_property_table(m.body)
      case m: Body => m.elements.toStream.flatMap(_get_property_table).headOption
      case m: Section => _get_property_table_in_section(m)
      case _ => None
    }

    private def _get_property_table_in_section(p: Section): Option[Table] = {
      println(s"SimpleModelParser#Builder#_get_property_table_in_section Section: $p")
      println(s"SimpleModelParser#Builder#_get_property_table_in_section Property table: ${p.tableList}")
      p.tableList match {
        case Nil => None
        case x :: Nil => Vector(x).find(config.isPropertyTable)
        case xs => xs.find(config.isPropertyTable)
      }
    }

    // private def _get_property_table(p: Dox): Option[Table] = p.collectFirst {
    //   case m: Table if config.isPropertyTable(m) => m
    //     // p match {
    //     //   case m: Table => ???
    //     //   case m: Section =>
    //     //     if (m.titleName.toLowerCase == "???")
    //     //       _get_feature_table(m)
    //     //     else
    //     //       None
    //     //   case m => _get_feature_table(m)
    //     // }
    // }

    private def _get_statemachine_tables(p: Dox): List[Table] = p.elements.collect {
      case m: Table if config.isStatemachineTable(m) => m
    }

    // private def _get_properties_table(p: Dox): Option[Table] =
    //   p match {
    //     case m: Section =>
    //       if (m.titleName.toLowerCase == "???")
    //         _get_properties_table(m)
    //       else
    //         None
    //   }

    // private def _get_properties_table(p: Section) = {
    //   p.tableList match {
    //     case Nil => None
    //     case x :: Nil => Vector(x).find(config.isPropertyTable)
    //     case xs => xs.find(config.isPropertyTable)
    //   }
    // }

    private def _name(dox: Dox): ValidationNel[ParseMessage, String] = {
      // dox.elements.collect {
      //   case m: Body => ??? // Success(m.elements.sections.head.titleName)
      // }.getOrElse(???)
      val body = dox.elements.collect {
        case m: Body => m
      }.head
      Success(body.sections.head.titleName)
    }

    private def _affiliation(features: Option[Record]): MPackageRef = features.flatMap(_package).getOrElse(MPackageRef.default)

    private def _package(features: Record): Option[MPackageRef] =
      features.getStringCaseInsensitive(config.packageNames).map(I18NString.parse).map(MPackageRef.create)

    private def _label(features: Option[Record]): Option[I18NString] = features.flatMap(_label)

    private def _label(features: Record): Option[I18NString] =
      features.getStringCaseInsensitive(config.labelNames).map(I18NString.parse)

    private def _base(features: Option[Record]): Option[MObjectRef] = features.flatMap(_base)

    private def _base(features: Record): Option[MObjectRef] =
      features.getStringCaseInsensitive(config.baseClassNames).map(MObjectRef(pkg, _))

    private def _traits(features: Option[Record]): List[MTraitRef] = features.map(_traits).getOrElse(Nil)

    private def _traits(features: Record): List[MTraitRef] = 
      features.getStringCaseInsensitive(config.traitNames).
        map(x => Strings.totokens(x).map(MTraitRef(pkg, _))).
        getOrElse(Nil)

    private def _powertypes(properties: List[Record]): List[MPowertypeRef] =
      properties.withFilter(_.getStringCaseInsensitive(config.attributeKindNames).map(x => config.powertypeNames.exists(_.equalsIgnoreCase(x))).getOrElse(false)).
        map(MPowertypeRef(pkg, _))

    private def _attributes(properties: List[Record]): List[MAttribute] = 
      properties.withFilter(_.getStringCaseInsensitive(config.attributeKindNames).map(x => config.attributeNames.exists(_.equalsIgnoreCase(x))).getOrElse(false)).
        map(MAttribute(config, _))

    private def _associations(properties: List[Record]): List[MAssociation] =
      properties.withFilter(_.getStringCaseInsensitive(config.attributeKindNames).map(x => config.associationNames.exists(_.equalsIgnoreCase(x))).getOrElse(false)).
        map(MAssociation(config, _))

    private def _statemachines(attrs: List[MAttribute], ps: Seq[Table]): List[MStateMachineRef] = {
      Nil // TODO
    }
  }
}
