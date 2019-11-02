package org.simplemodeling.parser

import java.util.Locale
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.goldenport.Strings
import org.goldenport.i18n.I18NString
import org.goldenport.parser._
import org.goldenport.record.v3.{Table => _, _}
import org.simplemodeling.model._
import org.simplemodeling.model.domain._

/*
 * @since   Aug.  8, 2019
 * @version Aug. 10, 2019
 * @author  ASAMI, Tomoharu
 */
case class SimpleModelParser(config: SimpleModelParser.Config) {
  import SimpleModelParser._

  def apply(p: String): SimpleModel = {
    val c = ???
    val blocks = LogicalBlocks.parse(c, p)
    apply(blocks)
  }

  def apply(p: LogicalBlocks): SimpleModel = {
    val xs = p.blocks.flatMap(_parse_by_kind)
    SimpleModel(xs)
  }

  private def _parse_by_kind(p: LogicalBlock): Vector[MElement] = {
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

  private def _parse_resources(p: LogicalBlock) = {
    ???
  }

  private def _parse_resource(p: LogicalBlock) = _parse_entity(p, DResoruceStereotype)

  private def _parse_events(p: LogicalBlock) = {
    ???
  }

  private def _parse_actors(p: LogicalBlock) = {
    ???
  }

  private def _parse_traits(p: LogicalBlock) = {
    ???
  }

  private def _parse_entity(p: LogicalBlock, st: DStereotype) = {
    val doxconfig = Dox2Parser.Config.default // TODO
    val dox = Dox2Parser.parse(doxconfig, p)
    val ft = _get_feature_table(dox)
    val features = ft.map(toPropertyRecord)
    val pt = _get_properties_table(dox)
    val properites = pt.map(toRecords)
    val base = ???
    val traits = ???
    val attributes = ???
    val associations = ???
    val operations = ???
    val statemachines = ???
    DEntity(st, base, traits, attributes, associations, operations, statemachines)
    ???
  }

  private def _get_feature_table(p: Dox): Option[Table] =
    p match {
      case m: Section =>
        if (m.titleName.toLowerCase == "???")
          _get_feature_table(m)
        else
          None
    }

  private def _get_feature_table(p: Section): Option[Table] = {
    p.tableList match {
      case Nil => None
      case x :: Nil => Vector(x).find(config.isFeatureTable)
      case xs => xs.find(config.isFeatureTable)
    }
  }

  private def _get_properties_table(p: Dox): Option[Table] =
    p match {
      case m: Section =>
        if (m.titleName.toLowerCase == "???")
          _get_properties_table(m)
        else
          None
    }

  private def _get_properties_table(p: Section) = {
    p.tableList match {
      case Nil => None
      case x :: Nil => Vector(x).find(config.isPropertyTable)
      case xs => xs.find(config.isPropertyTable)
    }
  }
}

object SimpleModelParser {
  case class Config(
    locale: Locale,
    blockKindMap: Map[BlockKind, Vector[String]]
  ) {
    lazy val _block_kind_vector = blockKindMap.toVector

    def blockKind(p: I18NString): BlockKind = {
      _block_kind_vector.find {
        case (k, vs) => p.containsKey(vs)
      }.map(_._1).getOrElse(UnknownBlock)
    }

    def isFeatureTable(p: Table): Boolean = ???

    def isPropertyTable(p: Table): Boolean = ???
  }

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

  private def _to_symbol(ps: List[Inline]): Symbol = Symbol(ps.map(_.toText).mkString)

  private def _to_field_value(ps: List[Inline]): FieldValue = ps match {
    case Nil => EmptyValue
    case x :: Nil => x match {
      case m: Text => SingleValue(m.toText)
      case m => SingleValue(m)
    }
    case xs => SingleValue(Fragment(xs))
  }
}
