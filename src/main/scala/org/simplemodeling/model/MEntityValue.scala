package org.simplemodeling.model

/*
 * @since   Feb. 18, 2026
 *  version Feb. 19, 2026
 * @version Mar. 14, 2026
 * @author  ASAMI, Tomoharu
 */
case class MEntityValue(
  entity: MEntity,
  kind: MEntityValue.Kind
) extends MValue {
  def description = entity.description // TODO
  def affiliation = entity.affiliation
  def attributes = entity.attributes
  def base = None
  def traits = Nil
  def powertypes = Nil
  def stereotypes = Nil
  def operations = Nil
}

object MEntityValue {
  sealed trait Kind
  sealed trait InOut { self => Kind }
  sealed trait Input extends InOut
  sealed trait Output extends InOut
  sealed trait Action { self => Kind }
  sealed trait CommandAction extends Action
  sealed trait QueryAction extends Action
  object Kind {
    case object Create extends Kind with Input with CommandAction
    case object Save extends Kind with Input with CommandAction
    case object Update extends Kind with Input with CommandAction
    case object Query extends Kind with Input with QueryAction
    case object Whole extends Kind with Output
    case object Summary extends Kind with Output
  }

  def create(p: MEntity): MEntityValue = MEntityValue(p, Kind.Create)
  def save(p: MEntity): MEntityValue = MEntityValue(p, Kind.Save)
  def update(p: MEntity): MEntityValue = MEntityValue(p, Kind.Update)
  def query(p: MEntity): MEntityValue = MEntityValue(p, Kind.Query)
  def whole(p: MEntity): MEntityValue = MEntityValue(p, Kind.Whole)
  def summary(p: MEntity): MEntityValue = MEntityValue(p, Kind.Summary)
}
