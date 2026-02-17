package org.simplemodeling.model

/*
 * @since   Feb. 10, 2026
 * @version Feb. 10, 2026
 * @author  ASAMI, Tomoharu
 */
case class MResult(
  elementCore: MElement.Core = MElement.Core(),
  resultType: MResult.MResultType
) extends MElement with MElement.Core.Holder {
}

object MResult {
  sealed trait MResultType
  case class MUnitResultType() extends MResultType
  case class MDataTypeResultType(datatype: MDataType) extends MResultType
  case class MObjectResultType(o: MObject) extends MResultType
  case class MObjectRefResultType(ref: MObjectRef) extends MResultType

  val unit: MResult = apply(MUnitResultType())

  def apply(p: MResultType): MResult = MResult(resultType = p)

  def apply(o: MObject): MResult = apply(MObjectResultType(o))
  def apply(ref: MObjectRef): MResult = apply(MObjectRefResultType(ref))
  def apply(dt: MDataType): MResult = apply(MDataTypeResultType(dt))

  def option(o: MObject): MResult = MResult(
    MObjectResultType(MTypedObject.option(o))
  )

  def option(o: MObjectRef): MResult = ???

  def option(dt: MDataType): MResult = ???

  def select(o: MObject): MResult = MResult(
    MObjectResultType(MTypedObject.select(o))
  )

  def select(o: MObjectRef): MResult = ???
  def select(dt: MDataType): MResult = ???
}
