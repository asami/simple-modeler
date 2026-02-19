package org.simplemodeling.model

import org.goldenport.collection.NonEmptyVector

/*
 * @since   Feb. 10, 2026
 * @version Feb. 20, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class MTypedObject() extends MObject
    with MElement.Core.Holder
    with MObject.Core.Holder
    with MTypedObject.Core.Holder {
}

object MTypedObject {
  sealed trait Slot
  object Slot {
    case class ObjectRef(ref: MObjectRef) extends Slot
    case class ObjectBody(o: MObject) extends Slot
  }

  case class Core(
    typeParameters: NonEmptyVector[Slot]
  )
  object Core {
    trait Holder {
      def typedObjectCore: Core

      def typeParameters = typedObjectCore.typeParameters
    }

    def apply(in: MObject): Core = Core(NonEmptyVector[Slot](Slot.ObjectBody(in)))

    def apply(in: MObjectRef): Core = Core(NonEmptyVector[Slot](Slot.ObjectRef(in)))
  }

  case class Instance(
    elementCore: MElement.Core,
    objectCore: MObject.Core,
    typedObjectCore: Core
  ) extends MTypedObject {
  }

  def option(in: MObject): MTypedObject = Instance(
    MElement.Core("Option"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def option(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Option"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def list(in: MObject): MTypedObject = Instance(
    MElement.Core("List"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def list(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("List"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def vector(in: MObject): MTypedObject = Instance(
    MElement.Core("Vector"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def vector(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Vector"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def query(in: MObject): MTypedObject = Instance(
    MElement.Core("Query"),
    MObject.Core(MPackageRef.directive),
    MTypedObject.Core(in)
  )

  def query(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Query"),
    MObject.Core(MPackageRef.directive),
    MTypedObject.Core(in)
  )

  def searchresult(in: MObject): MTypedObject = Instance(
    MElement.Core("SearchResult"),
    MObject.Core(MPackageRef.directive),
    MTypedObject.Core(in)
  )

  def searchresult(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("SearchResult"),
    MObject.Core(MPackageRef.directive),
    MTypedObject.Core(in)
  )
}
