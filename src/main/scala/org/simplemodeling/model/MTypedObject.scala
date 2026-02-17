package org.simplemodeling.model

import org.goldenport.collection.NonEmptyVector

/*
 * @since   Feb. 10, 2026
 * @version Feb. 14, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class MTypedObject() extends MObject
    with MElement.Core.Holder
    with MObject.Core.Holder
    with MTypedObject.Core.Holder {
}

object MTypedObject {
  case class Core(
    typeParameters: NonEmptyVector[MObjectRef]
  )
  object Core {
    trait Holder {
      def typedObjectCore: Core

      def typeParameters = typedObjectCore.typeParameters
    }

    def apply(in: MObjectRef): Core = Core(NonEmptyVector(in))
  }

  case class Instance(
    elementCore: MElement.Core,
    objectCore: MObject.Core,
    typedObjectCore: Core
  ) extends MTypedObject {
  }

  def option(in: MObject): MTypedObject = option(in.toObjectRef)

  def option(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Option"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def list(in: MObject): MTypedObject = list(in.toObjectRef)

  def list(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("List"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def vector(in: MObject): MTypedObject = vector(in.toObjectRef)

  def vector(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Vector"),
    MObject.Core(MPackageRef.lang),
    MTypedObject.Core(in)
  )

  def select(in: MObject): MTypedObject = select(in.toObjectRef)

  def select(in: MObjectRef): MTypedObject = Instance(
    MElement.Core("Select"),
    MObject.Core(MPackageRef.value),
    MTypedObject.Core(in)
  )
}
