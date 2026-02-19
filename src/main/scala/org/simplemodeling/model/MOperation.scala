package org.simplemodeling.model

import org.smartdox.Description

/*
 * derived from SOperation and SMOperation.
 *
 * @since   Oct. 24, 2008
 *  version Sep. 21, 2009
 *  version Nov. 12, 2012
 *  version Dec.  6, 2012
 *  version Aug.  8, 2019
 *  version Dec. 14, 2019
 *  version Apr. 25, 2020
 * @version Feb. 19, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class MOperation extends MElement
    with MElement.Core.Holder
    with MOperation.Core.Holder {
}

object MOperation {
  sealed trait Kind
  object Kind {
    case object Command extends Kind
    case object Query extends Kind
  }

  case class Descriptor(
    kind: Kind
  )
  object Descriptor {
    val query = Descriptor(Kind.Query)
    val command = Descriptor(Kind.Command)
  }

  case class Core(
    descriptor: Descriptor,
    parameters: List[MParameter],
    result: MResult
  )
  object Core {
    trait Holder {
      def operationCore: Core

      def descriptor = operationCore.descriptor
      def parameters = operationCore.parameters
      def result = operationCore.result
    }
  }

  case class Instance(
    elementCore: MElement.Core,
    operationCore: Core
  ) extends MOperation {
  }

  def query(name: String, param: MParameter, result: MResult): MOperation =
    query(name, List(param), result)

  def query(name: String, params: List[MParameter], result: MResult): MOperation =
    Instance(
      MElement.Core(name),
      Core(Descriptor.query, params, result)
    )

  // def query(name: String, param: MParameter, result: MDataType): MOperation =
  //   query(name, param, MResult(result))

  // def query(name: String, param: MParameter, result: MObject): MOperation =
  //   query(name, param, MResult(result))

  // def query(name: String, param: MParameter, result: MObjectRef): MOperation =
  //   query(name, param, MResult(result))

  def command(name: String, param: MParameter): MOperation =
    command(name, List(param))

  def command(name: String, params: List[MParameter]): MOperation =
    command(name, params, MResult.unit)

  def command(name: String, params: List[MParameter], result: MResult): MOperation =
    Instance(
      MElement.Core(name),
      Core(Descriptor.command, params, result)
    )
}
