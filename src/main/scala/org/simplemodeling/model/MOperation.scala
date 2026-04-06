package org.simplemodeling.model

import org.smartdox.Description
import org.simplemodeling.SimpleModeler.generator.scala.Generator.GenM

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
name, List(param), result) *  version Feb. 27, 2026
name, List(param), result) * @version Mar. 13, 2026
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
    result: MResult,
    body: Option[() => GenM[Unit]] = None,
    access: Option[MComponent.OperationAccess] = None
  )
  object Core {
    trait Holder {
      def operationCore: Core

      def descriptor = operationCore.descriptor
      def parameters = operationCore.parameters
      def result = operationCore.result
      def body = operationCore.body
      def access = operationCore.access
    }
  }

  case class Instance(
    elementCore: MElement.Core,
    operationCore: Core
  ) extends MOperation {
  }

  def query(name: String, param: MParameter, result: MResult): MOperation =
    query(name, List(param), result)

  def query(name: String, param: MParameter, result: MResult, description: Description): MOperation =
    query(name, List(param), result, description)

  def query(name: String, params: List[MParameter], result: MResult): MOperation =
    query(name, params, result, Description.name(name))

  def query(name: String, params: List[MParameter], result: MResult, description: Description): MOperation =
    Instance(
      MElement.Core(description),
      Core(Descriptor.query, params, result)
    )

  def queryBody(
    name: String,
    param: MParameter,
    result: MResult
  )(body: GenM[Unit]): MOperation = queryBody(name, List(param), result)(body)

  def queryBody(
    name: String,
    param: MParameter,
    result: MResult,
    description: Description
  )(body: GenM[Unit]): MOperation = queryBody(name, List(param), result, description)(body)

  def queryBody(
    name: String,
    params: List[MParameter],
    result: MResult
  )(body: GenM[Unit]): MOperation = queryBody(name, params, result, Description.name(name))(body)

  def queryBody(
    name: String,
    params: List[MParameter],
    result: MResult,
    description: Description,
    access: Option[MComponent.OperationAccess] = None
  )(body: GenM[Unit]): MOperation = Instance(
    MElement.Core(description),
    Core(Descriptor.query, params, result, Some(() => body), access)
  )

  // def query(name: String, param: MParameter, result: MDataType): MOperation =
  //   query(name, param, MResult(result))

  // def query(name: String, param: MParameter, result: MObject): MOperation =
  //   query(name, param, MResult(result))

  // def query(name: String, param: MParameter, result: MObjectRef): MOperation =
  //   query(name, param, MResult(result))

  def command(name: String, param: MParameter): MOperation =
    command(name, List(param))

  def command(name: String, param: MParameter, description: Description): MOperation =
    command(name, List(param), MResult.unit, description)

  def command(name: String, params: List[MParameter]): MOperation =
    command(name, params, MResult.unit)

  def command(name: String, params: List[MParameter], description: Description): MOperation =
    command(name, params, MResult.unit, description)

  def command(name: String, params: List[MParameter], result: MResult): MOperation =
    command(name, params, result, Description.name(name))

  def command(name: String, params: List[MParameter], result: MResult, description: Description): MOperation =
    Instance(
      MElement.Core(description),
      Core(Descriptor.command, params, result)
    )

  def commandBody(name: String, param: MParameter)(body: GenM[Unit]): MOperation =
    commandBody(name, List(param), MResult.unit)(body)

  def commandBody(name: String, param: MParameter, description: Description)(body: GenM[Unit]): MOperation =
    commandBody(name, List(param), MResult.unit, description)(body)

  def commandBody(name: String, params: List[MParameter], result: MResult)(body: GenM[Unit]): MOperation =
    commandBody(name, params, result, Description.name(name))(body)

  def commandBody(
    name: String,
    params: List[MParameter],
    result: MResult,
    description: Description,
    access: Option[MComponent.OperationAccess] = None
  )(body: GenM[Unit]): MOperation =
    Instance(
      MElement.Core(description),
      Core(Descriptor.command, params, result, Some(() => body), access)
    )
}
