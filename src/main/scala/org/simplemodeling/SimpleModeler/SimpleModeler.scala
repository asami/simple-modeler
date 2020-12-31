package org.simplemodeling.SimpleModeler

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.value._
import org.goldenport.record.v3.{Table => _, _}
import org.simplemodeling.model.SimpleModel
import org.simplemodeling.parser.SimpleModelParser
import org.simplemodeling.SimpleModeler.operations._

/*
 * @since   Feb.  2, 2020
 *  version Feb. 16, 2020
 *  version Mar.  8, 2020
 *  version Apr. 30, 2020
 *  version May. 17, 2020
 * @version Jun. 20, 2020
 * @author  ASAMI, Tomoharu
 */
class SimpleModeler(
  config: Config,
  environment: Environment,
  services: Services,
  operations: Operations
) {
  private val _engine = Engine.standard(services, operations)

  def execute(args: Array[String]) = _engine.apply(environment, args)

  def run(args: Array[String]) {
    execute(args)
  }
}

object SimpleModeler {
  case object SimpleModelerServiceClass extends ServiceClass {
    val name = "simplemodeler"
    val defaultOperation = None
    val operations = Operations(
      JavaOperationClass,
      ScalaOperationClass,
      DiagramOperationClass,
      HtmlOperationClass
    )
  }

  def main(args: Array[String]) {
    val env0 = Environment.create(args)
    val config = Config.create(env0)
    val context = new Context(env0, config)
    val env = env0.withAppEnvironment(context)
    val services = Services(
      SimpleModelerServiceClass
    )
    val sm = new SimpleModeler(config, env, services, SimpleModelerServiceClass.operations) // TODO
    sm.run(args)
  }
}
