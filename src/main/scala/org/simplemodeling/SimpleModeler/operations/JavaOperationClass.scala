package org.simplemodeling.SimpleModeler.operations

import org.goldenport.cli._
import org.simplemodeling.model.SimpleModel
import org.simplemodeling.parser.SimpleModelParser
import org.simplemodeling.SimpleModeler._
import org.simplemodeling.SimpleModeler.transformer.maker._
import org.simplemodeling.SimpleModeler.transformers.Java6RealmTransformer

/*
 * Derived from JavaRealmGeneratorService.
 * 
 * @since   Dec. 12, 2011
 *  version Feb. 28, 2012
 *  version Nov. 19, 2012
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
case object JavaOperationClass extends OperationClassWithOperation {
  val request = spec.Request.empty
  val response = spec.Response.empty
  val specification = spec.Operation("java", request, response)

  def apply(env: Environment, req: Request): Response = {
    val s = req.arguments(0).toInputText
    val context = new PContext(env.appEnvironment.asInstanceOf[Context])
    val tx = new Java6RealmTransformer(context)
    val model = _parse(s)
    val pmodel = _parse(model)
    val r = tx.transform(pmodel)
    FileRealmResponse(r.realm)
  }

  private def _parse(p: String): SimpleModel = {
    val config = SimpleModelParser.Config.default
    val parser = SimpleModelParser(config)
    parser(p)
  }

  private def _parse(p: SimpleModel): PModel = PModel.parse(p)
}
