package org.simplemodeling.SimpleModeler.operations

import org.goldenport.cli._
import org.simplemodeling.model.SimpleModel
import org.simplemodeling.parser.SimpleModelParser
import org.simplemodeling.SimpleModeler._
import org.simplemodeling.SimpleModeler.transformer.maker.{PContext, PModel}
import org.simplemodeling.SimpleModeler.transformers.Scala12RealmTransformer

/*
 * @since   May. 16, 2020
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
case object ScalaOperationClass extends OperationClassWithOperation {
  val request = spec.Request.empty
  val response = spec.Response.empty
  val specification = spec.Operation("scala", request, response)

  def apply(env: Environment, req: Request): Response = {
    val s = req.arguments(0).toInputText
    val context = new PContext(env.appEnvironment.asInstanceOf[Context])
    val tx = new Scala12RealmTransformer(context)
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
