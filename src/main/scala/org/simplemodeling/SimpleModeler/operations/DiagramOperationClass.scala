package org.simplemodeling.SimpleModeler.operations

import org.goldenport.RAISE
import org.goldenport.cli._
import org.simplemodeling.model._
import org.simplemodeling.parser._
import org.simplemodeling.SimpleModeler._
import org.simplemodeling.SimpleModeler.generators.uml._

/*
 * Derived from DiagramGeneratorService.
 * 
 * @since   Nov.  7, 2011
 *  version Dec.  6, 2011
 *  version Feb. 28, 2012
 *  version Jun. 17, 2012
 *  version Dec. 17, 2012
 * @version May. 19, 2020
 * @author  ASAMI, Tomoharu
 */
case object DiagramOperationClass extends OperationClassWithOperation {
  val request = spec.Request.empty
  val response = spec.Response.empty
  val specification = spec.Operation("diagram", request, response)

  def apply(env: Environment, req: Request): Response = {
    val in = req.arguments(0).toInputText
    val pkgname = req.getPropertyString("package")
    val context = env.appEnvironment.asInstanceOf[Context]
    val model = _parse(in)
    val tx = new ClassDiagramGenerator(context, model)
    val pkg: MPackage = pkgname.map {
      case "" => model.root
      case m => model.root.getPackage(m).getOrElse(
        RAISE.noSuchElementFault(s"package $m not found.")
      )
    }.getOrElse(model.root)
    val theme = HilightPerspective
    val r = tx.makeClassDiagramPng(pkg, theme)
    // FileRealmResponse(r.realm)
    FileResponse(r)
  }

  private def _parse(p: String): SimpleModel = {
    val config = SimpleModelParser.Config.default
    val parser = SimpleModelParser(config)
    parser(p)
  }
}
