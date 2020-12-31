package org.simplemodeling.SimpleModeler.operations

import org.goldenport.RAISE
import org.goldenport.cli._
import org.smartdox._
import org.smartdox.generator.{Context => DoxContext}
import org.smartdox.generators.{SpecDoc2DoxGenerator, Dox2HtmlGenerator}
import org.simplemodeling.model._
import org.simplemodeling.parser._
import org.simplemodeling.SimpleModeler._
import org.simplemodeling.SimpleModeler.transformer.maker._
import org.simplemodeling.SimpleModeler.transformers.specdoc.SpecDocTransformer

/*
 * Derived from HtmlRealmGeneratorService.
 * 
 * @since   Oct. 27, 2008
 *  version Dec. 18, 2010
 *  version Feb. 28, 2012
 *  version Jun. 21, 2020
 *  version Jul. 24, 2020
 *  version Oct. 11, 2020
 * @version Nov. 14, 2020
 * @author  ASAMI, Tomoharu
 */
case object HtmlOperationClass extends OperationClassWithOperation {
  val request = spec.Request.empty
  val response = spec.Response.empty
  val specification = spec.Operation("html", request, response)

  def apply(env: Environment, req: Request): Response = {
    val in = req.arguments(0).toInputText
//    val pkgname = req.getPropertyString("package")
    val model = _parse(in)
    val pmodel = _parse(model)
    val context = new PContext(env.appEnvironment.asInstanceOf[Context])
    val config = SpecDocTransformer.Config()
    val sdtx = new SpecDocTransformer(context, config, model, pmodel)
    val sd = sdtx.transform()
    val ctx = DoxContext.create(env)
    val doxtx = new SpecDoc2DoxGenerator(ctx)
    val dox = doxtx.generate(sd)
    val htmltx = new Dox2HtmlGenerator(ctx)
    val r = htmltx.generate(dox)
    // TODO
    FileRealmResponse(r)
  }

  private def _parse(p: String): SimpleModel = {
    val config = SimpleModelParser.Config.default
    val parser = SimpleModelParser(config)
    parser(p)
  }

  private def _parse(p: SimpleModel): PModel = PModel.parse(p)
}

// class HtmlRealmGeneratorService(aCall: GServiceCall, serviceClass: GServiceClass) extends GService(aCall, serviceClass) {
//   def execute_Service(aRequest: GServiceRequest, aResponse: GServiceResponse) = {
// //println("HtmlRealm : before simpleModel") 2009-03-01
//     val simpleModel = aRequest.entity.asInstanceOf[SimpleModelEntity]
// //println("HtmlRealm : after simpleModel")
//     val sm2SpecDoc = new SimpleModel2SpecDocTransformer(simpleModel)
// //println("HtmlRealm : after specdoc")
//     aRequest.parameterBoolean("html.diagram") match {
//       case Some(value) => {
// 	record_trace("html.diagram = " + value)
// 	sm2SpecDoc.drawDiagram = value
//       }
//       case None => //
//     }
// //    sm2SpecDoc.drawDiagram = false
//     val specDoc = sm2SpecDoc.transform
//     val sd2SDoc = new SpecDoc2SmartDocRealmGenerator(specDoc)
//     val sdoc = sd2SDoc.transform
//     val sdoc2HtmlRealm = new SmartDocRealm2HtmlRealmGenerator(sdoc)
//     val htmlRealm = sdoc2HtmlRealm.toHtmlRealm
//     aResponse.addRealm(htmlRealm)
// //    aResponse += htmlRealm
//   }
// }

// object HtmlRealmGeneratorService extends GServiceClass("html") {
//   description.title = "(preliminary)"

//   def new_Service(aCall: GServiceCall): GService =
//     new HtmlRealmGeneratorService(aCall, this)
// }
