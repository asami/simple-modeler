package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.goldenport.RAISE
import org.goldenport.recorder.Recorder
import org.simplemodeling.model._
// import org.simplemodeling.SimpleModeler.entity.domain._
import org.simplemodeling.SimpleModeler.Context
import org.simplemodeling.SimpleModeler.transformer.maker._
// import org.goldenport.Goldenport.{Application_Version, Application_Version_Build}

/*
 * Dervied from SqlEntityContext.
 *
 * @since   May.  3, 2012
 *  version Dec. 29, 2012
 *  version Mar.  3, 2020
 *  version Apr.  9, 2020
 * @version May.  9, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlContext(context: Context) extends PContext(context) {
  var sqlKind = "mysql" // for RDS

  def createClassDefinition(entity: SqlEntity): SqlClassDefinition = {
    val model = RAISE.notImplementedYetDefect
    sqlKind match {
      case "mysql" => new MySqlClassDefinition(this, model, Nil, entity)
      case _ => new Sql92SqlClassDefinition(this, model, Nil, entity)
    }
  }
}
