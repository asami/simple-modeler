package org.simplemodeling.SimpleModeler.transformer.maker.sql

// import java.io.BufferedWriter
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May.  3, 2012
 *  version Dec. 29, 2012
 *  version Mar.  3, 2020
 *  version Apr. 25, 2020
 *  version May.  3, 2020
 * @version Aug.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlEntity(aContext: SqlContext) extends SqlObject(aContext) with PEntity {
  def description: Description = Description.empty
  def attributes: List[PAttribute] = ???
  def operations: List[POperation] = ???
  // override protected def class_Definition = {
  //   aContext.createClassDefinition(this)
  // }
/*
  override protected def write_Content(out: BufferedWriter) {
    val klass = new Sql92SqlClassDefinition(aContext, Nil, SqlEntityEntity.this)
    klass.build()
    out.append(klass.toText)
    out.flush
  }
*/
}
