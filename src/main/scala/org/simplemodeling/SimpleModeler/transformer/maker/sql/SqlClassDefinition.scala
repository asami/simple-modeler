package org.simplemodeling.SimpleModeler.transformer.maker.sql

import scalaz._
import Scalaz._
import java.text.SimpleDateFormat
import java.util.TimeZone
import com.asamioffice.goldenport.text.UString
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/**
 * @since   May.  3, 2012
 *  version Jun. 16, 2012
 *  version Dec. 29, 2012
 *  version Feb. 21, 2013
 *  version Mar.  2, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class SqlClassDefinition(
  val sqlContext: SqlContext,
  model: PModel,
  anAspects: Seq[SqlAspect],
  val sqlObject: SqlObject,
  maker: JavaMaker = null
) extends GenericClassDefinition(sqlContext, model, anAspects, sqlObject) with JavaMakerHolder {
  type ATTR_DEF = SqlClassAttributeDefinition

  require (pobject != null, "SqlClassDefinition: sql object should not be null.")
  require (UString.notNull(sqlObject.name), "SqlClassDefinition: SqlObjectEntity.name should not be null.")

  if (maker == null) {
    jm_open(Nil) // XXX SqlAspect
  } else {
    jm_open(maker, Nil) // XXX SqlAspect
  }
  anAspects.foreach(_.openSqlClass(this))

  protected def pln() {
    jm_pln()
  }

  override def toText = {
    jm_to_text
  }

  override protected def attribute(attr: PAttribute): ATTR_DEF = {
    new NullSqlClassAttributeDefinition(sqlContext, model, Nil, attr, this, maker) // XXX SqlAspect
  }

  override protected def class_open_body {
  }

  override protected def class_close_body {
  }

  override protected def attribute_variables_Prologue {
  }

  override protected def attribute_variables_Epilogue {
    jm_pln("CREATE TABLE %s (", sqlContext.sqlTableName(sqlObject))
    jm_indent_up
    wholeAttributeDefinitions.filter(_is_db_attribute).map { x =>
      () => x.ddl
    } intersperse {
      () => jm_pln(",")
    } map (_())
    jm_pln
    jm_indent_down
    jm_p(")")
    create_Epilogue
    jm_pln(";")
  }

  private def _is_db_attribute(attr: SqlClassAttributeDefinition): Boolean = {
    attr.attr.isSingle && !attr.attr.isParticipation
  }

  protected def create_Epilogue {}
}
