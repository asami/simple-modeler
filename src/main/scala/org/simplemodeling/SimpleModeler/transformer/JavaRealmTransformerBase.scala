package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.Strings
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker.JavaClassDefinition
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SimpleModel2JavaRealmTransformerBase (Apr. 18, 2011 - Nov. 19, 2013)
 * 
 * @since   Dec.  8, 2019
 *  version Dec. 21, 2019
 *  version Feb. 11, 2020
 *  version Mar.  8, 2020
 *  version Apr. 25, 2020
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait JavaRealmTransformerBase extends ProgramRealmTransformerBase {
  val fileSuffix = "java"

  protected def make_Entity(model: PModel, p: PEntity): String = {
    val aspects = Nil
    val maker = new JavaClassDefinition(context, model, aspects, p)
    maker.build()
    maker.toText
  }

  protected def package_File_Pathname(p: PObject): PathName = {
    val prjdir = "src" // TODO
    PathName(prjdir) :+ p.affiliation.packageName.replace('.', '/')
  }

  /*
   * Legacy
   */
  protected def make_Entity(p: MEntity): String = {
    // val aspects = Nil
    // val model = ???
    // val po = MPEntity(p)
    // val maker = new JavaClassDefinition(context, model, aspects, po)
    // maker.build()
    // maker.toText
    ???
  }

  protected def package_To_Pathname(p: MObject): String = ???
}
