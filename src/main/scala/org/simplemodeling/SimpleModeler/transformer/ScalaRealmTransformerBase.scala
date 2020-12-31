package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.Strings
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker.ScalaClassDefinition
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SimpleModel2ScalaRealmTransformerBase (Nov. 19, 2012)
 * 
 * @since   Dec.  8, 2019
 *  version Dec.  8, 2019
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
trait ScalaRealmTransformerBase extends ProgramRealmTransformerBase {
  val fileSuffix = "scala"

  protected def make_Entity(model: PModel, p: PEntity): String = {
    val aspects = Nil
    val maker = new ScalaClassDefinition(context, model, aspects, p)
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
