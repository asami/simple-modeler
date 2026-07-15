package org.simplemodeling.SimpleModeler.transformers

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * @since   May.  5, 2025
 *  version Feb. 11, 2026
 *  version May. 20, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
class Scala3RealmTransformer(
  val context: PContext
) extends ScalaRealmTransformerBase {
  override def source_managed_main_pathname = s"target/scala-$scala3_version/src_managed/main/scala"
}
