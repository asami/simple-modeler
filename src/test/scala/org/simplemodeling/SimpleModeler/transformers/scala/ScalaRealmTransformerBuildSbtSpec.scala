package org.simplemodeling.SimpleModeler.transformers.scala

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.simplemodeling.SimpleModeler.transformer.ScalaRealmTransformerBase
import org.simplemodeling.SimpleModeler.transformer.maker.PContext

/*
 * @since   May. 20, 2026
 *  version May. 20, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
class ScalaRealmTransformerBuildSbtSpec extends AnyFunSuite with Matchers {
  test("build.sbt scaffold uses only direct CNCF and ScalaTest dependencies") {
    val content = new ScalaRealmTransformerBase {
      override def context: PContext = null
    }.buildSbtContent

    content should include("""val scala3Version = "3.3.8"""")
    content should include("""val cncfversion = sampleVersion("CNCF_VERSION", "cncf-version.conf", "0.4.8")""")
    content should include(""""org.goldenport" %% "goldenport-cncf"""")
    content should include(""""org.goldenport" %% "goldenport-cncf" % cncfversion""")
    content should include(""""org.scalatest" %% "scalatest"""")
    content should not include "0.4.2-SNAPSHOT"
    content should not include "junit-interface"
    content should not include "cats-core"
    content should not include "kittens"
    content should not include "spire"
    content should not include "circe-core"
    content should not include "cats-testkit"
    content should not include "discipline-core"
    content should not include "simplemodeling-model"
    content should not include "cncf-collaborator-api"
    content should not include "dependencyOverrides"
  }

  test("Scala 3 scaffold writes managed sources for Scala 3.3.8") {
    val pathname = new org.simplemodeling.SimpleModeler.transformers.Scala3RealmTransformer(null) {
      def managedSourcePathname: String = source_managed_main_pathname
    }.managedSourcePathname

    pathname shouldBe "target/scala-3.3.8/src_managed/main/scala"
  }
}
