package org.simplemodeling.SimpleModeler.transformers.specdoc

import org.goldenport.values.Designation
import org.smartdox.{Dox, Text, Fragment, Ul, Li}
import org.smartdox.Description
import org.smartdox.specdoc._
import org.simplemodeling.model._

/*
 * @since   Jul. 28, 2020
 *  version Aug.  3, 2020
 *  version Oct.  4, 2020
 * @version Dec. 21, 2020
 * @author  ASAMI, Tomoharu
 */
trait GlossaryPart { self: SpecDocTransformer =>
  def add_glossary(): SDEntity = {
    val designation = Designation("用語集")
    val desc = Description.empty // TODO
    SDEntity(designation, self.Category_Glossary, desc)
  }
    // def add_glossary() {
    //   val glossary = new SDSummary("glossary")
    //   specPackage.addSummary(glossary)
    //   glossary.title = "用語集"
    //   glossary.tableHead = List("用語", "説明", "オブジェクト", "種類")
    //   glossary.tableRow = (anEntity: SDEntity) =>
    //   Array(
    //     anEntity.feature(FeatureTerm).value,
    //     anEntity.summary,
    //     anEntity.anchor,
    //     anEntity.feature(FeatureType).value)
    // }
}
