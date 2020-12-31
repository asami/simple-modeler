package org.simplemodeling.SimpleModeler.transformers.specdoc

import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Dec. 21, 2020
 * @version Dec. 21, 2020
 */
class GlossaryCategory extends SDCategory("用語集") {
  override def tableHead = Dox.list("名前", "派生", "説明", "備考") // TODO

  override def tableRow(anEntity: SDEntity): Seq[Dox] = { // TODO
    Array(anEntity.anchor,
	  anEntity.feature(MFeature.OwnerClass).value,
	  anEntity.caption,
	  anEntity.note)
  }
}
