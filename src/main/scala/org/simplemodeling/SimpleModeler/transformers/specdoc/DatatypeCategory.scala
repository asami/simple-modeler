package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._

/*
 * @since   Oct. 25, 2008
 *  version Dec. 18, 2010
 *  version Jun. 14, 2020
 * @version Oct. 11, 2020
 */
class DatatypeCategory extends SDCategory("データタイプ") {
  override def tableHead = Dox.list("名前", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(anEntity.anchor,
	  anEntity.caption,
	  anEntity.note)
  }
}
