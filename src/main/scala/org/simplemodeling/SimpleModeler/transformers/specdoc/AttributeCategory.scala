package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Oct. 13, 2008
 *  version Dec. 18, 2010
 *  version Jun. 16, 2020
 * @version Oct. 11, 2020
 */
class AttributeCategory extends SDCategory("属性") {
  override def tableHead = Dox.list("名前", "型", "多重度", "派生", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(
      anEntity.anchor,
      anEntity.feature(MFeature.Type).value,
      anEntity.feature(MFeature.Multiplicity).value,
      anEntity.feature(MFeature.OwnerClass).value,
      anEntity.caption,
      anEntity.note
    )
  }
}
