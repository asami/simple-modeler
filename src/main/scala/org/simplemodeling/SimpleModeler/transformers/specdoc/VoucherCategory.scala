package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Aug.  8, 2020
 * @version Oct. 11, 2020
 * @author  ASAMI, Tomoharu
 */
class VoucherCategory extends SDCategory("伝票") {
  override def tableHead = Dox.list("名前", "種別", "区分", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(
      anEntity.anchor,
      anEntity.feature(MFeature.Kind).value,
      anEntity.feature(MFeature.Powertype).value,
      anEntity.caption,
      anEntity.note
    )
  }
}
