package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Nov. 10, 2008
 *  version Dec. 18, 2010
 *  version Jun. 17, 2020
 * @version Oct. 11, 2020
 */
class UseCategory extends SDCategory("使用") {
  override def tableHead = Dox.list("名前", "オブジェクト", "使用種別", "使用者", "受信者", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(
      anEntity.anchor,
      anEntity.feature(MFeature.Class).value,
      anEntity.feature(MFeature.UseKind).value,
      anEntity.feature(MFeature.UserClass).value,
      anEntity.feature(MFeature.ReceiverClass).value,
      anEntity.caption,
      anEntity.note
    )
  }
}
