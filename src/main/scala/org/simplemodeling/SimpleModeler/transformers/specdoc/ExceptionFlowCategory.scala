package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Nov. 23, 2008
 *  version Dec. 18, 2010
 *  version Jun. 17, 2020
 * @version Oct. 11, 2020
 */
class ExceptionFlowCategory extends SDCategory("例外脚本") {
  override def tableHead = Dox.list("名前", "派生", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(
      anEntity.anchor,
      anEntity.feature(MFeature.OwnerClass).value,
      anEntity.caption,
      anEntity.note
    )
  }
}
