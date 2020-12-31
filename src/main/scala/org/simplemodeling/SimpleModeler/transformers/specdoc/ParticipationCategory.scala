package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Oct. 24, 2008
 *  version Dec. 18, 2010
 *  version Jun. 17, 2020
 * @version Oct. 11, 2020
 */
class ParticipationCategory extends SDCategory("参加") {
  override def tableHead = Dox.list("要素", "種類", "種別", "役割種類", "役割名", "派生", "説明", "備考")

  override def tableRow(anEntity: SDEntity): Seq[Dox] = {
    Array(
      anEntity.feature(MFeature.Element).value,
      anEntity.feature(MFeature.Type).value,
      anEntity.feature(MFeature.Kind).value,
      anEntity.feature(MFeature.RoleType).value,
      anEntity.feature(MFeature.RoleName).value,
      anEntity.feature(MFeature.OwnerClass).value,
      anEntity.caption,
      anEntity.note
    )
  }
}
