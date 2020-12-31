package org.simplemodeling.SimpleModeler.transformers.specdoc

// import org.goldenport.sdoc._
// import org.goldenport.entities.specdoc._
// import org.simplemodeling.SimpleModeler.entity._
import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Mar. 21, 2011
 *  version Mar. 21, 2011
 *  version Jun. 17, 2020
 * @version Oct. 11, 2020
 * @author  ASAMI, Tomoharu
 */
class FlowMachineCategory extends SDCategory("フロー機械") {
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
