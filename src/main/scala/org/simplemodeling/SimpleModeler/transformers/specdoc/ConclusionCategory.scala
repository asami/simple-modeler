package org.simplemodeling.SimpleModeler.transformers.specdoc

import org.smartdox._
import org.smartdox.specdoc._
import org.simplemodeling.model.MFeature

/*
 * @since   Aug. 10, 2020 @ Kansuirou
 * @version Aug. 10, 2020
 */
class ConclusionCategory extends SDCategory("まとめ") {
  def tableHead: Seq[org.smartdox.Dox] = Nil
  def tableRow(anEntity: SDEntity): Seq[Dox] = Nil
}
