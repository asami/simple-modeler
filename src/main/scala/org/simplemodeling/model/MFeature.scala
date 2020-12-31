package org.simplemodeling.model

import org.smartdox.{Dox, Description}
import org.smartdox.specdoc.SDFeature
import org.goldenport.values.Designation

/*
 * Derived from SFeature and SMFeature.
 * 
 * @since   Sep. 10, 2008
 *  version Oct. 18, 2008
 *  version May. 10, 2020
 *  version Jun. 16, 2020
 *  version Aug. 15, 2020
 * @version Oct.  4, 2020
 * @author  ASAMI, Tomoharu
 */
case class MFeature(
  designation: Designation,
  owner: MElement,
  value: Dox,
  description: Description = Description.empty
) extends MElement {
  override def getAffiliation: Option[MPackageRef] = owner.getAffiliation
  // def base: Option[val] = None = Designation.nameLabel
  // def traits: List[MTraitRef] = Nil
  // def powertypes: List[MPowertypeRef] = Nil
  // def stateMachines: List[MStateMachineRef] = Nil
  // def attributes: List[MAttribute] = Nil
  // def associations: List[MAssociation] = Nil
  // def operations: List[MOperation] = Nil
  // def ports: List[MPort] = Nil
  // def roles: List[MRoleRef] = Nil
  // def services: List[MServiceRef] = Nil
  // def rules: List[MRuleRef] = Nil
  // def vouchers: List[MVoucherRef] = Nil

  def toSDFeature: SDFeature = SDFeature(designation, value, description.content)
}

object MFeature {
  val Component = Designation.nameLabel("Component", "コンポーネント")

  val Package = Designation.nameLabel("Package", "パッケージ")

  val Name = Designation.nameLabel("Name", "名前")

  val Class = Designation.nameLabel("Class", "クラス")

  val BaseClass = Designation.nameLabel("BaseClass", "基底クラス")

  val DerivedClasses = Designation.nameLabel("DerivedClasses", "派生クラス")

  val OwnerClass = Designation.nameLabel("OwnerClass", "所有クラス")

  val UserClass = Designation.nameLabel("UserClass", "使用クラス") // ??? 使用者クラス

  val ReceiverClass = Designation.nameLabel("ReceiverClass", "受信クラス")

  val Element = Designation.nameLabel("Element", "要素")

  val Type = Designation.nameLabel("Type", "種類")

  val Kind = Designation.nameLabel("Kind", "種別")

  val Powertype = Designation.nameLabel("Powertype", "区分")

  // currently unsed
  val Role = Designation.nameLabel("Role", "役割")

  val RoleType = Designation.nameLabel("RoleType", "役割種類")

  val RoleKind = Designation.nameLabel("RoleKind", "役割種別")

  val RoleName = Designation.nameLabel("RoleName", "役割名")

  val UseKind = Designation.nameLabel("UseKind", "使用種別")

  val Multiplicity = Designation.nameLabel("Multiplicity", "多重度")

  val Term = Designation.nameLabel("Term", "用語")

  /*
   object StateMachine extends GKey("StateMachine") {
   set_label("状態機械")
   }
   */

  val Input = Designation.nameLabel("Input", "入力")

  val Output = Designation.nameLabel("Output", "出力")

  val InOut = Designation.nameLabel("InOut", "入出力")

  val PreState = Designation.nameLabel("PreState", "事前状態")

  val PostState = Designation.nameLabel("PostState", "事後状態")

  val Guard = Designation.nameLabel("Guard", "ガード")

  val IncludeTask = Designation.nameLabel("IncludeTask", "Includeタスク")

  val IncludeUsecase = Designation.nameLabel("IncludeUsecase", "Includeユースケース")

  val ExtendUsecase = Designation.nameLabel("ExtendUsecase", "Extendユースケース")

  val UserTask = Designation.nameLabel("userTask", "利用者タスク")

  val UserUsecase = Designation.nameLabel("userUsecase", "利用者ユースケース")

  // def rawValue(value: SDoc)(): SDoc = value

  // def apply(name: Designation, owner: MElement, value: Dox): MFeature =
  //   MFeature(name, owner, value) 
}
