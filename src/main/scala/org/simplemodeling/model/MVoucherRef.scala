package org.simplemodeling.model

/*
 * Derived from SDocumentRelationship and SMDocumentRelationship.
 * 
 * @since   Mar. 17, 2009
 *  version May.  6, 2020
 *  version Sep. 22, 2020
 * @version Oct.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class MVoucherRef(
  packageRef: MPackageRef,
  voucherName: String
) extends MReference {
  def relationshipType = MRelationshipType(voucherName, packageRef)
  // def targetName: String = voucherName
  // def targetPackageName: String = packageRef.packageName
  def voucher(implicit sm: SimpleModel): MVoucher = ???
}
