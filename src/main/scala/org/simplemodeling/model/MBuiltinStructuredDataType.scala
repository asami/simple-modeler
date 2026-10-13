package org.simplemodeling.model

import org.smartdox.Description

/*
 * @since   Oct.  5, 2026
 * @version Oct.  5, 2026
 * @author  ASAMI, Tomoharu
 */
sealed trait MBuiltinStructuredDataType extends MAttributeType {
  def logicalIdentity: String
  def runtimePackageName: String
  def runtimeClassName: String
  def affiliation: MPackageRef = {
    MPackageRef(runtimePackageName)
  }
  override def getAffiliation: Option[MPackageRef] = {
    Some(affiliation)
  }
  override def constraints: Seq[MConstraint] = {
    Nil
  }
}

object MBuiltinStructuredDataType {
  case object Json extends MBuiltinStructuredDataType {
    val logicalIdentity = "json"
    val runtimePackageName = "io.circe"
    val runtimeClassName = "Json"
    val description = Description.name("Json")
  }

  case object SemiStructuredData extends MBuiltinStructuredDataType {
    val logicalIdentity = "semiStructuredData"
    val runtimePackageName = "org.goldenport.record"
    val runtimeClassName = "Record"
    val description = Description.name("SemiStructuredData")
  }

  def get(spelling: String): Option[MBuiltinStructuredDataType] = {
    spelling.trim.toLowerCase(java.util.Locale.ROOT) match {
      case "json" | "xjson" => {
        Some(Json)
      }
      case "record" | "semistructureddata" | "semi-structured-data" | "xsemistructureddata" => {
        Some(SemiStructuredData)
      }
      case _ => {
        None
      }
    }
  }
}
