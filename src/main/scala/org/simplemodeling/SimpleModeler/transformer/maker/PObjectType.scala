package org.simplemodeling.SimpleModeler.transformer.maker

import scala.collection.mutable
import scala.collection.mutable.{Buffer, ArrayBuffer}
import java.io._
//import org.goldenport.util.Options
//import org.goldenport.entity._
//import org.goldenport.entity.datasource.GDataSource
//import org.goldenport.entity.datasource.GContentDataSource
//import org.simplemodeling.SimpleModeler.entity.{SMConstraint, SMAttributeType, SMAssociation}
import org.goldenport.RAISE
import org.goldenport.record.v2._
import org.simplemodeling.model._

/*
 * @since   Apr. 22, 2011
 *  version Jul. 25, 2011
 *  version Apr. 11, 2012
 *  version Oct. 30, 2012
 *  version Nov. 24, 2012
 *  version Dec. 26, 2012
 *  version Jan. 29, 2013
 *  version Jan.  1, 2020
 *  version Jan.  5, 2020
 *  version Mar.  1, 2020
 *  version Apr. 27, 2020
 *  version May. 24, 2020
 * @version Jun.  1, 2020
 * @author  ASAMI, Tomoharu
 */
trait PObjectType {
  def model: Option[MAttributeType]

  def objectTypeName: String
  def getDatatypeName: Option[String] = None
  def xmlDatatypeName: String = "string"
  def sqlDatatypeName: String = "VARCHAR(32)"
  /**
    * For JPA.
    */
  def jpaObjectTypeName: String = objectTypeName
  /**
    * For JPA.
    */
  def getJpaDatatypeName: Option[String] = getDatatypeName
  /**
    * For Google AppEngine.
    */
  def jdoObjectTypeName: String = objectTypeName
  /**
    * For Google AppEngine.
    */
  def getJdoDatatypeName: Option[String] = getDatatypeName
  //
//  val constraints = mutable.Map[String, PConstraint]()
  def isEntity = false
  def isDataType = getDatatypeName.isDefined
  /**
    * Get dsl datatype name like 'XString', 'XDate'.
    */
  def dslDataTypeName: Option[String] = Some("X" + objectTypeName)

  // if (model_attribute_type != null) {
  //   for (constraint <- model_attribute_type.constraints) {
  //     addConstraint(new PConstraint(constraint.name, constraint.value))
  //   }
  // }

  // def this() = this(null)

  // def addConstraints(constraints: Seq[PConstraint]) {
  //   constraints.foreach(addConstraint)
  // }

  // def addConstraint(constraint: PConstraint) {
  //   this.constraints += constraint.name -> constraint
  // }

  def getEntity: Option[PEntity] = None

  def apply[T](f: Function1[PObjectType, T]): T = {
    f(this)
  }

  override def toString() = {
    "PObjectType(" + objectTypeName + ")"
  }
}

trait PDataType extends PObjectType {
  def pConstraints: List[PConstraint]
  lazy val constraints: List[PConstraint] =
    model.map(_.constraints.map(PConstraint.apply).toList).getOrElse(Nil) ++ pConstraints

  def addConstraint(p: PConstraint, ps: PConstraint*): PDataType = addConstraints(p +: ps)
  def addConstraints(ps: Seq[PConstraint]): PDataType

  // final def getLongMax(): Option[Long] = {
  //   constraints.get("max") match {
  //     case Some(c) => c.getLongValue match {
  //       case Some(v) => base_long_max_option match {
  //         case Some(b) => Some(Math.min(v, b))
  //         case None => Some(v)
  //       }
  //       case None => base_long_max_option
  //     }
  //     case None => base_long_max_option
  //   }
  // }

  protected def base_long_max_option: Option[Long] = None

  // final def getLongMin(): Option[Long] = {
  //   constraints.get("min") match {
  //     case Some(c) => c.getLongValue match {
  //       case Some(v) => base_long_min_option match {
  //         case Some(b) => Some(Math.max(v, b))
  //         case None => Some(v)
  //       }
  //       case None => base_long_min_option
  //     }
  //     case None => base_long_min_option
  //   }
  // }

  protected def base_long_min_option: Option[Long] = None
}

case class PStringType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName: String = "VARCHAR(64)"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PTokenType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName: String = "VARCHAR(32)"
  override def dslDataTypeName = Some("XToken")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PByteStringType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "ShortBlob" // XXX
  override def xmlDatatypeName = "base64Binary"
  override def sqlDatatypeName: String = "BLOB" // XXX
  override def jdoObjectTypeName = "ShortBlob" // com.google.appengine.api.datastore.ShortBlob
  override def dslDataTypeName = Some("XBase64Binary")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PBooleanType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Boolean"
  override def getDatatypeName = Some("boolean")
  override def xmlDatatypeName = "boolean"
  override def sqlDatatypeName: String = "BOOLEAN"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PByteType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Byte"
  override def getDatatypeName = Some("byte")
  override def xmlDatatypeName = "byte"
  override def sqlDatatypeName: String = "TINYINT"
  override def jdoObjectTypeName = "Short"

  override def base_long_max_option = Some(java.lang.Byte.MAX_VALUE)
  override def base_long_min_option = Some(java.lang.Byte.MIN_VALUE)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PUnsignedByteType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Short"
  override def getDatatypeName = Some("short")
  override def sqlDatatypeName = "TINYINT"
  override def xmlDatatypeName = "unsignedByte"

  override def base_long_max_option = Some(java.lang.Byte.MAX_VALUE * 2)
  override def base_long_min_option = Some(0)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PShortType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Short"
  override def getDatatypeName = Some("short")
  override def sqlDatatypeName = "SMALLINT"
  override def xmlDatatypeName = "short"

  override def base_long_max_option = Some(java.lang.Short.MAX_VALUE)
  override def base_long_min_option = Some(java.lang.Short.MIN_VALUE)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PUnsignedShortType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Integer"
  override def getDatatypeName = Some("int")
  override def xmlDatatypeName = "unsignedShort"
  override def sqlDatatypeName = "SMALLINT"

  override def base_long_max_option = Some(java.lang.Short.MAX_VALUE * 2)
  override def base_long_min_option = Some(0)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PIntType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Integer"
  override def getDatatypeName = Some("int")
  override def xmlDatatypeName = "int"
  override def sqlDatatypeName = "INT"
  override def dslDataTypeName = Some("XInt")

  override def base_long_max_option = Some(java.lang.Integer.MAX_VALUE)
  override def base_long_min_option = Some(java.lang.Integer.MIN_VALUE)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PUnsignedIntType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Long"
  override def getDatatypeName = Some("long")
  override def xmlDatatypeName = "unsignedInt"
  override def sqlDatatypeName = "INT"

  override def base_long_max_option = Some(java.lang.Integer.MAX_VALUE * 2)
  override def base_long_min_option = Some(0)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PLongType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Long"
  override def getDatatypeName = Some("long")
  override def xmlDatatypeName = "long"
  override def sqlDatatypeName = "BIGINT"

  override def base_long_max_option = Some(java.lang.Long.MAX_VALUE)
  override def base_long_min_option = Some(java.lang.Long.MIN_VALUE)

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PUnsignedLongType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "unsignedLong"
  override def sqlDatatypeName = "BIGINT"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PFloatType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Float"
  override def getDatatypeName = Some("float")
  override def xmlDatatypeName = "float"
  override def sqlDatatypeName = "REAL"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PDoubleType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Double"
  override def getDatatypeName = Some("double")
  override def xmlDatatypeName = "double"
  override def sqlDatatypeName = "DOUBLE"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PIntegerType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "integer"
  override def sqlDatatypeName = "INTEGER"
  override def jdoObjectTypeName = "String"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PPositiveIntegerType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "positiveInteger"
  override def sqlDatatypeName = "INTEGER"
  override def jdoObjectTypeName = "String"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PNonPositiveIntegerType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "nonPositiveInteger"
  override def sqlDatatypeName = "INTEGER"
  override def jdoObjectTypeName = "String"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PNegativeIntegerType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "negativeInteger"
  override def sqlDatatypeName = "INTEGER"
  override def jdoObjectTypeName = "String"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PNonNegativeIntegerType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigInteger"
  override def xmlDatatypeName = "nonNegativeInteger"
  override def sqlDatatypeName = "INTEGER"
  override def jdoObjectTypeName = "String"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PDecimalType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigDecimal"
  override def xmlDatatypeName = "decimal"
  override def jdoObjectTypeName = "String"
  override def sqlDatatypeName = "DECIMAL"
  override def dslDataTypeName = Some("XDecimal")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

//
case class PDurationType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Duration" // javax.xml.datatype.Duration
  override def xmlDatatypeName = "duration"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PDateTimeType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Date" // java.util.Date
  override def xmlDatatypeName = "dateTime"
  override def sqlDatatypeName = "DATETIME"
  override def dslDataTypeName = Some("XDateTime")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PDateType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Date"
  override def xmlDatatypeName = "date"
  override def sqlDatatypeName = "DATE"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PTimeType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Date"
  override def xmlDatatypeName = "time"
  override def sqlDatatypeName = "TIME"
  override def dslDataTypeName = Some("XTime")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGYearMonthType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "XMLGregorianCalendar" // javax.xml.datatype.XMLGregorianCalendar
  override def xmlDatatypeName = "gYearMonth"
  override def sqlDatatypeName = "TINYINT"
  override def dslDataTypeName = Some("XGYearMonth")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGMonthDayType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "XMLGregorianCalendar" // javax.xml.datatype.XMLGregorianCalendar
  override def xmlDatatypeName = "gMonthDay"
  override def sqlDatatypeName = "TINYINT"
  override def dslDataTypeName = Some("XGMonthDay")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGYearType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "XMLGregorianCalendar" // javax.xml.datatype.XMLGregorianCalendar
  override def xmlDatatypeName = "gYear"
  override def sqlDatatypeName = "TINYINT"
  override def dslDataTypeName = Some("XGYear")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGMonthType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "XMLGregorianCalendar" // javax.xml.datatype.XMLGregorianCalendar
  override def xmlDatatypeName = "gMonth"
  override def sqlDatatypeName = "TINYINT"
  override def dslDataTypeName = Some("XGMonth")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGDayType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "XMLGregorianCalendar" // javax.xml.datatype.XMLGregorianCalendar
  override def xmlDatatypeName = "gDay"
  override def sqlDatatypeName = "TINYINT"
  override def dslDataTypeName = Some("XGDay")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PAnyURIType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "URI"
  override def sqlDatatypeName = "VARCHAR(128)"
  override def dslDataTypeName = Some("XAnyURI")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PLanguageType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Locale"
  override def sqlDatatypeName = "VARCHAR(32)"
  override def dslDataTypeName = Some("XLanguage")

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PImageLinkType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Link" // com.google.appengine.api.datastore.Link

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

/*
 * XML Datatype
 */


/*
 * HTML
 */
case class PColorType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XColor")

  override def toString() = "datatype:color"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PFileType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XFile")

  override def toString() = "datatype:file"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PPasswordType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XPassword")

  override def toString() = "datatype:password"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PRangeType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XRange")

  override def toString() = "datatype:range"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PSearchType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XSearch")

  override def toString() = "datatype:search"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PTelType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XTel")

  override def toString() = "datatype:tel"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PWeekType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XWeek")

  override def toString() = "datatype:week"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

/*
 * AppEngine
 */
case class PUserType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "User" // com.google.appengine.api.users.User

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PBlobType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Blob" // com.google.appengine.api.datastore.Blob
  override def xmlDatatypeName = "base64Binary" // hexBinary ??

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PTextType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def jdoObjectTypeName = "Text" // com.google.appengine.api.datastore.Text
  override def dslDataTypeName = Some("XText")


  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PCategoryType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Category" // com.google.appengine.api.datastore.Category

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PLinkType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Link" // com.google.appengine.api.datastore.Link
  override def sqlDatatypeName = "VARCHAR(128)"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PEmailType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Email" // com.google.appengine.api.datastore.Email
  override def sqlDatatypeName = "VARCHAR(64)"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PGeoPtType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "GeoPt" // com.google.appengine.api.datastore.GeoPt

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PIMType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "IM" // com.google.appengine.api.datastore.IM

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PPhoneNumberType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "PhoneNumber" // com.google.appengine.api.datastore.PhoneNumber

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PPostalAddressType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "PostalAddress" // com.google.appengine.api.datastore.PostalAddress

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PRatingType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Rating" // com.google.appengine.api.datastore.Rating

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

// XXX Link
case class PUrlType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Link" // com.google.appengine.api.datastore.Link

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

/*
 * Business datatype
 */
case class PMoneyType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "BigDecimal"
  override def xmlDatatypeName = "decimal"
  override def dslDataTypeName = Some("XMoney")
  override def toString() = "datatype:money"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PPercentType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "Float"
  override def getDatatypeName = Some("flaot")
  override def dslDataTypeName = Some("XPercent")
  override def toString() = "datatype:percent"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PUnitType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = Some("XUnit")
  override def toString() = "datatype:unit"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

/*
 * Platform datatype
 */
case class PUuidType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName = "CHAR(36)"
  override def dslDataTypeName = Some("XUuid")
  override def toString() = "datatype:uuid"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PObjectIdType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName = "CHAR(128)"
  override def dslDataTypeName = Some("XEverforthid")
  override def toString() = "datatype:id"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PXmlType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName = "TEXT"
  override def dslDataTypeName = Some("XXml")
  override def toString() = "datatype:xml"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

case class PHtmlType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PDataType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def sqlDatatypeName = "TEXT"
  override def dslDataTypeName = Some("XHtml")
  override def toString() = "datatype:html"

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

/*
 * ReferenceProperty
 */
case class PObjectReferenceType(
  name: String,
  packageName: String
) extends PObjectType {
  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None

  /**
    * Nullable in case of reference to an external object.
    * Use Case: Base class is an external object. (e.g. android.content.ContentProvider)
    */
  private var _reference: PObject = _
  def reference: PObject = {
    require(_reference != null, "_reference must be initialized. (%s)".format(name))
    _reference
  }
  def reference_=(ref: PObject) = _reference = ref

  def referenceOption: Option[PObject] = Option(_reference)

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}
object PObjectReferenceType {
  def apply(p: MObjectRef): PObjectReferenceType = PObjectReferenceType(
    p.objectName,
    p.packageRef.packageName
  )
}

// Value
case class PValueType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(aValue: PValue) = this(aValue.name, aValue.packageName)

  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None

  private var _value: PValue = _
  def value: PValue = {
    require(_value != null, "PValueType " + name + " should be resolved a reference to the value entity.")
    _value
  }
  def value_=(aValue: PValue) = _value = aValue

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}

// Voucher (Document)
case class PVoucherType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(aVoucher: PVoucher) = this(aVoucher.name, aVoucher.packageName)

  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None

  var voucher: PVoucher = _
  /*
   private var _voucher: PVoucherEntity = _
   def voucher: PVoucherEntity = {
   require(_voucher != null, "PVoucherType " + name + " should be resolved a reference to the value entity.")
   _voucher
   }
   def voucher_=(aVoucher: PVoucherEntity) = _voucher = aVoucher
   */

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}

// Powertype
case class PPowertypeType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(aPowertype: PPowertype) = this(aPowertype.name, aPowertype.packageName)

  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None

  private var _powertype: PPowertype = _
  def powertype: PPowertype = {
    require(_powertype != null, "powertype " + name + " in " + packageName + " should be resolved.")
    _powertype
  }
  def powertype_=(aPowertype: PPowertype) = _powertype = aPowertype

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}

// StateMachine
case class PStateMachineType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(aStateMachine: PStateMachine) = this(aStateMachine.name, aStateMachine.packageName)

  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None
  override def sqlDatatypeName = "INT" // XXX

  private var _statemachine: PStateMachine = _
  def statemachine: PStateMachine = {
    require(_statemachine != null)
    _statemachine
  }
  def statemachine_=(aStateMachine: PStateMachine) = _statemachine = aStateMachine

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}

// Entity
case class PEntityType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(anEntity: PEntity) = this(anEntity.name, anEntity.packageName)

  val model = None

  override def objectTypeName = name
  override def dslDataTypeName = None

  private var _entity: PEntity = _
  def entity: PEntity = {
    require (_entity != null, "PEntityType entity is unset: " + name)
    _entity
  }
  def entity_=(anEntity: PEntity) = {
    require (anEntity != null, "EntityType entity must be not null: " + name)
    require (_entity == null, "EntityType entity already has been setted up: " + name)
    _entity = anEntity
  }

  override def getEntity = Some(entity)

  /*
   var modelAssociation: SMAssociation = _

   def model_association_is(anAssoc: SMAssociation): PEntityType = {
   modelAssociation = anAssoc
   this
   }

   def isAggregation = {
   println("GaeEntityType assoc = " + modelAssociation)
   if (modelAssociation != null) modelAssociation.isAggregation
   else false
   }

   def isComposition =
   if (modelAssociation != null) modelAssociation.isComposition
   else false

   def isQueryReference =
   if (modelAssociation != null) modelAssociation.isQueryReference
   else false
   */

  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
  override def isEntity = true

  def isLogicalOperation: Boolean = entity.isLogicalOperation
}

@deprecated("use PEntityType instead.", "0.5")
case class PEntityPartType(
  name: String,
  packageName: String
) extends PObjectType {
  def this(aPart: PEntityPart) = this(aPart.name, aPart.packageName)
  val model = None

  override def objectTypeName = name
  override def jdoObjectTypeName = "String"
  override def dslDataTypeName = None

  private var _part: PEntityPart = _
  def part: PEntityPart = {
    require(_part != null)
    _part
  }
  def part_=(aPart: PEntityPart) = _part = aPart

  // XXX unify
  def qualifiedName = {
    require (name != null && packageName != null)
    if (packageName == "") name
    else packageName + "." + name
  }
}

//
case class PGenericType(
  aModelAttrType: MAttributeType,
  pConstraints: List[PConstraint] = Nil
) extends PObjectType {
  val model = Some(aModelAttrType)

  override def objectTypeName = "String"
  override def dslDataTypeName = None

  def addConstraints(ps: Seq[PConstraint]) = copy(pConstraints = pConstraints ++ ps)
}

object PObjectType {
  def apply(p: MAttributeType): PObjectType = {
    val a: PObjectType = p match {
      case m: MDatatype => m.datatype match {
        case XBoolean => new PBooleanType(m)
        case XByte => new PByteType(m)
        case XShort => new PShortType(m)
        case XInt => new PIntType(m)
        case XLong => new PLongType(m)
        case XFloat => new PFloatType(m)
        case XFloat1 => new PFloatType(m)
        case XDouble => new PDoubleType(m)
        case XInteger => new PIntegerType(m)
        case XDecimal => new PDecimalType(m)
        case XString => new PStringType(m)
        case XNonEmptyString => new PStringType(m).addConstraint(???)
        case XNonBlankString => new PStringType(m).addConstraint(???)
        case XToken => new PTokenType(m)
        case XNonEmptyToken => new PStringType(m).addConstraint(???)
        case XNonBlankToken => new PStringType(m).addConstraint(???)
        case XDate => new PDateType(m)
        case XTime => new PTimeType(m)
        case XDateTime => new PDateTimeType(m)
        case XText => new PTextType(m)
        case XNonEmptyText => new PTextType(m).addConstraint(???)
        case XNonBlankText => new PTextType(m).addConstraint(???)
        case XBase64 => new PBlobType(p) // XXX
        case XBinary => new PByteStringType(p) // XXX
        case XLink => ???
        case XImageLink => new PImageLinkType(m)
        case XEMail => new PEmailType(m)
        case XMoney => new PMoneyType(m)
        case XPercent => new PPercentType(m)
        case XUnit => new PUnitType(m)
        case XColor => new PColorType(m)
        case XFile => new PFileType(m)
        case XMonth => new PGMonthType(m)
        case XMonthDay => new PGMonthDayType(m)
        case XYear => new PGYearType(m)
//        case XYearEffective => PYearEffectiveType // new TODO
//        case XYearPast => PYearPastType // TODO
        case XYearMonth => new PGYearMonthType(m)
        case XDay => new PGDayType(m)
        case XDuration => new PDurationType(m)
        case XPassword => new PPasswordType(m)
        case mm: XRange => new PRangeType(m) // TODO
        case XSearch => new PSearchType(m)
        case XTel => new PTelType(m)
        case XWeek => new PWeekType(m)
        case XUuid => new PUuidType(m)
        case XEverforthid => new PObjectIdType(m)
        case XXml => new PXmlType(m)
        case XHtml => new PHtmlType(m)
//        case m: XEntityReference => new PObjectReferenceType()
//        case m: XValue => XXX
//        case XRecordInstance => XXX
//        case XEverforthObjectReference => XXX
        case m: XPowertype => new PPowertypeType(???, ???)
        case m: XPowertypeReference => RAISE.notImplementedYetDefect
        case m: XStateMachine => new PStateMachineType(???, ???)
        case m: XStateMachineReference => RAISE.notImplementedYetDefect
        case m: XExternalDataType => ???
        case m: XValue => ???
        case XRecordInstance => ???
        case XYearEffective => ???
        case XYearPast => ???
        case XEntityId => ???
        case m: XEntityReference => ???
        case m: XEverforthObjectReference => ???
      }
    }
    a
  }
}

/*


        case XAnyURI => PLinkType
        // AppEngine
        case ext.XText => PTextType
        case ext.XCategory => PCategoryType
        case ext.XUser => PUserType
        case ext.XGeoPt => PGeoPtType
        case ext.XIM => PIMType
        case ext.XPhoneNumber => PPhoneNumberType
        case ext.XPostalAddress => PPostalAddressType
        case ext.XRating => PRatingType
        //
        case business.XMoney => PMoneyType
        case business.XPercent => PPercentType
        case business.XUnit => PUnitType
        case _ => anAttr.attributeType.dslAttributeType match {
          case v: SValue => new PValueType(v.name, v.packageName);
          case d: SDocument => new PDocumentType(d.name, d.packageName)
          case x => {
            record_warning("%sは未定義のデータ型です。", x.name)
            new PGenericType(attributeType)
          }
        }
 */
