package org.simplemodeling.SimpleModeler.transformer.maker

import org.goldenport.recorder.{ForwardRecorder, Recorder}
import org.smartdox.generator.{Context => DoxContext}
import org.simplemodeling.SimpleModeler.Context
import org.simplemodeling.SimpleModeler.transformer.maker.sql._

/*
 * @since   Dec. 22, 2019
 *  version Jan.  5, 2020
 *  version Mar.  8, 2020
 *  version Apr. 30, 2020
 *  version May.  9, 2020
 * @version Jul. 26, 2020
 * @author  ASAMI, Tomoharu
 */
class PContext(
  val context: Context
) extends ForwardRecorder {
  protected def forward_Recorder: Recorder = context.recorder

  def recorder: Recorder = context.recorder
  def doxContext: DoxContext = ???

  def labelName(p: PObject): String = ???
  def asciiName(p: PObject): String = ???
  def asciiName(p: PAttribute): String = ???
  def attributeName(p: PAttribute): String = p.name
  def variableName(p: PAttribute): String = p.name
  def variableName4RefId(p: PAttribute): String = ???
  def voucherAttributeName(p: PAttribute): String = ???
  def voucherVariableName(p: PAttribute): String = ???
  def labelName(p: PAttribute): String = ???
  def dataKey(p: PAttribute): String = ???
  def constantName(p: String): String = p.toUpperCase
  def contextName(pkgname: String): String = ???
  def moduleName(pkgname: String): String = ???
  def factoryName(pkgname: String): String = ???
  def repositoryName(pkgname: String): String = ???
  def modelName(pkgname: String): String = ???
  def errorModelName(pkgname: String): String = ???
  def agentName(pkgname: String): String = ???
  def controllerName(pkgname: String): String = ???
  def entityServiceName(pkgname: String): String = ???
  def eventServiceName(pkgname: String): String = ???
  def traversePlatform[T](p: PartialFunction[PObject, T]): Unit = {
    // TODO
  }
  def collectPlatform[T](p: PartialFunction[PObject, T]): Seq[T] = ???
  def entityVoucherName(p: PObject): String = ???

  // def packageObject(model: PModel, p: PObject): PPackage = model.packageObject(p)

  /*
   * SQL
   */
  /**
   * SqlPlatform automatically generates SQL entities.
   */
  // lazy val sqlRealm = SqlPlatform.create(this)

  // final def getSqlEntity(entity: PEntityEntity): SqlEntityEntity = {
  //   sqlRealm.getEntityEntity(entity)
  // }

  // final def getSqlEntity(doc: PVoucherEntity): SqlVoucherEntity = {
  //   sqlRealm.getVoucherEntity(doc) getOrElse {
  //     sys.error("not implemented yet")
  //   }
  // }

  def sqlMaker(model: PModel, entity: PEntity): SqlMaker = {
    new EntitySqlMaker(model, entity)(this)
  }

  def sqlMaker(doc: PVoucher): SqlMaker = {
    new VoucherSqlMaker(doc)(this)
  }

  def sqlMaker(model: PModel, entity: PEntity, isTarget: PAttribute => Boolean): SqlMaker = {
    new EntitySqlMaker(model, entity, isTarget)(this)
  }

  def sqlMaker(doc: PVoucher, isTarget: PAttribute => Boolean): SqlMaker = {
    new VoucherSqlMaker(doc, isTarget)(this)
  }

  def sqlTableName(o: PObject): String = {
    pickup_name(o.sqlTableName, o.term_en, o.term_ja, o.term, o.name_en, o.name_ja, o.name)
  }

  /**
   * for Single Table Inheritance.
   */
  def sqlTableName4SingleTableInheritanceOption(model: PModel, o: PObject): Option[String] = {
    baseClassesAsStream(model, o).find(_.hasInheritancePowertypeExcludingBaseClasses).map(sqlTableName)
  }

  def sqlTableName4SingleTableInheritance(model: PModel, o: PObject): String = {
    sqlTableName4SingleTableInheritanceOption(model, o) getOrElse sqlTableName(o)
  }

  // utility
  def baseClassesAsStream(model: PModel, o: PObject): Stream[PObject] = {
    Stream.cons(o, o.getBaseObject(model).map(baseClassesAsStream(model, _)) getOrElse Stream.empty)
  }

  def sqlIdColumnName(o: PObject): String = {
    sqlColumnName(o.idAttr)
  }

  def sqlColumnName(o: PAttribute): String = {
    pickup_name(o.sqlColumnName, o.term_en, o.term_ja, o.term, o.name_en, o.name_ja, o.name)
  }

  def sqlTableName(a: PAttribute): String = {
    val o = a.attributeType match {
      case e: PEntityType => e.entity
      case p: PPowertypeType => p.powertype
      case s: PStateMachineType => s.statemachine
    }
    sqlTableName(o)
  }

  def sqlJoinColumnName(a: PAttribute): String = {
    val o = a.attributeType match {
      case e: PEntityType => e.entity
      case p: PPowertypeType => p.powertype
      case s: PStateMachineType => s.statemachine
    }
    sqlJoinColumnName(o)
  }

  /*
   * If target object is Trait, id name is unknown.
   * In the case using "id" as a column name.
   */
  def sqlJoinColumnName(o: PObject): String = {
    o.idAttrOption match {
      case Some(attr) => sqlColumnName(attr)
      case None => "id"
    }
  }

  def sqlJoinBackReferenceColumnName(base: PObject, backref: PObject): String = {
    backref.wholeAttributesWithoutId.find(x => {
      x.getEntity.map(base.isAncestor) getOrElse false
    }).map(sqlColumnName).get
  }

  def sqlNameColumnName(a: PAttribute): String = {
    val o = a.attributeType match {
      case e: PEntityType => e.entity
      case p: PPowertypeType => p.powertype
      case s: PStateMachineType => s.statemachine
    }
    sqlNameColumnName(o)
  }

  def sqlNameColumnName(o: PObject): String = {
    val r = sqlColumnName(o.nameAttr)
//    println("PEntityContext#sqlNameColumnName(%s) = %s / %s".format(a.name, o.name, r))
//    println(o.nameAttr)
//    o.wholeAttributesWithoutId.foreach(println)
//    println("===")
    r
  }

  def sqlNameAlias(a: PAttribute): String = {
    asciiName(a) + "__name"
  }

  /**
   * SqlMaker uses to get a attribute of counter association.
   */
  def sqlAssociationClassCounterAssociation(a: AttributeParticipation): Option[PAttribute] = {
    sqlAssociationClassCounterAssociation(a.source, a.attribute)
  }

  def sqlAssociationClassCounterAssociation(s: PObject, a: PAttribute): Option[PAttribute] = {
    val attrs = s.attributes
    if (attrs.length != 2) None
    else attrs.filter(_ != a).headOption
  }

  // /**
  //  * SqlMaker uses to get a attribute of counter association.
  //  */
  // def sqlAssociationClassCounterAssociation(a: AttributeParticipation): Option[PAttribute] = {
  //   sqlAssociationClassCounterAssociation(a.source, a.attribute)
  // }

  // def sqlAssociationClassCounterAssociation(s: PObject, a: PAttribute): Option[PAttribute] = {
  //   val attrs = s.attributes
  //   if (attrs.length != 2) None
  //   else attrs.filter(_ != a).headOption
  // }

  private def pickup_name(names: String*): String = ???
}
