package org.simplemodeling.SimpleModeler.transformer.maker.sql

import org.goldenport.realm.Realm
// import org.goldenport.sdoc.structure._
// import org.goldenport.entities.workspace.TreeWorkspaceNode
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SqlRealmEntity.
 *
 * @since   May.  5, 2012
 *  version Nov.  2, 2012
 *  version Dec. 25, 2012
 * @version Mar.  3, 2020
 * @author  ASAMI, Tomoharu
n */
class SqlRealm(
  val sqlContext: SqlContext,
  val realm: Realm
) {
//   def getEntityEntity(entity: PEntityEntity): SqlEntityEntity = {
// //    println("SqlRealmEntity: start")
// //    dump()
// //    println("SqlRealmEntity: end")
//     val me = entity.modelEntity
//     val pn = sqlContext.makePathname(me.qualifiedName)
//     getContent(pn) match {
//       case Some(c: EntityContent) => c.entity.asInstanceOf[SqlEntityEntity]
//       case _ => sys.error("not found")
//     }
//   }

//   /**
//    * Current behaior of SimpleModel2ProgramRealmTransformerBase is insuffient
//    * about generating of SqlDocumentEntity.
//    * Consequently, this method always return None.
//    * SimpleModel2ProgramRealmTransformerBase creates PDocumentEntity.
//    * This method invoked by DocumentJavaClassDefinition via PEntityContext.getSqlEntity.
//    */
//   def getDocumentEntity(entity: PDocumentEntity): Option[SqlDocumentEntity] = {
// //    println("SqlRealmEntity: start")
// //    dump()
// //    println("SqlRealmEntity: end")
//     entity.modelEntityOption.flatMap(me => {
//       println("SqlRealmEntity#getDocumentEntity: " + me)
//       val pn = sqlContext.makePathname(me.qualifiedName)
//       println("SqlRealmEntity#getDocumentEntity: " + pn)
//       getContent(pn).collect {
//         case c: EntityContent => c
//       } collect {
//         case d: SqlDocumentEntity => d
//       }
//     })
//   }
}
