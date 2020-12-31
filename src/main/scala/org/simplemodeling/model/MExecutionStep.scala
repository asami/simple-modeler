package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Dox
import org.simplemodeling.model._

/*
 * Derived from ExecutionStep and SMExecutionStep.
 * 
 * @since   Dec.  5, 2008
 * @version Jul. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class MExecutionStep(
  designation: Designation,
  affiliation: MPackageRef
) extends MStep {
  def getAffiliation = Some(affiliation)
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct

  final def getTargetEntityTerm: Dox = {
    // new SMTermRef(dslExecutionStep.entity)
    ???
  }

  final def getVerbTerm: Dox = {
    // dslExecutionStep.kind match {
    //   case kind: Issue => new SMKeywordRef("発行", "help:issue")
    //   case kind: Open => new SMKeywordRef("オープン", "help:open")
    //   case kind: Close => new SMKeywordRef("クローズ", "help:close")
    //   case kind: Create => new SMKeywordRef("作成", "help:create")
    //   case kind: Read => new SMKeywordRef("参照", "help:read")
    //   case kind: Update => new SMKeywordRef("更新", "help:update")
    //   case kind: Delete => new SMKeywordRef("削除", "help:delete")
    // }
    ???
  }
}
