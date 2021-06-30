package org.simplemodeling.model

import org.goldenport.values.Designation
import org.smartdox.Dox
import org.simplemodeling.model._

/*
 * Derived from SInvocationStep and SMInvocationStep.
 * 
 * @since   Dec.  5, 2008
 *  version Nov. 12, 2012
 *  version Jul. 27, 2020
 *  version Aug.  8, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MInvocationStep(
  override val designation: Designation,
  affiliation: MPackageRef
) extends MStep {
  def getAffiliation = Some(affiliation)
  def inputs: List[MEntityRef] = Nil // TODO
  def outputs: List[MEntityRef] = Nil // TODO
  def entities: List[MEntityRef] = (inputs ::: outputs).distinct

  final def isBidirectional: Boolean = {
    //    dslInvocationStep.responseDocument != NullDocument
    ???
  }

  final def isRequestVoucher: Boolean = {
    // dslInvocationStep.requestDocument != NullDocument
    ???
  }

  final def isResponseVoucher: Boolean = {
    // dslInvocationStep.responseDocument != NullDocument
    ???
  }

  final def getRequestVoucherTerm: Dox = {
    // def to_term(anOperation: SOperation) = {
    //   val doc = anOperation.in.get // XXX 
    //   new SIAnchor(doc.term) unresolvedRef_is new SElementRef(doc.packageName, doc.name)
    // }

    // if (isOperation) {
    //   to_term(dslInvocationStep.operation)
    // } else if (isService) {
    //   to_term(dslInvocationStep.service.mainOperation)
    // } else {
    //   "Unkonw"
    // }
    ???
  }

  final def getResponseVoucherTerm: Dox = {
    // def to_term(anOperation: SOperation) = {
    //   val doc = anOperation.out.get // XXX
    //   new SIAnchor(doc.term) unresolvedRef_is new SElementRef(doc.packageName, doc.name)
    // }

    // if (isOperation) {
    //   to_term(dslInvocationStep.operation)
    // } else if (isService) {
    //   to_term(dslInvocationStep.service.mainOperation)
    // } else {
    //   "Unkonw"
    // }
    ???
  }

  final def getVerbTerm: Dox = {
    // if (isOperation) {
    //   val oper = dslInvocationStep.operation
    //   new SIAnchor(operationTerm) unresolvedRef_is new SElementRef(oper.name, oper.name) // XXX
    // } else if (isService) {
    //   val service = dslInvocationStep.service
    //   new SIAnchor(serviceTerm) unresolvedRef_is new SElementRef(service.name, service.name) // XXX
    // } else if (informalOperationName != "") {
    //   informalOperationName
    // } else {
    //   "Unkonw"
    // }
    ???
  }
}
