package org.simplemodeling.model.requirement

import org.smartdox._
import org.simplemodeling.model._

/*
 * Derived from RequirementUsecase and SMRequirementUsecase.
 * 
 * @since   Dec. 10, 2008
 *  version Sep. 18, 2011
 *  version Nov.  4, 2011
 *  version Nov.  5, 2012
 *  version May.  9, 2020
 * @version Jul. 25, 2020
 * @author  ASAMI, Tomoharu
 */
trait MRequirementUsecase extends MUsecase {
  final def includeTasksLiteral: Dox = {
    // objects_literal(includeRequirementTasks)
    ???
  }

  final def includeUsecasesLiteral: Dox = {
    // objects_literal(includeRequirementUsecases)
    ???
  }

  final def userUsecasesLiteral: Dox = {
    // objects_literal(userRequirementUsecases)
    ???
  }

  final def userBusinessTasksLiteral: Dox = {
    // objects_literal(userBusinessTasks)
    ???
  }
}
