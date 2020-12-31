package org.simplemodeling.model.business

import org.smartdox.Dox
import org.simplemodeling.model._

/*
 * Derived from BusinessUsecase and SMBusinessUsecase.
 * 
 * @since   Nov.  8, 2008
 *  version Dec.  8, 2008
 *  version Nov.  4, 2011
 *  version Nov.  5, 2012
 *  version May.  9, 2020
 * @version Jul. 24, 2020
 * @author  ASAMI, Tomoharu
 */
trait MBusinessUsecase extends MUsecase {
  final def includeTasksLiteral: Dox = {
    // objects_literal(includeBusinessTasks)
    ???
  }

  final def includeUsecasesLiteral: Dox = {
    // objects_literal(includeBusinessUsecases)
    ???
  }

  final def userUsecasesLiteral: Dox = {
    // objects_literal(userBusinessUsecases)
    ???
  }
}
