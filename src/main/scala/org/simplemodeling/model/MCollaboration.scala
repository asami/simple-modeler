package org.simplemodeling.model

/*
 * @since   Nov.  4, 2019
 * @version Nov.  4, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait MCollaboration {
}

case class CombiCollaboration() extends MCollaboration {
}
