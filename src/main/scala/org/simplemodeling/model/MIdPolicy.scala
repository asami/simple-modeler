package org.simplemodeling.model

/*
 * Migrated from SIdPolicy
 *
 * @since   May. 31, 2009
 *  version Nov. 12, 2010
 *  version Dec. 15, 2011
 * @version Jan.  4, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait MIdPolicy {
}
case object AutoIdPolicy extends MIdPolicy
case object ApplicationIdPolicy extends MIdPolicy
