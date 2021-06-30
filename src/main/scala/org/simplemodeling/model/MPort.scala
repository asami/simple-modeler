package org.simplemodeling.model

import org.goldenport.value._
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._

/*
 * Derived from SPort and SMPort.
 * 
 * @since   Mar. 21, 2011
 *  version May. 10, 2020
 *  version Jun. 17, 2020
 *  version Aug.  1, 2020
 * @version Jun. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class MPort(
  override val designation: Designation,
  affiliation: MPackageRef,
  inout: MPort.InOut = MPort.InputOutput,
  description: Description = Description.empty
) extends MElement {
  def getAffiliation = None
  def entityType: MEntityRef = ???
}

object MPort {
  sealed trait InOut extends NamedValueInstance {
  }
  object InOut extends EnumerationClass[InOut] {
    val elements = Vector(InputOutput, Input, Output)
  }

  case object InputOutput extends InOut {
    val name = "input-output"
  }
  case object Input extends InOut {
    val name = "input"
  }
  case object Output extends InOut {
    val name = "output"
  }
}
