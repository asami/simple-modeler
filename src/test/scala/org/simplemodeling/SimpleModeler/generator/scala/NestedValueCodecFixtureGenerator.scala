package org.simplemodeling.SimpleModeler.generator.scala

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.goldenport.values.Designation
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.MDomainValue
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ValueFamilyGenerator

object NestedValueCodecFixtureGenerator {
  private val _package = "org.simplemodeling.generatedcodec.value"

  private def _attribute(name: String, datatype: MAttributeType, multiplicity: MMultiplicity): MAttribute =
    MAttribute(Designation(name), datatype, multiplicity, Nil, None)

  private def _value(name: String, attributes: List[MAttribute]): MDomainValue =
    MDomainValue(Description.name(name), MPackageRef(_package), Nil, None, Nil, Nil, attributes, Nil)

  private def _reference(name: String): MAttributeType =
    MObjectAttributeType(MObjectRef.create(_package + "." + name))

  def values: Vector[MDomainValue] = Vector(
    _value("Leaf", List(_attribute("text", MDataType.string, MOne))),
    _value("Branch", List(_attribute("leaf", _reference("Leaf"), MOne))),
    _value("Envelope", List(
      _attribute("optional", _reference("Branch"), MZeroOne),
      _attribute("ordered", _reference("Branch"), MZeroMore),
      _attribute("nonempty", _reference("Leaf"), MOneMore))))

  def main(args: Array[String]): Unit = {
    val output = Paths.get(args(0))
    val products = values
    val context = ScalaModel.Context.fromObjects(products)
    val generator = new Scala3ValueFamilyGenerator(context)
    products.foreach { value =>
      generator.generate(value).take.slots.foreach { slot =>
        val path = output.resolve(slot.path.stripPrefix("/"))
        Files.createDirectories(path.getParent)
        Files.write(path, slot.content.getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
