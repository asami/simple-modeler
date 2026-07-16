package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.SimpleModeler.generator.scala.Scala3ClassGeneratorBase.ClassKind
import org.simplemodeling.SimpleModeler.generator.scala.model._

/*
 * @since   Jul. 16, 2026
 * @version Jul. 16, 2026
 * @author  ASAMI, Tomoharu
 */
final class SchemaParameterProjectionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Schema parameter projection" should {
    "preserve scalar, optional, repeated, and non-empty multiplicities" in {
      Given("generated parameters for every supported multiplicity family")
      val projection = new SchemaProjection()
      val scalar = Parameter.create("scalar", TypeName.record)
      val optional = scalar.toOptionType
      val repeated = scalar.toVectorType
      val nonempty = Parameter.create(
        "nonempty",
        TypeName.Container(TypeName.Plain(PackageName("cats.data"), "NonEmptyVector"), TypeName.record)
      )

      When("their runtime schema multiplicities are projected")
      val actual = Vector(scalar, optional, repeated, nonempty).map(projection.multiplicity)

      Then("each domain multiplicity remains distinct")
      actual shouldBe Vector(
        "org.goldenport.schema.Multiplicity.One",
        "org.goldenport.schema.Multiplicity.ZeroOne",
        "org.goldenport.schema.Multiplicity.ZeroMore",
        "org.goldenport.schema.Multiplicity.OneMore"
      )
    }
  }

  private final class SchemaProjection
      extends Scala3ClassGeneratorExecutor[SComponent](ScalaModel.Context.default, ClassKind.Component, _component) {
    def multiplicity(parameter: Parameter): String =
      schema_parameter_multiplicity_expr(parameter)
  }

  private val _component = SComponent(
    core = ClassCore(
      packageName = PackageName("org.example"),
      declaration = ClassDeclaration.Control,
      className = ClassName("ExampleComponent")
    ),
    componentCore = SComponent.ComponentCore("example", Nil)
  )
}
