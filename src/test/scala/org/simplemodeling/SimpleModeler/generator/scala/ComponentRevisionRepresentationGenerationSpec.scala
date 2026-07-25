package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3ComponentGenerator

/*
 * @since   Jul. 25, 2026
 * @version Jul. 25, 2026
 * @author  ASAMI, Tomoharu
 */
final class ComponentRevisionRepresentationGenerationSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  "Component revision representation generation" should {
    "preserve generated Entity model metadata in the CNCF runtime descriptor" in {
      Given("a generated component descriptor with embedded Entity revision metadata")
      val descriptor = SComponent.EntityRuntimeDescriptor(
        entityName = "Person",
        packageName = PackageName("example.entity"),
        revisionModelKind = Some("simple-entity"),
        revisionRepresentation = Some("embedded")
      )
      val component = SComponent(
        core = ClassCore(
          packageName = PackageName("example"),
          declaration = ClassDeclaration.Control,
          className = ClassName("ExampleComponent")
        ),
        componentCore = SComponent.ComponentCore(
          componentName = "example",
          services = Nil,
          entityRuntimeDescriptors = Vector(descriptor)
        )
      )

      When("the Scala component adapter is generated")
      val source = new Scala3ComponentGenerator(ScalaModel.Context.default)
        .generate(component)
        .take
        .slots
        .map(_.content)
        .mkString("\n")

      Then("the runtime descriptor declares the canonical embedded representation")
      source should include(
        "revisionModelKind = Some(org.goldenport.cncf.entity.EntityRevisionModelKind.SimpleEntity)"
      )
      source should include(
        "revisionRepresentation = Some(org.goldenport.cncf.entity.EntityRevisionRepresentation.Embedded)"
      )
    }

    "reject unsupported model and representation metadata during generation" in {
      Given("component descriptors containing non-canonical revision metadata")
      val invalidmodelkind = SComponent.EntityRuntimeDescriptor(
        entityName = "Person",
        packageName = PackageName("example.entity"),
        revisionModelKind = Some("simpleentity")
      )
      val invalidrepresentation = SComponent.EntityRuntimeDescriptor(
        entityName = "Person",
        packageName = PackageName("example.entity"),
        revisionModelKind = Some("simple-entity"),
        revisionRepresentation = Some("carrier")
      )

      When("the Scala component adapter is generated")
      val modelkindresult = _generate(invalidmodelkind)
      val representationresult = _generate(invalidrepresentation)

      Then("generation rejects both values before runtime initialization")
      modelkindresult shouldBe a[IllegalArgumentException]
      representationresult shouldBe a[IllegalArgumentException]
    }
  }

  private def _generate(
    descriptor: SComponent.EntityRuntimeDescriptor
  ): Throwable =
    intercept[IllegalArgumentException] {
      val component = _component(descriptor)
      new Scala3ComponentGenerator(ScalaModel.Context.default)
        .generate(component)
        .take
    }

  private def _component(
    descriptor: SComponent.EntityRuntimeDescriptor
  ): SComponent =
    SComponent(
      core = ClassCore(
        packageName = PackageName("example"),
        declaration = ClassDeclaration.Control,
        className = ClassName("ExampleComponent")
      ),
      componentCore = SComponent.ComponentCore(
        componentName = "example",
        services = Nil,
        entityRuntimeDescriptors = Vector(descriptor)
      )
    )
}
