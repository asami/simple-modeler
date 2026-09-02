package org.simplemodeling.SimpleModeler.transformers.scala

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import org.goldenport.record.v2.{XInt, XString}
import org.goldenport.values.Designation
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.Description
import org.simplemodeling.model._
import org.simplemodeling.model.domain.{MDomainResource, MDomainValue}
import org.simplemodeling.SimpleModeler.generator.scala.model.ScalaModel
import org.simplemodeling.SimpleModeler.generators.scala.Scala3EntityFamilyGenerator

/*
 * @since   Sep.  2, 2026
 * @author  ASAMI, Tomoharu
 */
final class ConcurrentScalaGenerationSpec
    extends AnyWordSpec
    with Matchers
    with GivenWhenThen {
  private implicit val _execution_context: ExecutionContext = ExecutionContext.global

  "Concurrent Scala generation" should {
    "isolate declared types and persistence metadata across repeated concurrent interleavings" in {
      Given("two models that declare the same short value name with distinct storage semantics")
      val alpharequest = _request("org.example.alpha", "AlphaAccount", true)
      val betarequest = _request("org.example.beta", "BetaAccount", false)

      When("both request contexts generate repeatedly on concurrent workers")
      val sources = Await.result(
        Future.sequence(
          Vector.tabulate(64) { index =>
            val request = if (index % 2 == 0) alpharequest else betarequest
            Future(_generate(request._1, request._2))
          }
        ),
        30.seconds
      )

      Then("every alpha generation keeps only alpha's declared value and scalar persistence metadata")
      sources.zipWithIndex.collect {
        case (source, index) if index % 2 == 0 => source
      }.foreach { source =>
        source should include("import org.example.alpha.value.Identifier")
        source should not include "org.example.beta.value.Identifier"
        source should include(
          "EntityStoreAttribute.scalarString(\"identifier\", \"identifier\")"
        )
      }

      And("every beta generation keeps only beta's declared value and non-scalar persistence metadata")
      sources.zipWithIndex.collect {
        case (source, index) if index % 2 == 1 => source
      }.foreach { source =>
        source should include("import org.example.beta.value.Identifier")
        source should not include "org.example.alpha.value.Identifier"
        source should include(
          "private val _store_record_attributes: Vector[EntityStoreAttribute] = Vector.empty"
        )
        source should not include "EntityStoreAttribute.scalarString(\"identifier\", \"identifier\")"
      }
    }

    "retain each request context across sequential generation" in {
      Given("two independent declared-type contexts")
      val alpharequest = _request("org.example.alpha", "AlphaAccount", true)
      val betarequest = _request("org.example.beta", "BetaAccount", false)

      When("their source families are generated in alternating sequence")
      val alphasourcefirst = _generate(alpharequest._1, alpharequest._2)
      val betasource = _generate(betarequest._1, betarequest._2)
      val alphasourcelast = _generate(alpharequest._1, alpharequest._2)

      Then("the second request cannot replace the first request's immutable registry")
      alphasourcefirst should include("import org.example.alpha.value.Identifier")
      betasource should include("import org.example.beta.value.Identifier")
      alphasourcelast should include("import org.example.alpha.value.Identifier")
      alphasourcelast should not include "org.example.beta.value.Identifier"
    }

    "compose nested identity and declared-type scopes before restoring the outer request" in {
      Given("outer and inner request contexts with the same short declared type name")
      val outerrequest = _request("org.example.outer", "OuterAccount", true)
      val innerrequest = _request("org.example.inner", "InnerAccount", false)

      When("the inner request is scoped inside the outer component identity")
      val observations = ScalaModel.Context.withContext(outerrequest._1) {
        ScalaModel.Context.withComponentIdentity("outer", "outer-id", Some("Outer")) {
          val outerbefore = (
            ScalaModel.Context.current.componentNamespace,
            ScalaModel.Context.current.resolveDeclaredType("Identifier", "org.example.outer")
              .map(_.qualifiedName)
          )
          val inner = ScalaModel.Context.withContext(innerrequest._1) {
            ScalaModel.Context.withComponentIdentity("inner", "inner-id", Some("Inner")) {
              (
                ScalaModel.Context.current.componentNamespace,
                ScalaModel.Context.current.resolveDeclaredType("Identifier", "org.example.inner")
                  .map(_.qualifiedName)
              )
            }
          }
          val outerafter = (
            ScalaModel.Context.current.componentNamespace,
            ScalaModel.Context.current.resolveDeclaredType("Identifier", "org.example.outer")
              .map(_.qualifiedName)
          )
          (outerbefore, inner, outerafter)
        }
      }

      Then("the nested identity uses its own registry")
      observations._2 shouldBe (
        Some("inner"),
        Some("org.example.inner.value.Identifier")
      )

      And("the outer identity and registry are restored after the nested scope")
      observations._1 shouldBe (
        Some("outer"),
        Some("org.example.outer.value.Identifier")
      )
      observations._3 shouldBe observations._1
    }

    "restore the prior context when a scoped generation path fails" in {
      Given("outer and failing request-owned declared-type contexts")
      val outerrequest = _request("org.example.outerfailure", "OuterFailureAccount", true)
      val failingrequest = _request("org.example.failure", "FailureAccount", false)

      When("an inner scoped body throws")
      val observations = ScalaModel.Context.withContext(outerrequest._1) {
        val thrown = the[IllegalStateException] thrownBy {
          ScalaModel.Context.withContext(failingrequest._1) {
            throw new IllegalStateException("expected scope failure")
          }
        }
        val restored = ScalaModel.Context.current.resolveDeclaredType(
          "Identifier",
          "org.example.outerfailure"
        ).map(_.qualifiedName)
        (thrown, restored)
      }

      Then("the failure is exposed to the caller")
      observations._1.getMessage shouldBe "expected scope failure"

      And("the outer request context is restored after the failure")
      observations._2 shouldBe Some("org.example.outerfailure.value.Identifier")

      And("the outer scope is removed after completion")
      ScalaModel.Context.current shouldBe ScalaModel.Context.default
    }

    "preserve deterministic short-name lookup for duplicate request-local declarations" in {
      Given("one immutable registry whose first, requested-package, and scope-package values share a short name")
      val first = _declared_value("Shared", "org.example.first")
      val requested = _declared_value("Shared", "org.example.requested")
      val scoped = _declared_value("Shared", "org.example.scope")
      val missingscope = _declared_value("LookupScope", "org.example.missing")
      val registry = ScalaModel.DeclaredTypeRegistry(
        Vector(first, requested, scoped, missingscope)
      )
      val unqualified = MObjectRef.create("Shared")
      val requestedref = MObjectRef.create("org.example.requested.Shared")

      When("object and declared-type lookup resolve the duplicate short name")
      val objectobservations = (
        registry.resolveObject(unqualified, scoped).map(_.qualifiedName),
        registry.resolveObject(requestedref, missingscope).map(_.qualifiedName),
        registry.resolveObject(unqualified, missingscope).map(_.qualifiedName)
      )
      val declaredtypeobservations = (
        registry.resolveDeclaredType(unqualified, "org.example.scope").map(_.qualifiedName),
        registry.resolveDeclaredType(requestedref, "org.example.missing").map(_.qualifiedName),
        registry.resolveDeclaredType(unqualified, "org.example.missing").map(_.qualifiedName)
      )

      Then("object lookup prefers scope, then the requested package, then the first registered candidate")
      objectobservations shouldBe (
        Some("org.example.scope.Shared"),
        Some("org.example.requested.Shared"),
        Some("org.example.first.Shared")
      )

      And("declared-type lookup retains the same deterministic legacy precedence")
      declaredtypeobservations shouldBe objectobservations
    }
  }

  private def _generate(
    scalacontext: ScalaModel.Context,
    entity: MDomainResource
  ): String =
    new Scala3EntityFamilyGenerator(scalacontext)
      .generate(entity)
      .take
      .slots
      .map(_.content)
      .mkString("\n")

  private def _request(
    namespace: String,
    accountname: String,
    scalarstring: Boolean
  ): (ScalaModel.Context, MDomainResource) = {
    val identifierdatatype = if (scalarstring) XString else XInt
    val identifier = MDomainValue(
      description = Description.name("Identifier"),
      affiliation = MPackageRef(s"$namespace.value"),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(Designation("value"), MDataType(Designation("value"), identifierdatatype, MPackageRef.default), MOne, Nil, None)
      ),
      operations = Nil
    )
    val account = MDomainResource(
      description = Description.name(accountname),
      affiliation = MPackageRef(namespace),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = List(
        MAttribute(
          Designation("identifier"),
          MDataType(Designation("Identifier"), XString, MPackageRef(s"$namespace.value")),
          MOne,
          Nil,
          None
        )
      ),
      associations = Nil,
      operations = Nil,
      stateMachines = Nil
    )
    (ScalaModel.Context.fromObjects(Vector(identifier, account)), account)
  }

  private def _declared_value(name: String, pkg: String): MDomainValue =
    MDomainValue(
      description = Description.name(name),
      affiliation = MPackageRef(pkg),
      stereotypes = Nil,
      base = None,
      traits = Nil,
      powertypes = Nil,
      attributes = Nil,
      operations = Nil
    )
}
