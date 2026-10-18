package org.simplemodeling.SimpleModeler.generator.scala

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.generators.scala.Scala3CaseClassGenerator

/*
 * @since   Oct. 14, 2026
 * @version Oct. 14, 2026
 * @author  ASAMI, Tomoharu
 */
final class Scala3RecordInputKeysGenerationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "G1 declared and wire input names" must afterWord(
    "in spec:scala3-record-input-keys, example:G1, rules:R1,R4, phase:sm-workflow-6"
  ) {
    "accept the derived wire candidate while preserving declared output names" in {
      Given("Spec: docs/spec/scala3-record-input-keys.md; Rules: R1,R4; Example: G1; a required sessionId with no external alias")
      val parameters = Vector(_parameter("sessionId"))

      When("the public Scala case-class generator produces the Value")
      val source = _generate(parameters)

      Then("the generated input candidates are declared then wire, with unchanged PROP and Record output")
      _input_keys(source) shouldBe Map("SESSION_ID" -> Vector("sessionId", "session_id"))
      _property_names(source) shouldBe Map("SESSION_ID" -> "sessionId")
      _output_pairs(source) should contain ("sessionId" -> "sessionId")
    }
  }

  "G2 explicit alias priority" must afterWord(
    "in spec:scala3-record-input-keys, example:G2, rules:R1,R2, phase:sm-workflow-6"
  ) {
    "place the explicit alias before the declaration and derived wire name" in {
      Given("Spec: docs/spec/scala3-record-input-keys.md; Rules: R1,R2; Example: G2; sessionId with externalName execution")
      val parameters = Vector(_parameter("sessionId", Some("execution")))

      When("the public Scala case-class generator produces the aliased Value")
      val source = _generate(parameters)

      Then("the generated candidates retain alias priority and declared output names")
      _input_keys(source) shouldBe Map("SESSION_ID" -> Vector("execution", "sessionId", "session_id"))
      _property_names(source) shouldBe Map("SESSION_ID" -> "sessionId")
      _output_pairs(source) should contain ("sessionId" -> "sessionId")
    }
  }

  "G3 nonempty distinct candidates" must afterWord(
    "in spec:scala3-record-input-keys, example:G3, rules:R1,R2, phase:sm-workflow-6"
  ) {
    "retain original priority for coincident aliases and unchanged declarations" in {
      Given("Spec: docs/spec/scala3-record-input-keys.md; Rules: R1,R2; Example: G3; declared, wire and empty aliases plus snake_case and plain declarations")
      val cases = Vector(
        ("sessionId", Some("sessionId"), "SESSION_ID", Vector("sessionId", "session_id")),
        ("sessionId", Some("session_id"), "SESSION_ID", Vector("session_id", "sessionId")),
        ("sessionId", Some(""), "SESSION_ID", Vector("sessionId", "session_id")),
        ("session_id", None, "SESSION_ID", Vector("session_id")),
        ("message", None, "MESSAGE", Vector("message"))
      )
      cases.foreach { case (name, alias, symbol, expected) =>
        When("the public Scala case-class generator produces each candidate-name variant")
        val source = _generate(Vector(_parameter(name, alias)))

        Then("the candidates contain no empty or duplicate names and keep their original priority")
        _input_keys(source) shouldBe Map(symbol -> expected)
        _property_names(source) shouldBe Map(symbol -> name)
        _output_pairs(source) should contain (name -> name)
      }
    }
  }

  "G4 shared multiplicity readers" must afterWord(
    "in spec:scala3-record-input-keys, example:G4, rules:R1,R5, phase:sm-workflow-6"
  ) {
    "supply ordered candidates to scalar, optional, vector and Record readers" in {
      Given("Spec: docs/spec/scala3-record-input-keys.md; Rules: R1,R5; Example: G4; required, optional, repeated and Record session fields")
      val parameters = Vector(
        _parameter("sessionId"),
        _parameter("optionalSessionId").toOptionType,
        _parameter("sessionIds").toVectorType,
        Parameter.create("sessionRecord", TypeName.record).copy(isAttribute = true)
      )

      When("the public Scala case-class generator produces the combined Value")
      val source = _generate(parameters)

      Then("each generated reader uses its shared ordered candidates without renaming outputs")
      _input_keys(source) shouldBe Map(
        "SESSION_ID" -> Vector("sessionId", "session_id"),
        "OPTIONAL_SESSION_ID" -> Vector("optionalSessionId", "optional_session_id"),
        "SESSION_IDS" -> Vector("sessionIds", "session_ids"),
        "SESSION_RECORD" -> Vector("sessionRecord", "session_record")
      )
      _reader_keys(source) should contain allOf (
        "SESSION_ID", "OPTIONAL_SESSION_ID", "SESSION_IDS", "SESSION_RECORD"
      )
      _property_names(source) shouldBe Map(
        "SESSION_ID" -> "sessionId", "OPTIONAL_SESSION_ID" -> "optionalSessionId",
        "SESSION_IDS" -> "sessionIds", "SESSION_RECORD" -> "sessionRecord"
      )
      parameters.foreach { parameter =>
        _output_pairs(source) should contain (parameter.name.name -> parameter.name.name)
      }
    }
  }

  private def _parameter(name: String, alias: Option[String] = None): Parameter =
    Parameter.create(name, TypeName.Primitive.string).copy(isAttribute = true, externalName = alias)

  private def _generate(parameters: Vector[Parameter]): String =
    new Scala3CaseClassGenerator(ScalaModel.Context.default)
      .generate(SCaseClass(ClassCore(
        PackageName("example"), ClassDeclaration.CaseClass, ClassName("InputEnvelope"),
        parameterSequence = ParameterSequence(parameters)
      )))
      .take.slots.head.content

  private def _input_keys(source: String): Map[String, Vector[String]] = {
    val definitions = """final\s+val\s+INPUT_KEYS_([A-Z_]+)\s*:\s*List\[String\]\s*=\s*List\(([^)]*)\)""".r
    val quoted = """"([^"]*)"""".r
    definitions.findAllMatchIn(source).map { definition =>
      definition.group(1) -> quoted.findAllMatchIn(definition.group(2)).map(_.group(1)).toVector
    }.toMap
  }

  private def _property_names(source: String): Map[String, String] =
    """final\s+val\s+PROP_([A-Z_]+)\s*=\s*"([^"]*)"""".r
      .findAllMatchIn(source).map(m => m.group(1) -> m.group(2)).toMap

  private def _output_pairs(source: String): Vector[(String, String)] =
    """"([^"]*)"\s*->\s*_to_external_value\(\s*([A-Za-z_][A-Za-z_0-9]*)\s*\)""".r
      .findAllMatchIn(source).map(m => m.group(1) -> m.group(2)).toVector

  private def _reader_keys(source: String): Vector[String] =
    """_record_get_(?:as_c\[[^\]]+\]|vector_as_c\[[^\]]+\]|record)\(\s*record\s*,\s*INPUT_KEYS_([A-Z_]+)\s*\)""".r
      .findAllMatchIn(source).map(_.group(1)).toVector
}
