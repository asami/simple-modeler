# Scala 3 Record Input Keys

status=draft
scope=generated Scala 3 Value Record readers

## Rules

- **R1 Ordered input names.** Generated `INPUT_KEYS` contain nonempty,
  distinct names in this order: explicit `externalName`, declared name,
  then the snake_case name derived from the declaration.
- **R2 Existing priority.** An explicit alias retains priority over the
  declared name, and the declaration retains priority over the wire fallback.
  Coincident names appear once at their original position.
- **R3 Required and malformed input.** Absent candidates retain structured
  missing-property failures. A decoding failure at an earlier present
  candidate remains a failure; it does not fall through to a later key.
- **R4 Output and API stability.** Declared output names, `PROP` values,
  Scala APIs, schemas, stores, other-language aliases and ValueReader
  semantics remain unchanged. The input fallback does not rename output.
- **R5 Shared readers.** Scalar, optional, collection and Record readers
  already using `INPUT_KEYS` share the same ordered candidate list without
  changing their multiplicity or reader folding.

## Generation examples

The executable counterpart is
[Scala3RecordInputKeysGenerationSpec](../../src/test/scala/org/simplemodeling/SimpleModeler/generator/scala/Scala3RecordInputKeysGenerationSpec.scala).
It checks actual generated products through the public case-class generator.

- **G1 (R1, R4):** `sessionId` without an alias accepts `sessionId`, then
  `session_id`; its `PROP` and `toRecord` output remain `sessionId`.
- **G2 (R1, R2):** An `execution` alias for `sessionId` accepts `execution`,
  `sessionId`, then `session_id`, retaining declared output names.
- **G3 (R1, R2):** Aliases `sessionId`, `session_id` and the empty string,
  plus already snake_case or plain declared names, produce only nonempty,
  distinct candidates with the original priority.
- **G4 (R1, R5):** Required `sessionId`, optional `optionalSessionId`,
  repeated `sessionIds` and Record `sessionRecord` retain ordered declared
  and snake_case names through their existing readers and output names.

## Compiled consumer examples

The downstream executable counterpart is
[WorkflowOperationValueBindingSpec](../../../sm-workflow/src/test/scala/org/simplemodeling/textus/smworkflow/admission/WorkflowOperationValueBindingSpec.scala).

- **B1 (R1, R5):** All three compiled workflow StartResult readers consume
  strict JSON Record input with `session_id`, `outcome` and a Japanese
  message, retaining the exact typed values.
- **B2 (R2, R3):** When `sessionId` and `session_id` are both present, all
  three compiled readers choose the declared value. Missing session
  candidates or another required field still fail.
