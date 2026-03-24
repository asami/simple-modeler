# Powertype/StateMachine Generation Update

status=implemented
published_at=2026-03-24
owner=simple-modeler

## Summary

Implemented generation pipeline support for top-level `POWERTYPE` and `STATEMACHINE`,
and added component metadata pass-through for state machine definitions.

## Main Changes

### 1. Program/Scala realm transformer routing

Files:
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/transformer/ProgramRealmTransformerBase.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/transformer/ScalaRealmTransformerBase.scala`

Added build routes:

- `MPowertype` -> `build_Powertype`
- `MStateMachine` -> `build_StateMachine`

### 2. Family generators and transformers

Files:
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/generators/scala/Scala3PowertypeFamilyGenerator.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/generators/scala/Scala3StateMachineFamilyGenerator.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/transformers/scala/PowertypeScalaModelTransformer.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/transformers/scala/StateMachineScalaModelTransformer.scala`

Added plain-purpose code generation path for powertype/statemachine models.

### 3. Component metadata extension

Files:
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/model/MComponent.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/generator/scala/model/ScalaModel.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/transformers/scala/ComponentScalaModelTransformer.scala`
- `/Users/asami/src/dev2025/simple-modeler/src/main/scala/org/simplemodeling/SimpleModeler/generator/scala/ComponentPart.scala`

Added:

- `StateMachineDefinition` model
- `stateMachineDefinitions` field in component core
- generated override method:
  - `stateMachineDefinitions: Vector[org.goldenport.cncf.statemachine.CmlStateMachineDefinition]`

## Validation

Executed:

- `sbt --batch compile`
- `sbt --batch publishLocal`

Result:

- pass
