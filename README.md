# SimpleModeler

SimpleModeler is a model-driven generation toolkit that interprets SimpleModel definitions and emits Scala/Java, diagram, and documentation artifacts for downstream consumption.

---

> Note
>
> This README.md is a project-local overview generated from the AI Directive template. It is not an authoritative AI rule document.

## Overview

SimpleModeler parses SimpleModel DSL inputs (`org.simplemodeling.parser.SimpleModelParser`) and coordinates transformer/generator pipelines in `org.simplemodeling.SimpleModeler` to produce language- and artifact-specific outputs (Scala3 classes, Java/diagram generators, specification HTML, etc.).

The repository operates under the **AI Directive** contract; AI activity is governed by the directive’s explicit instructions rather than implicit assumptions.

---

## AI Directive (Authoritative)

This project adopts the **AI Directive** mounted at:

    ai/directive

The AI Directive is managed as a **git submodule** and treated as an external, versioned contract shared across multiple projects.

### Authority and Priority

AI behavior MUST be interpreted in the following order:

1. **AI Directive**
   - ai/directive/core
   - active profile (e.g. chatgpt-desktop, codex)
2. **Project-specific rules**
   - docs/rules/
3. Other documentation (docs/spec, docs/design, docs/notes, docs/journal)

The AI Directive is authoritative. Project-specific rules may add constraints but MUST NOT implicitly override it.

[FIXED — contractual section]

---

## AI Entry Points

The following files are the **authoritative AI entry points** for this project:

- AGENT.md → symlink to ai/directive/core/AGENT.md
- RULE.md  → symlink to ai/directive/core/RULE.md

AI systems MUST start from these files. No additional agent or rule files at the project root are authoritative.

---

## AI Profiles Used

This project uses the following AI profiles defined by the AI Directive:

- **chatgpt-desktop**
  - Interactive reasoning, design, review, and documentation
  - Human-in-the-loop
  - MUST NOT execute commands or modify files directly

- **codex**
  - Execution-only agent
  - Operates strictly under explicit execution instructions
  - MUST stop on ambiguity or unsafe conditions

Profile-specific rules are defined under:

    ai/directive/chatgpt-desktop/
    ai/directive/codex/

No additional project-specific profile restrictions are currently defined.

---

## Documentation Structure

Project documentation is organized into semantic layers under docs/:

- docs/notes/  
  Exploratory notes, hypotheses, and informal records (non-normative, disposable)
- docs/design/  
  Architectural decisions, responsibility boundaries, and invariants
- docs/spec/  
  Behavioral specifications that implementations must satisfy
- docs/journal/  
  Chronological records (decisions, incidents, releases)

These layers are **not interchangeable**. Each carries a distinct semantic weight.

### Documentation Lifecycle (Authoritative)

The authoritative definition of the documentation lifecycle, including promotion rules and interpretation constraints, lives in:

    ai/directive/core/document-lifecycle.md

AI systems and contributors MUST follow that document when creating, moving, or interpreting documentation.

---

## Project-Specific Rules

Project-specific AI rules and constraints (if any) live under:

    docs/rules/

If docs/rules/ is empty, no additional project-specific constraints apply.

---

## Repository Structure (Relevant Parts)

```
.
├─ AGENT.md                   # AI entry point (symlink to ai/directive/core/AGENT.md)
├─ RULE.md                    # AI rules entry point (symlink to ai/directive/core/RULE.md)
├─ ai/                        # AI Directive (git submodule)
│  └─ directive/
├─ docs/
│  ├─ rules/                  # Project-specific AI rules
│  ├─ spec/
│  ├─ design/
│  ├─ notes/
│  └─ journal/
├─ src/                       # SimpleModeler implementation and generators
└─ README.md                  # This file
```

Adapt or extend this layout as needed, but keep AI directives under ai/directive and docs inside docs/.

---

## Contribution Notes

- Do NOT modify files under ai/directive/ from this repository. Those changes must flow through the ai-directive project.
- Updates to the AI Directive must be recorded as submodule updates.
- Documentation must respect the document lifecycle (`ai/directive/core/document-lifecycle.md`) and checklist semantics (`ai/directive/core/phase-subphase-checklist.md`).
- Follow the instructions in `docs/rules/` for any project-specific exceptions or extra discipline.

---

## Summary

This repository makes AI behavior explicit, versioned, and reviewable.

- The AI contract lives in ai/directive.
- SimpleModeler explicitly adopts that contract.
- Documentation and code are interpreted under that shared understanding.
