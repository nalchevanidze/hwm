# 🤖 HWM: Agent Contribution Rules

## 1. Research & Abstraction

* **Search First:** Before proposing new inline code, search the existing codebase for relevant utilities or patterns.
* **Reuse Abstractions:** Use existing types and internal APIs (e.g., `Registry` or `Matrix` modules) rather than writing custom configuration logic.
* **Avoid "God Functions":** Keep CLI command handlers lean; logic that belongs in the core library must stay there.

## 2. Code Integrity & DRY

* **Strict DRY Policy:** Do not duplicate logic for file path resolution, YAML parsing, or version bound calculations.
* **Centralize Shared Logic:** If logic is needed across multiple commands, move it to a shared internal module.
* **Standardized Error Handling:** Use the existing `MonadError m Issue` pattern. Ad-hoc printing is forbidden; error messages must be clear, actionable, and follow the project style.
* **UI Abstraction:** Use the `MonadUI` class from `hwm/src/HWM/Runtime/UI.hs` instead of directly printing with `putStrLn` or using IO utilities.
* **Consistent Formatting:** Use the `chalk` function for colored output to maintain a consistent user experience.
* Generic monads (e.g., `MonadIO`, `MonadError`, `MonadUI`) **MUST** be used in CLI command implementations to ensure composability and testability. no direct IO or error handling without these abstractions. every IO should be lifted into generiic `MonadIO m` and every error should be thrown using `MonadError m Issue` with well-defined `Issue` types.

## 3. Syntax & Prelude

* **No Implicit Prelude:** Every new Haskell module **MUST** include the `{-# LANGUAGE NoImplicitPrelude #-}` pragma at the top.
* **Overloaded Strings:** don't use `T.pack` or `S.fromList`use pragma `{-# LANGUAGE OverloadedStrings #-}` and write string literals directly.
* **Relude Integration:** Every new module **MUST** import `Relude` for consistency across the ecosystem.
* **Text Handling:** Prefer `Text` over `String` for all textual data.

## 4. Resource-Oriented (Domain-Based) Architecture

* **Namespace Discipline:** Features must reside in their respective resource namespace (`reg`, `env`, or `pkg`).
* **Nested Directory Mapping:** A command `hwm x y` must have its module in `cli/command/x/y.hs` rather than a flattened `cli/command/xy.hs`.
* **CLI State Awareness:** Commands modifying state (`add`, `audit --fix`) must re-verify environment health using internal monitoring (like `ls`) before proceeding.
* **Implicit Sync:** Any feature modifying `hwm.yaml` must implicitly trigger synchronization to keep the workspace consistent.

## 5. Documentation & Schema

* **Spec Alignment:** New CLI flags or behaviors must be updated in `docs/spec.md` before implementation.
* **Hash Integrity:** Respect the file hash system; ensure any changes to `hwm.yaml` update the hash-generation logic to prevent drift.

## 6. Verification & Completion

* **Mandatory Build:** Before finalizing any task, you **MUST** run `stack build`.
* **Success Requirement:** The work is not finished if the build fails. You must iterate until `stack build` passes.
