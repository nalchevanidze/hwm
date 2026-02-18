
# 🤖 HWM: Agent Contribution Rules

## 1. Research Before Implementation

* **Search First:** Before proposing any new inline code, search the existing codebase for relevant utilities or patterns.
* **Reuse Abstractions:** Use existing types and internal APIs (e.g., the `Registry` or `Matrix` modules) instead of writing custom logic for configuration handling.
* **Avoid "God Functions":** If you are adding logic to a resource command like `hwm reg add`, ensure you aren't duplicating logic that belongs in the core library.

## 2. Zero-Tolerance for Code Duplication

* **Strict Dry Policy:** Do not duplicate logic for file path resolution, YAML parsing, or version bound calculations.
* **Centralize Logic:** If a logic block is needed in two different commands, move it to a shared internal module.
* **Uniform Error Handling:** Use the project's standard error reporting format which uses `MonadError m Issue`; do not create new ad-hoc error printing logic. and error messages should be clear and actionable, following the existing style.

## 3. Resource-Oriented Command Integrity

* **Namespace Discipline:** Features must be implemented within their respective resource namespace (`reg`, `env`, or `pkg`). e.g command "hwm x y" module should be in `cli/command/x/ady` module instead of flatting it in `cli/command/xy`.
* **CLI State Awareness:** Commands that modify state (like `add` or `audit --fix`) must always re-verify the environment using internal monitoring logic (like `ls`) before proceeding.
* **Implicit Sync:** Any feature that modifies `hwm.yaml` must implicitly trigger the synchronization logic to keep the workspace consistent.

## 4. Documentation & Schema

* **Spec Alignment:** Every new CLI flag or behavior must be reflected in the `docs/spec.md` before implementation.
* **Hash Integrity:** You must respect the file hash system; never suggest a change to `hwm.yaml` that would break the hash without updating the hash-generation logic.