# 🤖 HWM: Agent Contribution Rules

## 🟥 CRITICAL: MANDATORY VERIFICATION

**The task is NOT finished until these steps are executed in order:**

1. **Validation:** You **MUST** run `stack build` before reporting completion.
2. **Success Requirement:** If the build fails, you are forbidden from stopping. You must analyze the error, fix it, and run `stack build` again until it passes.
3. **No Assumptions:** Do not assume code works. Terminal output is the only proof of completion.

---

## 🏗️ 1. Domain-Based Architecture

- **Namespace Discipline:** All features must live in their resource namespace: `reg` (Registry), `env` (Matrix/Environment), or `pkg` (Workspace).
- **Directory Mapping:** Strict 1-to-1 mapping between CLI command and directory.
- Command `hwm reg add` ➔ `cli/command/reg/add.hs`
- Command `hwm env ls` ➔ `cli/command/env/ls.hs`
- **FORBIDDEN:** Flattened paths like `cli/command/reg_add.hs`.
- each cli command or options should be defined its own module e.g `RegistryAuditOptions` should be defined in `cli/command/registry/audit.hs` and not in `cli/command/registry` same goes for anothe arguments or subcommands of registry command, each of them should be defined in its own module. every comand should be self contained in its own module. and its should have its own `ParseCLI` instance for parsing its options. where main App.hs should only be responsible for parsing the top level command and delegating to the correct module for parsing its options and executing its logic.
---

## ⚙️ 2. Core Abstractions & Monads

- **Monad Stack:** Use generic monad constraints only. **FORBIDDEN:** Direct `IO` or `String`-based error handling.
- Use `MonadIO m` for all IO (lifted).
- Use `MonadError Issue m` for all errors.
- Use `MonadUI m` (from `HWM.Runtime.UI`) for all user interaction.

- **UI & Output:** **FORBIDDEN:** `putStrLn` or `print`. Use `MonadUI` functions and the `chalk` utility for colored, consistent terminal output.
- **DRY Policy:** Never duplicate logic for path resolution, YAML parsing, or version bounds. Search `hwm/src/HWM/` for existing utilities before writing new code.

---

## 📝 3. Haskell Syntax & Style

- **Required Pragmas:** Every new module **MUST** start with:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

```

and only if it uses `Text` literals, it should also include:

```haskell
{-# LANGUAGE OverloadedStrings #-}

```

- **Prelude:** **MUST** import `Relude`.
- **String Handling:** \* Use `Text` instead of `String`.
- **FORBIDDEN:** `T.pack` or `S.fromList` for literals. Use `OverloadedStrings` literals directly.
- **Error Types:** All errors **MUST** be wrapped in the `Issue` type for consistent error handling across the codebase.
- **Maybe**: use `maybe` or `fromMaybe` monad for optional values instead of manual pattern matching or if else checks.
- **Text**: Text manipulation should utilize `Data.Text` functions like `T.isInfixOf`, `T.pack`, etc., instead of converting to `String` for processing. where we use qualified imports for `Data.Text` as `T`.

## 📜 4. Documentation & Files

- **Spec-First:** Any change to CLI flags or logic **MUST** be mirrored in `docs/spec.md` before the code is finalized.
- **HWM.yaml Integrity:** Any modification to `hwm.yaml` must:

1. Update the internal hash-generation logic.
2. Implicitly trigger `hwm sync`.

---

## 🔍 5. Research Protocol

- **Step 1:** Search the codebase for the feature/pattern you are about to implement.
- **Step 2:** Identify the `Issue` type in the error handling logic that matches your failure cases.
- **Step 3:** Implement using the nested directory mapping.
- **Step 4:** **Execute `stack build`.**

## 6. Verification & Completion

- **Mandatory Build:** Before finalizing any task, you **MUST** run `stack build`.
- **The "3-Strike" Circuit Breaker:** - If `stack build` fails, you may attempt to fix the code and rebuild up to **3 times**.
  - If the build still fails after the 3rd attempt, you **MUST STOP** immediately.
  - **FORBIDDEN:** Do not attempt a 4th or 5th build in a row.
  - **Action:** Report the specific compiler error to the user and ask for human intervention/guidance.

## 7. Documentation Files

- **docs/spec.md**: Must be updated with any changes to CLI flags, command behavior, or output format before code changes are finalized. only add section if we impolement new feature otherwise just update existing sections. changes should be placed in the correct section. if new section is needed like new command then add it as a last item to its command section, or subsections.
- **docs/architecture.md**: Should be updated if any core architectural changes are made (e.g., new monads, major refactors).
- **docs/README.md**: Should be updated if there are changes to installation instructions, usage examples, or overall project description.
- **docs/roadmap.md**: Any new features or major changes should be reflected in the project roadmap documentation to keep track of future plans and current progress. add checkmark to completed items. and shrink its insdide a collapsible section.
