# Golden Test Plan: Missing Domains

This document captures the golden-test expansion plan for missing domains and the methodology for testing `build`, `install`, and `test` safely and deterministically.

## Scope

Missing domains to cover:
- `registry`
- `version`
- `build`
- `install`
- `test`

Goal:
- 100% command-level golden coverage for these domains
- Strong scenario coverage (success + failure + edge behavior)

---

## Phase Plan

### Phase A — Test Infrastructure
1. Add new command test modules:
   - `hwm-golden/test/Commands/Registry.hs`
   - `hwm-golden/test/Commands/Version.hs`
   - `hwm-golden/test/Commands/Build.hs`
   - `hwm-golden/test/Commands/Install.hs`
   - `hwm-golden/test/Commands/Test.hs`
2. Register them in `hwm-golden/test/Main.hs`.
3. Add deterministic toolchain stubs support for golden scenarios.

### Phase B — Scenario Implementation
4. Implement `version` matrix.
5. Implement `registry` matrix.
6. Implement `build` matrix.
7. Implement `test` matrix.
8. Implement `install` matrix (including unsupported combinations).

### Phase C — Hardening
9. Add edge/failure scenarios (unknown workspace/member, env fanout, exclusion behavior).
10. Ensure output determinism (stable stdout + stable invocation logs).

---

## Scenario Matrix (Exhaustive Baseline)

### 1) Registry

#### `registry add`
- `registry/add/new-registry-only`
- `registry/add/new-with-workspace`
- `registry/add/already-registered`
- `registry/add/reject-unknown-workspace` (fail)

#### `registry audit`
- `registry/audit/check-clean`
- `registry/audit/check-outdated`
- `registry/audit/check-conflict` (fail)
- `registry/audit/fix`
- `registry/audit/fix-force`

#### `registry ls`
- `registry/ls/all`
- `registry/ls/search-hit`
- `registry/ls/search-empty`

---

### 2) Version
- `version/show-current`
- `version/bump-patch`
- `version/bump-minor`
- `version/bump-major`
- `version/set-fixed-higher`
- `version/set-fixed-same`
- `version/set-fixed-lower-warning`
- `version/reject-invalid-bump` (fail)

---

### 3) Build
- `build/default-global`
- `build/env-specific`
- `build/env-all`
- `build/scope-group`
- `build/scope-member`
- `build/fast`
- `build/reject-unknown-workspace` (fail)
- `build/stack-dispatch-shape`
- `build/nix-dispatch-shape`
- `build/excluded-pkg`

---

### 4) Install
- `install/default-global`
- `install/env-all`
- `install/scope-member`
- `install/fast`
- `install/stack-local-bin-path`
- `install/cabal-install-args`
- `install/reject-nix-builder` (fail)
- `install/reject-nix-cabal-builder` (fail)

---

### 5) Test
- `test/default-global`
- `test/env-all`
- `test/scope-group`
- `test/scope-member`
- `test/fast`
- `test/reject-unknown-workspace` (fail)
- `test/stack-dispatch-shape`
- `test/nix-dispatch-shape`

---

## Methodology for `build` / `install` / `test`

These commands call external tools (`cabal`, `stack`, `nix`). Golden tests should validate dispatch behavior, not perform real compilation.

### 1) Use scenario-local fake executables
For each relevant scenario, provide in override:
- `override/bin/cabal`
- `override/bin/stack`
- `override/bin/nix`

Each stub should:
- record command + args + key env vars to `invocations.yaml`
- exit with controlled status (0 for success scenarios; nonzero only where needed)

### 2) Control runtime environment during golden run
Set deterministic environment for each run:
- prepend `./bin` to `PATH`
- set `HOME=./.home` (stabilizes install-dir behavior)

### 3) Assertions per scenario
Golden checks should validate:
1. `stdout.ansi` (user-visible behavior)
2. `expected/invocations.yaml` (dispatch contract)
3. `delta.json` and `expected/*` (file change behavior)

### 4) What to verify specifically

#### Build/Test
- environment selection (`default`, explicit, `all`)
- target scope mapping (global/group/member)
- fast-flag mapping by builder
- builder command shape (cabal/stack/nix)

#### Install
- local bin path behavior
- builder-specific install args
- unsupported combinations fail early (`nix`, `nix/cabal` install)
- PATH warning behavior where applicable

---

## Coverage Targets (Golden)

For missing domains after this plan:
- Command coverage: **100%**
- Behavior coverage target: **~85–95%**

This gives broad confidence in CLI dispatch, option handling, scope/env routing, and failure semantics without flaky real builds.
