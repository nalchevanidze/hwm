# Golden Test Plan: Missing Domains

This document tracks the golden-test expansion and current implementation status.

## Scope

Missing domains originally targeted:
- `registry`
- `version`
- `build`
- `install`
- `test`

Goal:
- 100% command-level golden coverage for these domains
- Strong scenario coverage (success + failure + edge behavior)

---

## Phase Plan (with status)

### Phase A — Test Infrastructure
- [ ] Add new command test module: `hwm-golden/test/Commands/Registry.hs`
- [x] Add new command test module: `hwm-golden/test/Commands/Version.hs`
- [x] Add new command test module: `hwm-golden/test/Commands/Build.hs`
- [x] Add new command test module: `hwm-golden/test/Commands/Install.hs`
- [x] Add new command test module: `hwm-golden/test/Commands/Test.hs`
- [x] Register new modules in `hwm-golden/test/Main.hs`
- [x] Deterministic toolchain stubs support
  - Implemented via shared project stubs in `hwm-golden/test/projects/simple-bin/bin/*`
  - No per-scenario `override/bin` duplication

### Phase B — Scenario Implementation
- [x] Implement `version` matrix
- [ ] Implement `registry` matrix
- [x] Implement `build` matrix
- [x] Implement `test` matrix
- [x] Implement `install` matrix (including unsupported combinations)

### Phase C — Hardening
- [x] Add edge/failure scenarios for implemented domains
  - unknown workspace/member
  - env fanout (`--env all`)
  - excluded package behavior
  - unsupported install builder combinations
- [x] Ensure output determinism
  - fixed `HOME` and `PATH` in golden runner
  - fixed log-id seed (`HWM_LOG_ID_FIXED`)
  - command invocations captured in `invocations.yaml`

---

## Scenario Matrix (status)

### 1) Registry

#### `registry add`
- [ ] `registry/add/new-registry-only`
- [ ] `registry/add/new-with-workspace`
- [ ] `registry/add/already-registered`
- [ ] `registry/add/reject-unknown-workspace` (fail)

#### `registry audit`
- [ ] `registry/audit/check-clean`
- [ ] `registry/audit/check-outdated`
- [ ] `registry/audit/check-conflict` (fail)
- [ ] `registry/audit/fix`
- [ ] `registry/audit/fix-force`

#### `registry ls`
- [ ] `registry/ls/all`
- [ ] `registry/ls/search-hit`
- [ ] `registry/ls/search-empty`

---

### 2) Version
- [x] `version/show-current`
- [x] `version/bump-patch`
- [x] `version/bump-minor`
- [x] `version/bump-major`
- [x] `version/set-fixed-higher`
- [x] `version/set-fixed-same`
- [x] `version/set-fixed-lower-warning`
- [x] `version/reject-invalid-bump` (fail)

---

### 3) Build
- [x] `build/default-global`
- [x] `build/env-specific`
- [x] `build/env-all`
- [x] `build/scope-group`
- [x] `build/scope-member`
- [x] `build/fast`
- [x] `build/reject-unknown-workspace` (fail)
- [x] `build/stack-dispatch-shape`
- [x] `build/nix-dispatch-shape`
- [x] `build/excluded-pkg`

---

### 4) Install
- [x] `install/default-global`
- [x] `install/env-all`
- [x] `install/scope-member`
- [x] `install/fast`
- [x] `install/stack-local-bin-path`
- [x] `install/cabal-install-args`
- [x] `install/reject-nix-builder` (fail)
- [x] `install/reject-nix-cabal-builder` (fail)

---

### 5) Test
- [x] `test/default-global`
- [x] `test/env-all`
- [x] `test/scope-group`
- [x] `test/scope-member`
- [x] `test/fast`
- [x] `test/reject-unknown-workspace` (fail)
- [x] `test/stack-dispatch-shape`
- [x] `test/nix-dispatch-shape`

---

## Methodology for `build` / `install` / `test` (implemented)

These commands call external tools (`cabal`, `stack`, `nix`). Golden tests validate dispatch behavior, not real compilation.

### 1) Shared fake executables (project-level)
Implemented in:
- `hwm-golden/test/projects/simple-bin/bin/cabal`
- `hwm-golden/test/projects/simple-bin/bin/stack`
- `hwm-golden/test/projects/simple-bin/bin/nix`

Behavior:
- records command + args + env markers to `invocations.yaml`
- uses `cat` to include generated matrix files (`CABAL_PROJECT_FILE`, `STACK_YAML`) content in `invocations.yaml`

### 2) Avoid copying `.hwm`
Golden runner now removes `.hwm` when copying fixtures into temp workdir.

### 3) Control runtime environment
Golden runner sets deterministic env:
- `PATH=<workdir>/bin:<workdir>/.home/.local/bin:$PATH`
- `HOME=<workdir>/.home`
- `HWM_LOG_ID_FIXED=golden`

### 4) Assertions per scenario
Golden checks validate:
1. `stdout.ansi` (user-visible behavior)
2. `expected/invocations.yaml` (dispatch contract, including matrix file contents)
3. `delta.yaml` and `expected/*` (file-change behavior)

---

## Coverage Targets (Golden)

Current status for previously missing domains:
- `version`: implemented
- `build`: implemented
- `install`: implemented
- `test`: implemented
- `registry`: pending

Target remains:
- Command coverage: **100%**
- Behavior coverage target: **~85–95%**
