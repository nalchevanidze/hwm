# HWM Architecture Documentation (Implementation-Aligned)

**Target Audience:** AI agents, maintainers, contributors  
**Baseline:** current tracked code in this repository  
**Version:** 0.2.x aligned  
**Last Updated:** 2026-03-24

---

## 1. High-level architecture

```text
Main (hwm/app/Main.hs)
  -> CLI App Parser (HWM.CLI.App)
  -> Command Router (HWM.CLI.Command)
  -> Config Runtime (HWM.Domain.ConfigT)
  -> Domain Logic + Integrations + Runtime Services
```

Layered organization (logical):

1. **CLI layer** (`HWM.CLI.*`)  
   Parsing commands/options and invoking handlers.
2. **Domain layer** (`HWM.Domain.*`)  
   Core models and orchestration logic (config, environments, workspace, registry, release, build dispatch).
3. **Integration layer** (`HWM.Integrations.Toolchain.*`)  
   Tool-specific generation/execution for Stack/Cabal/Nix/Hie/Hpack/GitHub.
4. **Runtime layer** (`HWM.Runtime.*`)  
   Files, cache, network, process execution, UI, logging.
5. **Core primitives** (`HWM.Core.*`)  
   Shared types/parsing/formatting/versioning/results/options.

---

## 2. CLI surface and routing

### 2.1 Entry point

- `hwm/app/Main.hs` delegates to `HWM.CLI.App.main`.

### 2.2 Parser

`HWM.CLI.App` defines top-level commands:

- `init`, `status`, `sync`, `run`
- `workspace`, `environments`, `registry`
- `version`, `build`, `install`, `test`, `release`

Global flags:

- `--version`, `--quiet`

Fallback behavior:

- `hwm <script> ...` parses as `run` shortcut.

### 2.3 Router

`HWM.CLI.Command` maps parsed commands to handlers in `ConfigT`.

---

## 3. Runtime execution model (`ConfigT`)

`ConfigT` is the application monad:

```haskell
newtype ConfigT a = ConfigT (ReaderT Env (ResultT (UIT IO)) a)
```

`Env` includes:

- `options :: Options`
- `config :: Config`
- `cache :: Cache`
- `pkgs :: PkgRegistry`
- `fileSignature :: Signature`

### Key properties

- **Read-mostly context:** config/options/registry access via `Reader` + `Has` pattern
- **Issue/error handling:** via `ResultT` + `Issue`
- **Terminal UI:** via `UIT`/`MonadUI`
- **Config consistency check:** `runConfigT` compares file signature and environment signature, triggers validation/save when needed
- **Cache persistence:** loaded at startup, saved on success

---

## 4. Domain model

### 4.1 Config

`HWM.Domain.Config.Config` fields:

- `cfgName`, `cfgVersion`
- `cfgWorkspace`
- `cfgEnvironments`
- optional: `cfgGithub`, `cfgBounds`, `cfgRegistry`, `cfgScripts`, `cfgRelease`

### 4.2 Workspace

`Workspace = Map Name WorkGroup`, where `WorkGroup` has:

- optional `dir`
- `members`
- optional `prefix`

Resolution supports `group` and `group/member` targets.

### 4.3 Environments

`Environments` contains:

- `envsDefault`
- `envsProfiles :: Map Name EnviromentProfile`
- optional global toggles/builder (`envsNix`, `envsStack`, `envsHie`, `envsBuilder`)

`BuildEnvironment` is resolved per profile with:

- GHC, resolver, packages, builder
- feature toggles (stack/nix/hie)
- package exclusion handling

### 4.4 Registry and bounds

Registry is a map `PkgName -> Bounds`, exposed through:

- add / lookup / list helpers
- audit/update flows against tested ranges

### 4.5 Build dispatch

`HWM.Domain.Dispatcher` builds executable command plans per environment:

- prepares toolchain env vars (`STACK_YAML` or `CABAL_PROJECT_FILE`)
- applies builder-specific command transformation
- executes through runtime process/logging subsystem

> Current behavior: environment runs are dispatched sequentially by `dispatchForEach` (not multi-env parallel execution).

---

## 5. Integration layer details

### 5.1 Stack (`Integrations.Toolchain.Stack`)

Responsibilities:

- read and write `stack.yaml`
- create matrix stack files in `.hwm/matrix/stack-<env>.yaml`
- infer environments from stack files during `init`

### 5.2 Cabal (`Integrations.Toolchain.Cabal`)

Responsibilities:

- generate `cabal.project`
- parse/update cabal package descriptions
- run `sdist` for release publishing
- optional environment matrix setup via `CABAL_PROJECT_FILE`

### 5.3 Package synchronization (`Integrations.Toolchain.Package`)

Responsibilities:

- validate and sync package dependency/version consistency
- inject dependencies (`registry add` flow)
- sync across hpack/cabal sources

### 5.4 Hie (`Integrations.Toolchain.Hie`)

Responsibilities:

- generate `hie.yaml` cradle components from package source dirs

### 5.5 Nix (`Integrations.Toolchain.Nix`, `Nix.Build`)

Responsibilities:

- generate `flake.nix`
- define per-environment package/dev-shell/check outputs
- support Nix build command generation

Limitations reflected in code:

- `install` with Nix builder is unsupported
- artifact build path for pure Nix builder has explicit unsupported branch in build command path

### 5.6 GitHub helper (`Integrations.Toolchain.Github`)

Responsibilities:

- safety check release tagging (`ensureIsLatestTag`)

---

## 6. Runtime services

### 6.1 Files (`Runtime.Files`)

- YAML read/rewrite helpers
- structural rewrite with status (`Checked`/`Updated`)
- hash header support (`# hash: ...`)
- safe directory prep and sync helpers

### 6.2 Cache (`Runtime.Cache`)

- TVar-backed cache registry:
  - `currentEnv`
  - resolved package versions
- persisted to `.hwm/cache/state.json`
- includes stackage snapshot and hackage version fetch helpers

### 6.3 Process (`Runtime.Process`)

- executes commands with environment vars
- writes command logs to `.hwm/logs/*.log`
- supports spinner/status UI feedback and error conversion to `Issue`
- `inheritRun` for script-style direct process execution

### 6.4 UI (`Runtime.UI`)

- section/table/subpath rendering
- summary rendering for warnings/errors
- CI mode support for log extraction on failures

### 6.5 Logging (`Runtime.Logging`)

- start/end command event logging
- timestamped log ids and log path conventions
- log rotation utility

### 6.6 Network (`Runtime.Network`, `Runtime.Snapshots`)

- GitHub release upload URL resolution + asset upload
- Hackage upload/token handling
- Stackage snapshot suggestion APIs

---

## 7. Release architecture

Release domain has two tracks:

1. **Artifacts** (`hwm release artifacts`)  
   Build selected artifact targets, archive outputs (`zip`/`tar.gz`), emit checksums, optional GitHub upload.

2. **Publish** (`hwm release publish`)  
   Resolve publish groups, produce sdists, validate, topologically order by dependencies, upload to Hackage.

---

## 8. Correctness check vs previous architecture doc

The previous architecture document was **not fully correct** for current code. Main mismatches fixed here:

- mentioned obsolete commands (`outdated`, top-level `publish`)
- used old domain naming (`matrix`) instead of current `environments`
- omitted `release` command tree and task commands (`build/install/test`)
- implied flows and structures that no longer match current modules/types

This document is now aligned with current implementation.
