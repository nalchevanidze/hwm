# HWM: Feature Specification & Public API (Detailed)

**Target Audience:** AI agents, developers, external integrators  
**Implementation Baseline:** current `main` code in this repository  
**Spec Version:** 0.2.x aligned  
**Last Updated:** 2026-03-24

---

## 1) What HWM is

HWM (Haskell Workspace Manager) is a declarative orchestration CLI for Haskell monorepos.

You define project intent in `hwm.yaml`, and HWM keeps generated/toolchain files aligned, including:

- `cabal.project`
- `stack.yaml` (when enabled by environment settings)
- `flake.nix` (when enabled by environment settings)
- `hie.yaml` (when enabled by environment settings)
- package metadata synchronization (`package.yaml`/`.cabal` flow)

HWM is designed to sit above native tooling (Cabal/Stack/Nix/HLS) rather than replace it.

---

## 2) Global CLI behavior

### 2.1 Binary and global flags

```bash
hwm [GLOBAL_FLAGS] <COMMAND>
```

Global flags:

- `-v, --version` — print CLI version and exit
- `-q, --quiet` — suppress non-essential UI output
- `--help` — standard usage/help

### 2.2 Command fallback

If the first token is not a known top-level command, HWM treats it as a script name:

```bash
hwm <SCRIPT> [ARGS...]
```

Equivalent to:

```bash
hwm run <SCRIPT> [ARGS...]
```

Transparency note:

- this fallback also catches command typos
- typoed commands may surface as script resolution errors

---

## 3) Top-level commands (implemented)

- `hwm init [--force] [NAME]`
- `hwm status`
- `hwm sync [ENV]`
- `hwm run <SCRIPT> [ARGS...]`
- `hwm workspace <subcommand>`
- `hwm environments <subcommand>`
- `hwm registry <subcommand>`
- `hwm version [major|minor|patch|X.Y.Z]`
- `hwm build [--env ENV]... [WORKSPACE...] [--fast]`
- `hwm install [--env ENV]... [WORKSPACE...] [--fast]`
- `hwm test [--env ENV]... [WORKSPACE...] [--fast]`
- `hwm release <subcommand>`

> Important naming detail: the command is **`environments`** (plural), not `environment`.

---

## 4) Command reference

## 4.1 `hwm init`

### Syntax

```bash
hwm init [--force] [NAME]
```

### Behavior

- Scans current directory for stack files and Haskell packages
- Infers workspace structure and initial dependency registry
- Writes `hwm.yaml`
- If `NAME` is omitted, project name defaults to current directory name
- If `hwm.yaml` exists, command fails unless `--force` is set
- After initialization, command flow shows status

### Notable failure

- If no packages are discovered, init exits with a configuration error.

---

## 4.2 `hwm status`

### Syntax

```bash
hwm status
```

### Behavior

Shows:

- project name and version
- environments table (active/default context)
- workspace/package validation summary

---

## 4.3 `hwm sync`

### Syntax

```bash
hwm sync [ENV]
```

### Behavior

- Resolves build environment (`ENV`, or cached/default)
- Updates cache current environment
- Regenerates enabled config files for that environment:
  - `cabal.project`
  - `stack.yaml` (if environment stack feature enabled)
  - `flake.nix` (if environment nix feature enabled)
  - `hie.yaml` (if environment hie feature enabled)
- Syncs package files (`syncPackages` pipeline)

Current implementation note:

- generated `hie.yaml` uses stack cradle format and is not yet fully builder/profile-specific

---

## 4.4 `hwm run`

### Syntax

```bash
hwm run <SCRIPT> [ARGS...]
```

### Behavior

- Looks up `<SCRIPT>` in `scripts` map in `hwm.yaml`
- Executes script command with inherited stdio
- Passes `[ARGS...]` to the shell invocation as positional arguments
- Errors if script name does not exist

> Notes:
> - script commands that need forwarded args must consume shell positional parameters explicitly
> - execution path is shell-based (`/bin/sh -c ...`) in current implementation

---

## 4.5 `hwm build`, `hwm install`, `hwm test`

### Syntax

```bash
hwm build   [--env ENV]... [WORKSPACE...] [--fast]
hwm install [--env ENV]... [WORKSPACE...] [--fast]
hwm test    [--env ENV]... [WORKSPACE...] [--fast]
```

### Environment selection

- `--env all` => all configured environments
- repeated `--env` and comma-separated values are both supported
- no `--env` => active/default environment only

### Workspace/target selection

`WORKSPACE` positional targets support:

- `group` (all members)
- `group/member` (specific member)

If omitted, scope is global.

### `--fast` behavior

- Stack: forwards `--fast`
- Cabal: forwards `--disable-optimization`
- Nix: build flags are limited/ignored where not supported

### Builder compatibility notes

- Builder can be `cabal`, `stack`, `nix`, or `nix/cabal`
- `install` is not supported by `nix`
- `install` is not supported by `nix/cabal` mode
- unsupported install combinations fail fast with an explicit error

---

## 4.6 `hwm workspace`

### Subcommands

```bash
hwm workspace add <group> [--dir DIR] [--prefix PREFIX]
hwm workspace add <group/member>
hwm workspace ls
```

### Behavior

- `add <group>`: creates a new workspace group
- `add <group/member>`: adds new member package and scaffolds files
- member add triggers related config sync steps (notably stack/hie)
- `workspace ls` runs package validation/reporting

### Option applicability

`--dir` and `--prefix` are meaningful for group creation; they do not affect member-only add.

---

## 4.7 `hwm environments`

### Subcommands

```bash
hwm environments add <NAME> <GHC_VERSION>
hwm environments remove <NAME>
hwm environments set-default <NAME>
hwm environments ls
```

### Important

Environment creation currently expects an explicit **GHC version**, not resolver text.

### Behavior summary

- `add`: adds new profile if name does not exist
- `remove`: removes profile by name
- `set-default`: switches `environments.default`
- `ls`: prints environment list and active/default context

---

## 4.8 `hwm registry`

### Subcommands

```bash
hwm registry add <PACKAGE> [--workspace WORKSPACE]...
hwm registry audit [--fix] [--force]
hwm registry ls [--search TEXT]
```

### `registry add`

- If package already exists in registry, existing bounds are reused
- If missing, bounds are derived from tested range and added
- If `--workspace` targets are provided, dependency is injected into those packages
- if no workspace is provided, update is registry-only

### `registry audit`

- Compares registry bounds against tested snapshot range
- Reports conflicts/warnings
- `--fix` updates errors
- `--fix --force` updates broader warning set too
- syncs package files after successful auto-fix

### `registry ls`

- Lists all registry dependencies
- optional substring filter via `--search`

---

## 4.9 `hwm version`

### Syntax

```bash
hwm version
hwm version major
hwm version minor
hwm version patch
hwm version X.Y.Z
```

### Behavior

- no argument => prints current version and exits
- bump/fixed version => updates `version` in config and syncs packages
- warns (does not hard-fail) when setting a lower version

---

## 4.10 `hwm release`

### Subcommands

```bash
hwm release artifacts [TARGET]
hwm release publish [GROUP]
```

### `release artifacts` options

```bash
--github
--output-dir <DIR>
--format <csv>
--ghc-options <csv>
--name-template <TEMPLATE>
--builder <cabal|stack|nix|nix/cabal>
```

Notes:

- `TARGET` selects one artifact key; omitted means all configured artifacts
- `--format` and `--ghc-options` are comma-separated lists
- command builds binaries, archives them, and emits `.sha256` files
- with `--github`, uploads archive and checksum assets to GitHub release URL
- current limitation: `release artifacts --builder=nix` is not supported and fails with an explicit error
- current limitation: artifact config field `environments` is not yet enforced by command dispatch (active/default env is used)

### `release publish`

- publishes groups configured under `release.publish`
- performs source dist checks and topological publish ordering
- target argument selects one publish group; omitted publishes all configured groups
- publishing is Cabal `sdist` based by design (builder-independent, Hackage-oriented)

> Important: there is no top-level `hwm publish`; publishing is under `hwm release publish`.

---

## 5) `hwm.yaml` schema (current model)

Top-level fields used by current implementation:

- `name: Text`
- `version: Version`
- `workspace: Workspace`
- `environments: Environments`
- `github: Text?`
- `bounds: Bounds?`
- `registry: Registry?`
- `scripts: Map Text Text` (optional map; values are non-null strings)
- `release: Release?`

### 5.1 Workspace

```yaml
workspace:
  libs:
    dir: ./libs            # optional
    prefix: my-prefix      # optional
    members: [core, app]   # required
```

Workspace target syntax in CLI:

- `libs`
- `libs/core`

### 5.2 Environments

```yaml
environments:
  default: stable
  builder: stack           # optional global default
  stack: true              # optional global toggle
  nix: false               # optional global toggle
  hie: true                # optional global toggle
  profiles:
    stable:
      ghc: 9.6.3
      builder: cabal       # optional per-profile override
      exclude: [libs/old]  # optional
      stack:               # optional; can also be true/false
        resolver: lts-22.43
        allow-newer: false
        extra-deps:
          some-pkg: 1.2.3
```

### 5.3 Registry

Registry maps package names to bounds used for dependency sync/audit.

```yaml
registry:
  aeson: ">= 2.0 && < 3.0"
  text: ">= 2.0 && < 3.0"
```

### 5.4 Scripts

```yaml
scripts:
  lint: hlint .
  test-fast: hwm test --fast
```

### 5.5 Release

```yaml
release:
  artifacts:
    hwm:
      source: libs/_root_:hwm
      environments: [ci-cabal]
      formats: [zip, tar.gz]
      ghc-options: [-O2, -threaded, -split-sections]
      name-template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"
  publish:
    main:
      - libs/_root_
```

---

## 6) State and persistence

HWM runtime state/cache is stored under `.hwm/` (cache/log/artifact related internals).

Config persistence behavior:

- `hwm.yaml` is rewritten by mutating commands
- a header hash line is maintained (`# hash: ...`)
- config checks run when environment signature changes are detected

---

## 7) Error model (user-facing)

Primary error categories:

- configuration errors (bad YAML, missing/invalid values)
- resolution errors (unknown workspace/env/script/target)
- build/publish execution failures
- network failures (snapshot, hackage, github operations)

Typical behavior:

- command summary is printed
- issues are aggregated and emitted
- fatal failures terminate with non-zero exit

---

## 8) Known naming/compatibility/behavior clarifications

- Use `hwm environments ...` (plural).
- Use `hwm release publish ...`, not `hwm publish`.
- Use `hwm registry audit ...`, not `hwm outdated`.
- `environments add` currently takes `<GHC_VERSION>` directly.
- Unknown top-level command tokens are treated as script names (`hwm run ...` fallback).
- `hwm run` is shell-based and argument forwarding uses shell positional parameters.
- Generated files are managed artifacts and may be rewritten on `hwm sync`.
- Install with `nix`/`nix/cabal` is currently unsupported and fails with explicit errors.
- `release publish` is Cabal-`sdist` based by design (builder-independent, Hackage-oriented).
- environment remove flow does not yet guard against deleting the default/last profile; this can leave config in an invalid state until fixed.

---

## 9) Quick examples

### Initialize and sync

```bash
hwm init
hwm sync
hwm status
```

### Add environment and run tests across all

```bash
hwm environments add stable-98 9.8.2
hwm test --env=all
```

### Add dependency and audit bounds

```bash
hwm registry add aeson --workspace libs/core
hwm registry audit
```

### Bump version and publish release group

```bash
hwm version minor
hwm release publish main
```
