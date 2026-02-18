
# HWM: Feature Specification & Public API

**Target Audience:** AI Agents, Developers, External Integrators  
**Version:** 0.0.1  
**Last Updated:** February 15, 2026

## Executive Summary

HWM (Haskell Workspace Manager) is a declarative CLI tool for Haskell monorepos. It generates and syncs all project configs (`.cabal`, `stack.yaml`, `hie.yaml`) from a single `hwm.yaml` manifest, reducing 30+ files to 1 source of truth.

**Core Value:** Reduce 30+ config files to 1 declarative manifest.

**Target Audience:** This specification is for end users and AI agents working with HWM. It focuses on user-facing behavior, configuration, and CLI commands.


## Core Concepts

- **Workspace Groups:** Logical package collections (dirs, prefixes, publish flags)
- **Environments(Matrix):** Named GHC/resolver environments, script/env filters
- **Registry:** Central dependency bounds for all packages, managed and audited via unified `hwm registry` commands (add, audit, ls)
- **Scripts:** Template commands with `{TARGET}` substitution

## Comparison

| Feature         | HWM | Manual | Nix  |
| --------------- | --- | ------ | ---- |
| Single Config   | ✅  | ❌     | ❌   |
| Multi-GHC       | ✅  | ⚠️     | ✅   |
| Dependency Sync | ✅  | ❌     | ✅   |
| IDE Support     | ✅  | ⚠️     | ✅   |
| Publishing      | ✅  | ⚠️     | ❌   |
| Learning Curve  | Low | N/A    | High |


## Quick Reference

| Command                    | Synopsis                                               | Key Behavior                                                                 |
| -------------------------- | ------------------------------------------------------ | ---------------------------------------------------------------------------- |
| `hwm init [OPTIONS]`       | Generate `hwm.yaml` from Stack project                 | Scans stack files, infers packages/groups, writes manifest                   |
| `hwm sync [ENV]`           | Regenerate all config files for environment            | Updates stack.yaml, hie.yaml, package.yaml, cabal files                      |
| `hwm run SCRIPT [OPTIONS]` | Run custom scripts across envs/targets                 | Supports `--target`/`--env`, `{TARGET}` required if using `--target`         |
| `hwm registry add <pkg> <target>` | Add dependency to registry and inject into packages/groups | Discovers safe bounds, updates registry, syncs workspace |
| `hwm registry audit [--fix] [--force]` | Audit/fix registry bounds | Compares to Stackage LTS/Nightly, `--fix` updates errors, `--force` warnings |
| `hwm registry ls` | List all dependencies in the registry | Shows current bounds and status |
| `hwm version [BUMP]`       | Show or bump project version                          | `major`/`minor`/`patch`, updates bounds, propagates to all packages          |
| `hwm publish [GROUP]`      | Build & upload packages                               | For groups with `publish: true` or specified, runs sdist/upload              |
| `hwm status`               | Show project/env/workspace overview                   | Displays project name, version, envs, workspace structure                    |

**Global Flags:**  
`--version` (show CLI version), `--quiet` (suppress logs), `--help` (show help)

**Configuration Structure:**
```yaml
name: string         # Project identifier

## Registry Management

The `registry` is the central source of dependency version bounds for all packages in your workspace. HWM provides a unified set of commands under `hwm registry` to manage, audit, and list dependencies:

### hwm registry add <pkg> <target>

Adds a dependency to specific packages or entire groups while maintaining workspace consistency. HWM determines the appropriate version bounds by auditing the project's build matrix and external package sets.

#### Discovery & UI Logic

HWM adapts its output based on how it resolves the package version:

- **Already Registered:** If the package is already in the global `registry`, HWM reuses the existing bounds and displays them with an `(already registered)` tag.
- **Matrix Discovery:** If missing, HWM looks up the oldest (legacy) and newest (nightly) environments in your matrix to find the tested window.
- **Hackage Fallback:** If not found in Stackage, HWM queries Hackage for the latest preferred version as the upper bound.

**Examples:**

```bash
# Add to a specific package
hwm registry add aeson libs/core

# Add to an entire group
hwm registry add servant libs
```

#### Visual Examples

| Type | CLI Output Representation |
| --- | --- |
| **Existing** | `registry ....... >= 0.14.1 && <= 0.20.3.0 (already registered)` |
| **New (Full Matrix)** | `legacy ......... 0.14.1 (min)`<br>`nightly ........ 0.20.3.0 (max)` |
| **New (Hackage)** | `legacy ......... missing (min)`<br>`nightly ........ missing`<br>`hackage ........ 0.0.5 (max)` |

### hwm registry audit [--fix] [--force]

Audits all dependencies in the registry against the tested window defined by your build matrix (Stackage LTS and Nightly). Ensures your bounds are safe and up-to-date.

**Behavior:**

1. Clears version cache to ensure fresh snapshot data.
2. For each package in `registry`, fetches version info from the oldest LTS and latest Nightly.
3. Compares your bounds to the actual min/max available in these snapshots.
4. Reports errors (bounds too narrow), warnings (bounds too wide), or OK (fully covered).
5. If `--fix`, updates registry to match tested window (errors only by default).
6. If `--fix --force`, also fixes warnings.
7. Runs `hwm sync` to propagate changes.

**Options:**
- `-f, --fix`: Auto-update bounds and sync (errors only)
- `--force`: With --fix, also fixes warnings

**Examples:**

```bash
# Audit registry
hwm registry audit

# Auto-fix errors
hwm registry audit --fix

# Auto-fix errors and warnings
hwm registry audit --fix --force
```

**Output (Check Mode):**

```
• update dependencies
  mode .. check

• audit
  Glob                  >= 0.7.0     ->   0.10.1        &&    <  1.0.0       ->   0.10.2
  aeson                 >= 1.4.4     ->   1.5.6.0       &&    <  3.0.0       ->   2.2.3.0
  ...

• ▌ errors ▌

  • registry
  └─- • Found 11 outdated dependencies: Run 'hwm registry audit --fix --force' to update.
  └─- • Found 2 outdated dependencies: Run 'hwm registry audit --fix' to update.
```

**Output (Fix Mode):**

```
• update dependencies (auto-fix)
• registry ... megaparsec ↑ 9.7.0
• config ... hwm.yaml ✓
• workspace ... libs/core ⟳, libs/app ⟳, ...
```

**Updated Registry:**

```yaml
registry:
  - megaparsec  >= 7.0.0 && <= 9.7.0 # Updated from < 8.0.0
```

### hwm registry ls

Lists all dependencies in the registry, showing their current bounds and status.

**Example:**

```bash
hwm registry ls
```

**Output:**

```
registry:
  - aeson >= 2.0 && < 3.0
  - text  >= 2.0 && < 3.0
  ...
```
hwm outdated --fix --force
```

**Output (Check Mode):**


```
• update dependencies
  mode .. check

• audit
  Glob                  >= 0.7.0     ->   0.10.1        &&    <  1.0.0       ->   0.10.2
  aeson                 >= 1.4.4     ->   1.5.6.0       &&    <  3.0.0       ->   2.2.3.0
  ...

• ▌ errors ▌

  • registry
  └─- • Found 11 outdated dependencies: Run 'hwm outdated --fix --force' to update.
  └─- • Found 2 outdated dependencies: Run 'hwm outdated --fix' to update.
```

**Note:**
- `--fix` will only fix errors (bounds inside the matrix window).
- To also fix warnings (bounds outside the matrix window), use `--fix --force`.

**Output (Fix Mode):**


```
• update dependencies (auto-fix)
• registry ... megaparsec ↑ 9.7.0
• config ... hwm.yaml ✓
• workspace ... libs/core ⟳, libs/app ⟳, ...
```

**Updated Registry:**

```yaml
registry:
  - megaparsec  >= 7.0.0 && <= 9.7.0 # Updated from < 8.0.0
```

---

### hwm version [BUMP]

**Purpose:** Show or update project version

**Behavior:**

1. If no argument: Print version and exit
2. If bump specified:
   - Calculate new version (SemVer rules)
   - Update `version` and `bounds` in hwm.yaml
   - Propagate to all package.yaml files
   - Regenerate `.cabal` files

**Arguments:**

- `BUMP`: `major`, `minor`, or `patch`

**Version Bump Rules:**

```
major: X.Y.Z → (X+1).0.0
minor: X.Y.Z → X.(Y+1).0
patch: X.Y.Z → X.Y.(Z+1)
```

**Bounds Calculation:**

```
Version 0.28.0 → Bounds ">= 0.28.0 && < 0.29.0"
Version 1.2.3  → Bounds ">= 1.2.0 && < 1.3.0"
```

**Examples:**

```bash
# Show current version
hwm version
# Output: 0.28.0

# Bump patch: 0.28.0 → 0.28.1
hwm version patch

# Bump minor: 0.28.1 → 0.29.0
hwm version minor

# Bump major: 0.29.0 → 1.0.0
hwm version major
```

**Output:**

```
• bump version (minor)
  from .............. 0.28.4
  to ................ 0.29.0

• config
  hwm.yaml .......... ✓

./ workspace

  • libs
  └── core .............. ⟳
  └── app ............... ⟳
  └── client ............ ⟳
  └── server ............ ⟳

  • examples
  └── client ................. ⟳
  └── code-gen ............... ⟳

•  success
```

---

### hwm publish [GROUP]

**Purpose:** Build and upload packages to Hackage

**Behavior:**

1. Filters workspace groups:
   - If `GROUP` specified: Only that group
   - Otherwise: All groups with `publish: true`
2. For each package:
   - Runs `stack sdist {package}` (build source distribution)
   - Validates output (check for errors/warnings)
   - Runs `stack upload {package}` (publish to Hackage)
3. Stops on first error

**Prerequisites:**

- Hackage credentials: `~/.stack/upload/credentials.json`
- Clean builds (no sdist errors)
- Version must not exist on Hackage

**Arguments:**

- `GROUP` (optional): Workspace group name

**Examples:**

```bash
# Publish all groups with publish: true
hwm publish

# Publish specific group
hwm publish libs
```

**Output:**

```
• publish
  version ... 0.0.1
  target .... (all)
  registry .. hackage

./ workspace
  • libs
  └── core .............. ✓
  └── app ............... ✓

• success
```

---

### hwm status

**Purpose:** Display project state overview

**Behavior:**

1. Shows project name and version
2. Lists all build environments (marks active)
3. Shows workspace structure (groups → members)

**Examples:**

```bash
hwm status
```

**Output:**

```
• project
  name ..... morpheus-graphql
  version .. 0.29.0

• environments
  legacy (8.10.7) ... lts-18.10
  stable (9.6.6) .... lts-22.30 (active)
  nightly (9.10.3) .. lts-24.25

./ workspace

  • libs
  └── tests
  └── core
  └── code-gen-utils
  └── app
  └── subscriptions
  └── client
  └── server
  └── code-gen
  └── (root)

  • helpers
  └── benchmarks

  • examples
  └── client
  └── code-gen
  └── code-gen-docs
  └── scotty
  └── scotty-fraxl
  └── scotty-haxl
  └── scotty-freer-simple
  └── servant
  └── yesod-pubsub

•  success
---

## Workflow Examples

### Workflow 1: Initial Setup

**Scenario:** Adopt HWM in existing monorepo

```bash
# 1. Create hwm.yaml (see example above)

# 2. Initial sync
hwm sync

# 3. Verify generated files
ls stack.yaml hie.yaml
ls libs/*/package.yaml
ls libs/*/*.cabal

# 4. Test build
hwm run build
```

---

This new feature, **Smart Dependency Injection**, transforms HWM from a configuration synchronizer into an active package manager. Below is the chapter description for the `hwm add` feature, designed to be integrated into your **Feature Specification & Public API** document.



### hwm add <pkg> <target>

**Purpose:** Adds a dependency to specific packages or entire groups while maintaining workspace consistency. HWM determines the appropriate version bounds by auditing the project's build matrix and external package sets.

#### 🧩 Discovery & UI Logic

HWM adapts its output based on how it resolves the package version. This ensures transparency for new packages while remaining concise for existing ones.

##### Scenario 1: Already Registered

If the package is already defined in the global `registry`, HWM skips external lookups and reuses the existing source of truth.

* **UI:** Displays the current registry bounds with an `(already registered)` tag.

##### Scenario 2: Matrix Discovery

If the package is missing from the registry, HWM performs a lookup against the **Oldest (Legacy)** and **Newest (Nightly)** environments defined in your `matrix`.

* **UI:** Shows the specific versions found in each snapshot (e.g., `0.14.1 (min)` and `0.20.3.0 (max)`).
* **Missing in Legacy:** If a package is too new for the legacy environment, it marks it as `missing (min)`, prompting the user that the lower bound will be set by the next available environment.

##### Scenario 3: Hackage Fallback

If the package is not found in the Stackage Nightly snapshot, HWM reaches out to the **Hackage API**.

* **UI:** Adds a `hackage` line to the discovery section to show the latest preferred version used as the upper bound.

---

#### 🛠 Visual Examples

| Type | CLI Output Representation |
| --- | --- |
| **Existing** | `registry ....... >= 0.14.1 && <= 0.20.3.0 (already registered)` |
| **New (Full Matrix)** | `legacy ......... 0.14.1 (min)`<br>

<br>`nightly ........ 0.20.3.0 (max)` |
| **New (Hackage)** | `legacy ......... missing (min)`<br>

<br>`nightly ........ missing`<br>

<br>`hackage ........ 0.0.5 (max)` |

---

#### ⚙️ Execution Flow

1. **Dependency Injection:** Adds the package name to the relevant `package.yaml` files for all members in the `<target>` (group or specific package).
2. **Config Update:** Updates the `registry` and `hwm.yaml` file, ensuring the file hash is recalculated.
3. **Workspace Sync:** Triggers an implicit `hwm sync` to regenerate `.cabal` and `stack.yaml` files, ensuring the IDE and build tools immediately recognize the new dependency.

### Workflow 2: Multi-GHC Testing

**Scenario:** Test changes across GHC versions

```bash
# Run tests on all environments
hwm run test --env=all

# If failure, check logs
cat .hwm/logs/legacy.log

# Run on specific environments
hwm run test --env=stable,nightly
```

---

### Workflow 3: Dependency Update

**Scenario:** Update dependencies to latest

```bash
# Check for updates
hwm outdated

# Review proposed changes
# ...

# Apply updates
hwm outdated --fix

# Test changes
hwm run test --env=all
```

---

### Workflow 4: Release Process

**Scenario:** Coordinate release across packages

```bash
# 1. Bump version
hwm version minor

# 2. Final testing
hwm run test --env=all

# 3. Publish to Hackage
hwm publish libs

# 4. Tag release
git tag v0.29.0
git push --tags
```

---

## Resolution Logic

### Package Resolution

**Input:** WorkspaceGroup

```yaml
workspace:
  - name: libs
    prefix: morpheus-graphql
    dir: ./libs
    members: [core, server]
```

**Resolution:**

For each member:
- Directory: `{dir}/{prefix}-{member}` → `./libs/morpheus-graphql-core`
- Package name: Read from `package.yaml` `name` field
- Package ID: `{group}/{member}` → `libs/core`
- Package path: `./libs/morpheus-graphql-core`

### Target Resolution

**Syntax:**

- `group` → All members in group
- `group/member` → Specific member

**Examples:**

```bash
--target=libs           # All libs members
--target=libs/core      # Only libs/core
--target=libs --target=examples/scotty  # Multiple
```

### Environment Resolution

**Default:** `matrix.defaultEnvironment`

```bash
hwm sync              # Uses default
hwm sync legacy       # Explicit environment
```

**Matrix Execution:**

```bash
hwm run test --env=all              # All environments
hwm run test --env=legacy,stable    # Subset
```

---

## State Management

### State & Files Generated

| Path                           | Purpose                                                 |
| ------------------------------ | ------------------------------------------------------- |
| `.hwm/cache/state.json`        | Tracks current env + fetched versions                   |
| `.hwm/matrix/stack-<env>.yaml` | Per-environment stack files                             |
| `.hwm/logs/*.log`              | Captures async command output                           |
| `stack.yaml`                   | Mirrors active environment resolver                     |
| `hie.yaml`                     | Cradle referencing generated stack file                 |
| `*/package.yaml` + `*.cabal`   | Version/bounds updated per registry                     |

### Cache State (.hwm/cache/state.json)

**Purpose:** Track active environment and cache Hackage queries

**Structure:**

```json
{
  "currentEnv": "stable",
  "versions": {
    "aeson": ["1.4.4.0", "1.5.0.0", "2.0.0.0", "2.1.0.0", "2.2.0.0"],
    "text": ["1.2.3.0", "1.2.4.0", "2.0.0.0", "2.1.0.0"]
  }
}
```

**Operations:**

- Updated by: `hwm sync [ENV]`
- Cleared by: `hwm outdated` (force fresh queries)
- Read by: All commands (determines active environment)

### File Hash Integrity

**Purpose:** Detect manual edits to hwm.yaml

**Format:**

```yaml
# hash: {sha256-hex}
name: my-project
...
```

**Mechanism:**

1. On save: Compute SHA-256 of lines 2-N, write to line 1
2. On load: Verify hash matches
3. If mismatch: Treat as valid (allow manual edits)

---

## Error Handling

### Error Types

1. **Configuration Errors**
   - Invalid YAML syntax
   - Missing required fields
   - Invalid version format
   - Unknown environment name

2. **Resolution Errors**
   - Package not found at path
   - Workspace group not found
   - Script not defined
   - Invalid target syntax

3. **Build Errors**
   - Stack command failed
   - Cabal generation failed
   - HLS config invalid

4. **Network Errors**
   - Hackage API unreachable
   - Package not found on Hackage

### Error Output Format

```
✗ ERROR: {topic}

{message}

Details:
  File: {file-path}
  Command: {command}
  Log: {log-file}
```

### Logging

**Locations:**

- `.hwm/logs/{env}.log` - Command outputs
- `.hwm/logs/error-{timestamp}.log` - Error details

---

## Advanced Features

### Package Exclusion

**Use Case:** Exclude incompatible packages on specific GHC versions

```yaml
matrix:
  environments:
    - name: nightly
      ghc: 9.10.3
      exclude: [examples/old-api]
```

### Script Placeholders

**{TARGET} Replacement:**

```yaml
scripts:
  build: stack build {TARGET} --fast
```

**Usage:**

```bash
hwm run build --target=libs/core
# Executes: stack build morpheus-graphql-core --fast
```

**Global Scripts (no placeholder):**

```yaml
scripts:
  clean: find . -name "*.cabal" -exec rm -rf {} \; && stack clean
```

---

## Integration Points

### Stack

- Generates stack.yaml and environment-specific configs
- Executes: `stack build`, `stack test`, `stack sdist`, `stack upload`

### Cabal (via hpack)

- Updates package.yaml (version, dependencies)
- Generates `.cabal` files via hpack library

### HLS (Haskell Language Server)

- Generates hie.yaml with component mappings
- Format: `{pkg}:lib`, `{pkg}:test:{name}`, `{pkg}:exe:{name}`

### Hackage API

- Endpoint: `https://hackage.haskell.org/package/{pkg}/preferred`
- Returns: JSON list of available versions
- Cached in state.json

## Glossary

- **Workspace Group:** Logical collection of related packages
- **Build Environment:** GHC version + resolver + dependencies
- **Registry:** Centralized dependency version constraints
- **Target:** Package or group identifier (`group/member`)
- **Bounds:** Cabal version constraint (e.g., `>= 1.0 && < 2.0`)
- **Matrix:** Collection of build environments for testing