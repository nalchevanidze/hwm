# HWM: Feature Specification & Public API

**Target Audience:** AI Agents, Developers, External Integrators
**Version:** 0.0.1
**Last Updated:** February 15, 2026

## Executive Summary

HWM (Haskell Workspace Manager) is a declarative CLI tool that replaces manual configuration maintenance in Haskell monorepos. It generates and synchronizes `.cabal`, `stack.yaml`, and `hie.yaml` files from a single `hwm.yaml` source of truth.

**Core Value:** Reduce 30+ config files to 1 declarative manifest.

**Target Audience:** This specification is for end users and AI agents working with HWM. It focuses on user-facing behavior, configuration, and CLI commands.

---

## Core Concepts

| Concept          | Description                                                                   |
| ---------------- | ----------------------------------------------------------------------------- |
| Workspace Groups | Logical collections that resolve package directories, prefixes, publish flags |
| Build Matrix     | Named GHC/resolver environments + filters for scripts/sync                    |
| Registry         | Central dependency bounds shared across packages                              |
| Bounds Auditing  | Ensures dependency bounds are safe: not inside matrix window, warns if outside|
| Scripts          | Template commands with `{TARGET}` substitution                                |

---

## Quick Reference

### CLI Commands

| Command                    | Synopsis                                               | Key Behavior                                                                 |
| -------------------------- | ------------------------------------------------------ | ---------------------------------------------------------------------------- |
| `hwm init [OPTIONS]`       | Generate `hwm.yaml` from existing Stack project        | Scans stack files, discovers packages, infers config, creates hwm.yaml       |
| `hwm sync [ENV]`           | Regenerate `stack.yaml`, `hie.yaml`, package manifests | Uses Matrix default when ENV missing; updates registry current env           |
| `hwm run SCRIPT [OPTIONS]` | Execute custom script against env/targets              | Resolves env YAMLs, parallel when multiple envs, enforces `{TARGET}` rules   |
| `hwm outdated [--fix]`     | Detect or fix registry upper bounds                    | Queries Hackage for latest versions; `--fix` rewrites config + syncs packages |
| `hwm version [BUMP]`       | Show or bump semantic version                          | Recomputes bounds after bump; quiet mode prints only when not `--quiet`      |
| `hwm publish [GROUP]`      | Build & upload packages                                | Runs `stack sdist` then `stack upload`; honors workspace group filter        |
| `hwm status`               | Current version, envs, workspace tree                  | Displays project overview with formatted sections                            |

### Global Flags

- `--version` - Display CLI app version
- `--quiet` - Suppress non-essential logs
- `--help` - Show help (per optparse-applicative)`

### Configuration Structure

```yaml
name: string # Project identifier
version: string # SemVer (e.g., "0.1.0")
bounds: string # Auto-generated (">= 0.1.0 && < 0.2.0")
workspace: [WorkspaceGroup] # Package groups
matrix: Matrix # Build environments
registry: [Dependency] # Version constraints
scripts: { name: command } # Custom commands
```

---

## Configuration Schema (hwm.yaml)

### Complete Type Definitions

```typescript
// Root Configuration
type Config = {
  name: string; // Project name
  version: Version; // SemVer string
  bounds: Bounds; // Auto-generated bounds
  workspace: WorkspaceGroup[]; // Package groups
  matrix: Matrix; // Build environments
  registry: Dependency[]; // Global dependencies
  scripts?: Record<string, string>; // Custom commands
};

// Workspace Group
type WorkspaceGroup = {
  name: string; // Required: Group identifier
  dir?: string; // Optional: Base directory (default: "./")
  prefix?: string; // Optional: Package name prefix
  members: string[]; // Required: Member names
  publish?: boolean; // Optional: Hackage publish flag (default: false, auto-detected for examples/benchmarks)
};

// Build Matrix
type Matrix = {
  defaultEnvironment: string; // Required: Default env name
  environments: BuildEnv[]; // Required: List of environments
};

// Build Environment
type BuildEnv = {
  name: string; // Required: Environment identifier
  ghc: Version; // Required: GHC version
  resolver: string; // Required: Stack resolver
  extraDeps?: Record<string, Version>; // Optional: Extra dependencies
  exclude?: string[]; // Optional: Package IDs to exclude
  allowNewer?: boolean; // Optional: Enable --allow-newer
};

// Dependency Specification
type Dependency = string; // Format: "pkg-name >= X.Y && < Z.W"

// Bounds Safety:
// - HWM audits dependency bounds using the oldest environment in the matrix and the latest Stackage nightly.
// - Bounds must not be inside the matrix window (between oldest and newest tested envs); if so, an error is raised.
// - If bounds are outside the matrix window, a warning is shown (may break on untested versions).

// Version (SemVer)
type Version = string; // Format: "X.Y.Z" or "X.Y.Z.W"

// Bounds (Cabal constraint)
type Bounds = string; // Format: ">= X.Y.Z && < X.Y.Z"
```

### Validation Rules

1. **Version Format:** Must be SemVer (1-4 numeric segments)
2. **Bounds Generation:** Auto-computed as `>= X.Y.0 && < X.(Y+1).0`
3. **Workspace Members:** Each member must have package.yaml at resolved path
4. **Default Environment:** Must exist in `matrix.environments[].name`
5. **Extra Deps:** Package names must exist on Hackage
6. **Exclude Format:** Use `{group}/{member}` or `{group}` syntax
7. **File Hash:** First line contains SHA-256 hash for integrity check

### Example Configuration

```yaml
# hash: e7b77bc2760df12d8332ef151b6fe7570d382a3b26df1c3274bf1a53e0c1632f
name: morpheus-graphql
version: 0.28.0
bounds: ">= 0.28.0 && < 0.29.0"

workspace:
  - name: libs
    prefix: morpheus-graphql
    members: [core, server, client, subscriptions]
    publish: true

  - name: examples
    dir: examples
    members: [scotty, servant, websockets]
    publish: false

matrix:
  default-environment: stable
  environments:
    - name: legacy
      ghc: 8.10.7
      resolver: lts-18.10
      extra-deps:
        aeson: 1.4.4.0
        relude: 0.3.0
      exclude: [examples/websockets]

    - name: stable
      ghc: 9.6.3
      resolver: lts-22.6
      extra-deps:
        fastsum: 0.1.1.1

    - name: nightly
      ghc: 9.10.3
      resolver: nightly-2024-01-15
      allow-newer: true

registry:
  - aeson                 >= 1.4.4   && < 3.0.0
  - text                  >= 1.2.3   && < 3.0.0
  - mtl                   >= 2.0.0   && < 3.0.0
  - bytestring            >= 0.10.4  && <= 0.12.2.0
  - containers            >= 0.4.2.1 && <= 0.8

scripts:
  build: stack build {TARGET} --fast
  test: stack test {TARGET}
  install: stack install
  lint: curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
  clean: find . -name "*.cabal" -exec rm -rf {} \; && stack clean && echo "Cleaned build artifacts."
```

---

## CLI Command Reference

### 0. hwm init [OPTIONS]

**Purpose:** Bootstrap HWM by generating `hwm.yaml` from existing Stack project structure. Zero-config onboarding - transform existing Stack projects into HWM-managed workspaces in seconds.

**Exit Codes:**
- `0` - Success, hwm.yaml created
- `1` - No stack.yaml found
- `2` - hwm.yaml already exists (use --force to overwrite)
- `3` - Invalid project structure

**Behavior:**

1. **Checks preconditions:**
   - Verifies `hwm.yaml` doesn't exist (or `--force` used)
   - Ensures `stack.yaml` exists in current directory

2. **Discovers Stack configurations:**
   - Scans for `stack.yaml` and `stack-*.yaml` files in root directory
   - Extracts resolver, packages, extra-deps, allow-newer
   - Resolves GHC versions from resolver names (LTS/Nightly mappings)
   - Environment naming: `stack-legacy.yaml` → "legacy", `stack-nightly.yaml` → "nightly"
   - Default environment: first alphabetically or resolver-based name

3. **Analyzes packages:**
   - Reads `package.yaml` or `*.cabal` from each package path
   - Extracts package name, version, dependencies
   - Groups packages by directory structure patterns

4. **Package grouping heuristics:**
   - Group by common directory prefix
   - Directory patterns:
     - `libs/*` → group name "libs"
     - `apps/*` → group name "apps"
     - `examples/*` → group name "examples", publish: false
     - `test/*` → group name "test", publish: false
   - Root-level packages → "default" group
   - Root package (`.`) → uses repository directory name as member

5. **Infers project metadata:**
   - **Project name:** Single package → use package name; Multiple → use common prefix or repository directory
   - **Project version:** Use root package version, or most common version across packages, or default to "0.1.0"
   - Warns if packages have inconsistent versions

6. **Registry inference:**
   - Collects all dependencies from package.yaml files
   - Queries Hackage for latest version of each unique package
   - Generates conservative bounds: `>= X.Y && < (X+1).0`
   - Merges with extra-deps from stack files
   - Sorted alphabetically

7. **Generates and writes `hwm.yaml` with computed hash**

**Options:**

- `--force` - Overwrite existing hwm.yaml
- `[NAME]` - Optional project name (defaults to current directory name)

**Usage:**
```bash
hwm init                    # Use directory name as project name
hwm init my-project         # Specify custom project name
hwm init --force            # Overwrite existing configuration
```

**Examples:**

```bash
# Generate hwm.yaml from current Stack project
hwm init

# Overwrite existing configuration
hwm init --force
```

**Success Output:**

```
> hwm init

• init
  ├─ • scanning ........ stack.yaml 
  ├─ • scanning ........ stack-legacy.yaml
  ├─ • packages ........ 8 found
  └─ • generating ...... hwm.yaml 

• project
  name ..... morpheus-graphql
  version .. 0.28.0

• environments
  legacy (8.10.7) ... lts-18.10
  stable (9.6.6) .... lts-22.30 (active)

./ workspace

  • libs
  ├── core 
  ├── server
  ├── client
  └── subscriptions

  • examples
  ├── scotty
  └── servant

• registry
  dependencies .. 24 inferred

  success
```

**Error Cases:**

```
# No stack.yaml found
• init
  └─ • scanning ........ ✖

•  errors
  • No stack.yaml found in current directory
```
# hwm.yaml already exists
• init
  └─ • detecting ....... hwm.yaml exists

•  errors
  • Configuration already exists: hwm.yaml
  └─ Use --force to overwrite or 'hwm sync' to validate

  failed
```

  └─ Run 'stack init' first or ensure you're in a Stack project

  failed
```

**Edge Cases:**

- **Multiple versions across packages:** Uses most common version, warns about inconsistency
- **No common prefix:** Creates single "default" workspace group
- **Root package (`.`):** Uses repository directory name as member name
- **Non-standard environment names:** `stack-my-custom-build.yaml` → "my-custom-build"
- **Missing GHC version:** Tries Stack API, falls back to parsing resolver field
- **Conflicting extra deps:** Keeps per-environment in matrix, doesn't add to registry

---

### 1. hwm sync [ENV]

**Purpose:** Regenerate all toolchain configuration files

**Behavior:**

1. Loads hwm.yaml and validates schema
2. Resolves build environment (default or specified)
3. Generates files:
   - stack.yaml - Active environment configuration
   - `.hwm/matrix/stack-{env}.yaml` - Per-environment configs
   - hie.yaml - HLS IDE configuration
   - package.yaml - Updates dependencies and version
   - `{package}/{package}.cabal` - Generated via hpack
4. Updates state.json with active environment

**Arguments:**

- `ENV` (optional): Environment name from `matrix.environments[].name`

**Examples:**

```bash
hwm sync              # Use default environment
hwm sync legacy       # Switch to legacy environment
hwm sync nightly      # Switch to nightly environment
```

**Output:**

```
• sync
  enviroment .. stable (9.6.6)
  resolver .... lts-22.30

• config
  stack.yaml .. ✓
  hie.yaml .... ✓

./ workspace

  • libs
  └── core .............. ✓
  └── app ............... ✓
  └── server ............ ✓
  └── (root) ............ ✓

  • examples
  └── client ................. ✓
  └── scotty-fraxl ........... ! cabal
  └── scotty-haxl ............ ✓
  └── scotty-freer-simple .... ! cabal

•  warnings
  • scotty-fraxl
  ├─ • Invalid package: A 'license-file' is not specified.
  ├  └─ file: examples/scotty-fraxl/morpheus-graphql-examples-scotty-fraxl.cabal
  • scotty-freer-simple
  ├─ • Invalid package: A 'license-file' is not specified.
  ├  └─ file: examples/scotty-freer-simple/morpheus-graphql-examples-scotty-freer.cabal
```

**Generated stack.yaml:**

```yaml
resolver: lts-22.6
packages:
  - libs/morpheus-graphql-core
  - libs/morpheus-graphql-server
extra-deps:
  - fastsum-0.1.1.1
allow-newer: false
save-hackage-creds: false
```

**Generated hie.yaml:**

```yaml
cradle:
  stack:
    stackYaml: stack.yaml
    components:
      - path: ./libs/morpheus-graphql-core/src
        component: "morpheus-graphql-core:lib"
      - path: ./libs/morpheus-graphql-core/test
        component: "morpheus-graphql-core:test:spec"
```

---

### 2. hwm run SCRIPT [OPTIONS] [ARGS...]

**Purpose:** Execute custom scripts across build matrix with target filtering

**Behavior:**

1. Resolves script from `scripts` section
2. Validates `{TARGET}` placeholder:
   - Script has `{TARGET}` + no targets specified → ERROR
   - Script lacks `{TARGET}` + targets specified → ERROR
3. Creates environment YAML files
4. Executes command:
   - **Multi-env:** Parallel execution, captured output, spinner UI
   - **Single-env:** Inherited terminal, live output

**Options:**

- `-t, --target=TARGET`: Limit to package(s) (format: `group/member` or `group`)
- `-e, --env=ENV`: Run in environment(s) (comma-separated or `all`)
- `ARGS...`: Forward arguments to script (after `--`)

**Target Resolution:**

- `group` → All members in group
- `group/member` → Specific member
- Multiple: `--target=libs/core --target=libs/server`

**Examples:**

```bash
# Run build on default environment
hwm run build

# Run tests on all environments
hwm run test --env=all

# Run build for specific packages
hwm run build --target=libs/core --target=libs/server

# Run with environment selection
hwm run test --env=legacy,stable

# Forward arguments to script
hwm run bench --target=libs/core -- --timeout=60s
```

**Script Placeholder Rules:**

```yaml
scripts:
  # Requires --target flag
  build: stack build {TARGET} --fast

  # Global only (no --target allowed)
  clean: find . -name "*.cabal" -exec rm -rf {} \; && stack clean && echo "Cleaned build artifacts."
```

**Output (Single Environment):**

```
./ workspace
  targets ......... libs/core, libs/server

• environments
  stable (9.6.6)

❯ stack build morpheus-graphql-core morpheus-graphql-server --fast
[... live terminal output ...]
```

**Output (Multiple Environments):**

```
./ workspace
  targets ......... None (Global Scope)

• environments
  legacy (8.10.7) ..... ✖
  stable (9.6.6) ...... ✓
  nightly (9.10.3) .... ✓

•  failures
  • legacy
  ├─ • Command failed: stack build --fast
  ├  └─ logs: .hwm/logs/legacy.log
```

---

### 3. hwm outdated [--fix]


**Purpose:**
Detect and fix dependency version bounds in your registry by comparing them against the actual package sets provided by Stackage LTS (for your oldest tested environment) and the latest Stackage Nightly snapshot. This ensures that your declared bounds are both safe and meaningful: all supported versions are tested, and you are not allowing versions that could break your build.

**How It Works & Why:**

it fetches the exact package sets from Stackage snapshots:

- **Oldest LTS**: Represents the minimum version of each dependency that your build matrix tests. This is the lower bound of your "supported window".
- **Latest Nightly**: Represents the maximum version of each dependency that is available and can be tested. This is the upper bound of your "supported window".

By comparing your registry bounds to these two snapshots, HWM can:
- Guarantee that every version you claim to support is actually tested in CI (no false sense of safety).
- Prevent accidental breakage from untested versions (e.g., if you allow a version newer than Nightly, you risk breakage as soon as it is released).
- Warn you if your bounds are too restrictive (excluding versions that are tested and available).

**Behavior:**

1. Clears version cache (state.json) to ensure fresh snapshot data.
2. For each package in `registry`:
  - Fetches version information from the oldest Stackage LTS and the latest Stackage Nightly snapshot (e.g., https://github.com/commercialhaskell/stackage-snapshots/blob/master/nightly/2026/2/17.yaml).
  - Compares your declared lower and upper bounds to the actual minimum and maximum versions available in these snapshots.
3. Audits each dependency's bounds:
  - **Error:** If your bounds are inside the matrix window (i.e., you allow only a subset of the tested versions), this is considered dangerous: you may miss regressions or breakages. HWM will raise an error and require you to fix it.
  - **Warning:** If your bounds are outside the matrix window (i.e., you allow versions not tested by any environment), this is risky: you may claim support for versions that are not actually tested. HWM will warn you, but not block you by default.
  - **OK:** If your bounds exactly match the matrix window (from oldest LTS to latest Nightly), your registry is safe and fully covered by your build matrix.
4. Reports all outdated, unsafe, or misaligned dependencies in a clear audit table.
5. If `--fix` is used:
  - Updates your registry in hwm.yaml to match the tested window (fixing only errors by default).
  - Runs `hwm sync` to propagate changes to all generated files.
6. If `--fix --force` is used:
  - Also fixes warnings, expanding or contracting your bounds to exactly match the tested window.

**Options:**

- `-f, --fix`: Auto-update bounds and sync (only errors: bounds inside matrix window)
- `--force`: With --fix, also fixes warnings (bounds outside matrix window)

**Note:**
Hackage preferred versions are no longer used for auditing or updating bounds. All dependency version checks are performed using Stackage LTS and Nightly snapshots only. This guarantees that your registry reflects only what is actually tested in your build matrix.

**Examples:**

```bash
# Check for updates and audit bounds
hwm outdated

# Auto-apply updates (fix errors only)
hwm outdated --fix

# Auto-apply updates (fix errors and warnings)
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
• update dependencies
  mode .. auto-fix

• registry
  megaparsec .... ↑ 9.7.0

• config
  hwm.yaml .. ✓

./ workspace

  • libs
  └── tests ............. ✓
  └── core .............. ⟳
  └── code-gen-utils .... ✓
  └── app ............... ⟳
  └── subscriptions ..... ✓

# more sync output ...
```

**Updated Registry:**

```yaml
registry:
  - megaparsec  >= 7.0.0 && <= 9.7.0 # Updated from < 8.0.0
```

---

### 4. hwm version [BUMP]

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

### 5. hwm publish [GROUP]

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

### 6. hwm status

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



### 7. hwm add <pkg> <target>

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

---

## Comparison

| Feature         | HWM | Manual | Nix  |
| --------------- | --- | ------ | ---- |
| Single Config   | ✅  | ❌     | ❌   |
| Multi-GHC       | ✅  | ⚠️     | ✅   |
| Dependency Sync | ✅  | ❌     | ✅   |
| IDE Support     | ✅  | ⚠️     | ✅   |
| Publishing      | ✅  | ⚠️     | ❌   |
| Learning Curve  | Low | N/A    | High |

---

## Glossary

- **Workspace Group:** Logical collection of related packages
- **Build Environment:** GHC version + resolver + dependencies
- **Registry:** Centralized dependency version constraints
- **Target:** Package or group identifier (`group/member`)
- **Bounds:** Cabal version constraint (e.g., `>= 1.0 && < 2.0`)
- **Matrix:** Collection of build environments for testing

---

**Document Version:** 1.0  
**HWM Version:** 0.0.1  
**Status:** Complete

---

## Related Documentation

For detailed architecture information, module structure, and codebase internals, see:
- **[Architecture Documentation](architecture.md)** - Complete guide for maintainers and AI agents
