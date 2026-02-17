# HWM: Feature Roadmap

**Target Audience:** Contributors, Maintainers, Planning

**Last Updated:** February 15, 2026

This document outlines potential features and enhancements that could benefit HWM users. These are not committed features, but rather ideas for future consideration.

---

## Planned Enhancements

### 1. New Project Command (`hwm init --new`)

Add a command to bootstrap a fresh HWM project in a new directory, creating all necessary "glue" files for a hybrid workflow (Cabal + Stack + Nix).

#### Proposed Syntax
```bash
hwm init --new <project-name> [options]
```

#### Features

* **Directory Creation:** Creates `<project-name>/` directory.
* **Scaffolding:** Generates `hwm.yaml`, `cabal.project`, `stack.yaml`, and `flake.nix`.
* **Git Initialization:** Runs `git init` and generates a comprehensive `.gitignore`.
* **Hybrid Setup:** Ensures the generated Stack resolver matches the Nix compiler version.

#### Options

* `--template=NAME`: Use a specific starter template (lib, exe, servant, etc.)
* `--interactive`: Prompt for details instead of using defaults.

---

### 2. Init Command Options

Enhance `hwm init` (for existing directories) with additional configuration options.

#### Proposed Options


**`--name=NAME`**
* Override inferred project name.
* Use case: When the directory name doesn't match the desired project name.
* Example: `hwm init --name=my-awesome-project`


**`--version=VERSION`**
* Override inferred project version.
* Use case: Set a specific starting version instead of auto-detection.
* Example: `hwm init --version=1.0.0`


**`--default-env=ENV`**
* Set which environment becomes the matrix default.
* Use case: Control which resolver/GHC is used by default.
* Example: `hwm init --default-env=stable`


**`--no-scripts`**
* Don't generate the default scripts section.
* Use case: Clean, minimal config for a custom workflow.
* Example: `hwm init --no-scripts`


**`--dry-run`**
* Preview generated configuration without writing files.
* Use case: Validate inference before committing.
* Example: `hwm init --dry-run`

---

### 3. Version Command - Direct Set


Allow `hwm version` to accept specific version numbers in addition to bump types.

#### Proposed Syntax
```bash
hwm version 2.0.0       # Set to specific version
hwm version 1.5.2       # Jump to arbitrary version
```

#### Implementation Notes

* Extend `data Bump = Major | Minor | Patch | Set Version`.
* Parse version string as an alternative to bump keyword.
* Validate SemVer format.
* Warn on non-monotonic version changes.

#### Benefits

* Align with external version requirements.
* Support version resets or corrections.
* Better integration with release management tools.

---

### 4. Interactive Init Mode


Add interactive prompts when running `hwm init` to guide users through configuration.

#### Proposed Flow

```text
> hwm init --interactive

🚀 HWM Workspace Initialization

Project name (morpheus-graphql): [enter]
Project version (0.28.0): [enter]
Default environment (stable): [enter]
Infer registry from Hackage? (y/n): y
Generate default scripts? (y/n): y

✓ Configuration complete!
Preview:
  - 2 workspace groups (libs, examples)
  - 3 environments (legacy, stable, nightly)
  - 24 registry entries


Write to hwm.yaml? (y/n):

```

---

### 5. Init from Template


Support initialization from predefined templates (usable by both `new` and `init`).

```bash
hwm init --template=basic        # Minimal single-package
hwm init --template=monorepo     # Multi-package with examples
hwm init --template=library      # Public library defaults

```

#### Templates Could Include
* Workspace structure patterns
* Common script definitions
* Registry presets (e.g., popular packages)
* CI/CD integration files

---

### 6. Environment Management Commands


Add dedicated commands for environment manipulation without editing `hwm.yaml`.

```bash
hwm env add nightly-2024-12-01 --ghc=9.8.1
hwm env remove legacy
hwm env copy stable testing --ghc=9.6.6
hwm env set-default stable

```

#### Benefits
* Easier environment experimentation
* Better CLI-first workflow
* Reduced manual YAML editing

---

### 7. Dependency Management Commands


Enhanced registry management that automates the "tedious" parts of Haskell maintenance.

```bash
hwm deps add aeson --smart
hwm deps check --matrix
hwm deps prune --unused

```


#### Features

**Smart Add (`--smart`)**


* **Auto-Boundary Detection:** Fetches the latest Hackage version and suggests a PVP-compliant bound (e.g., `^>= 2.1.0`) based on your current GHC environment.
* **Conflict Pre-check:** Validates if the new dependency version is compatible with all existing environments (Stable, Nightly) before modifying files.


**Dependency Matrix Check (`--matrix`)**
* **Multi-Environment Validation:** Checks if your `build-depends` are valid across the entire `hwm.yaml` environment matrix.
* **Regression Warning:** Warns if a specific dependency version would break the build for an older GHC version you still support.


**Unused Dependency Detection (`--unused`)**
* **Import Analysis:** Cross-references `build-depends` with actual `import` statements in your `.hs` files (using `-ddump-minimal-imports`).
* **Pruning:** Safely removes unused packages from `.cabal` stanzas and `stack.yaml` extra-deps.

---

### 8. Workspace Validation


Add a deep validation command to catch configuration issues early, specifically for complex monorepos.

```bash
hwm validate [--strict]

```


#### Checks for:
* **PVP Compliance:** Flags dependencies with missing upper bounds or overly liberal ranges.
* **Circular Dependencies:** Detects cycles between internal workspace packages.
* **Version Drift:** Identifies when the same package has conflicting versions across different workspace groups.
* **Unreachable Packages:** Lists packages that exist in the directory but aren't included in any environment matrix.


### 9. Watch Mode

Auto-sync on configuration changes.

```bash
hwm watch
# Monitors hwm.yaml for changes, auto-runs sync
```

## 10. System Description: The HWM Philosophy


**HWM** (Haskell Workspace Manager) is a meta-build orchestrator designed to eliminate the friction of maintaining complex Haskell ecosystems. It operates as a "single source of truth" layer that sits above **Cabal**, **Stack**, and **Nix**, ensuring that these tools work in harmony rather than in conflict.

### Core Objectives

#### 1. Hybrid Workflow Orchestration


HWM recognizes that modern Haskell projects often require different tools for different tasks (e.g., Nix for reproducible builds, Stack for stable snapshots, and Cabal for flexible development). HWM synchronizes these by:

* Generating consistent `stack.yaml`, `cabal.project`, and `flake.nix` files from a single `hwm.yaml`.
* Aligning GHC versions and resolver snapshots across all backend tools.

#### 2. Empirical Dependency Intelligence


Unlike standard managers that rely on static metadata, HWM uses **Empirical Derivation**. It analyzes your actual build artifacts and test results to:

* **Derive Bounds:** Detect the true minimum and maximum compatible versions of dependencies by testing them against your matrix.
* **Fix Extra-Deps:** Automatically inject missing `extra-deps` into Stack configurations if they are not in the snapshot and derive fixed versions based on Cabal plan analysis.

#### 3. Maintenance Automation (The "Staff Engineer" in a Box)


HWM automates the high-level decision-making usually performed by senior maintainers:

* **Drift Prevention:** Flags when your local development environment deviates from your CI environment.
* **Smart Upgrades:** Doesn't just bump versions; it checks the "warning matrix" to ensure a bump doesn't introduce a flood of deprecation warnings across your workspace.

### Technical Architecture


HWM treats the workspace as a **Matrix of Environments**. Each package in your registry can be validated against multiple "Plan Sets" (e.g., `GHC-9.4-LTS` and `GHC-9.8-Nightly`). This allows a single developer to maintain a library that supports multiple years of Haskell evolution without manual context-switching.

### Summary of Value


By moving the complexity of workspace management into a declarative, automated tool, HWM allows developers to focus on writing Haskell code rather than fighting the Haskell toolchain. It transforms **"Cabal Hell"** into a **"Verified Matrix."**

---

## Contributing


If you're interested in implementing any of these features:


1. Open an issue to discuss the approach.
2. Check if the feature aligns with HWM's philosophy.
3. Submit a PR with tests and documentation.
4. Update this roadmap when features are completed.


Features listed here are **suggestions**, not commitments. Prioritization depends on community needs and maintainer availability.

---

## Completed Features


As features are implemented, they should be:


1. Removed from this roadmap.
2. Documented in [spec.md](spec.md).
3. Added to the changelog.

---


**Document Status:** Living Document  
**Next Review:** When a major version milestone is reached

