# HWM: Feature Roadmap

**Target Audience:** Contributors, Maintainers, Planning

**Last Updated:** February 15, 2026

This document outlines potential features and enhancements that could benefit HWM users. These are not committed features but rather ideas for future consideration.

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

* Override inferred project name
* Use case: When directory name doesn't match desired project name
* Example: `hwm init --name=my-awesome-project`

**`--version=VERSION`**

* Override inferred project version
* Use case: Set specific starting version instead of auto-detection
* Example: `hwm init --version=1.0.0`

**`--default-env=ENV`**

* Set which environment becomes the matrix default
* Use case: Control which resolver/GHC is used by default
* Example: `hwm init --default-env=stable`

**`--no-scripts`**

* Don't generate default scripts section
* Use case: Clean minimal config, custom workflow
* Example: `hwm init --no-scripts`

**`--dry-run`**

* Preview generated configuration without writing file
* Use case: Validate inference before committing
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

* Extend `data Bump = Major | Minor | Patch | Set Version`
* Parse version string as alternative to bump keyword
* Validate SemVer format
* Warn on non-monotonic version changes

#### Benefits

* Align with external version requirements
* Support version resets or corrections
* Better integration with release management tools

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

Enhanced registry management beyond `outdated`.

```bash
hwm deps add aeson --bounds=">= 2.0 && < 3.0"
hwm deps remove aeson
hwm deps update aeson  # Update specific package only
hwm deps search lens   # Search Hackage with preview
hwm deps check         # Validate bounds against current builds

```

---

### 8. Workspace Validation

Add deep validation command to catch configuration issues early.

```bash
hwm validate [--strict]

```

Check for:

* Duplicate package names
* Circular dependencies
* Version conflicts between registry and extra-deps
* Unreachable packages (not in any environment)
* Unused registry entries

---

### 9. Watch Mode

Auto-sync on configuration changes.

```bash
hwm watch
# Monitors hwm.yaml for changes, auto-runs sync

```

---

## Contributing

If you're interested in implementing any of these features:

1. Open an issue to discuss the approach
2. Check if the feature aligns with HWM's philosophy
3. Submit a PR with tests and documentation
4. Update this roadmap when features are completed

Features listed here are **suggestions**, not commitments. Prioritization depends on community needs and maintainer availability.

---

## Completed Features

As features are implemented, they should be:

1. Removed from this roadmap
2. Documented in [spec.md](spec.md)
3. Added to the changelog

---

**Document Status:** Living Document
**Next Review:** When major version milestone is reached

