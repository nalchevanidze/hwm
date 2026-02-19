
# HWM Feature Roadmap

**Audience:** Contributors, Maintainers, Planners
**Last Updated:** February 15, 2026

This document outlines potential features and enhancements for HWM. These are suggestions for future consideration, not commitments.

---

## 1. Version Command: Direct Set

Allow `hwm version` to accept specific version numbers, not just bump types.

**Syntax:**

```bash
hwm version 2.0.0   # Set to specific version
hwm version 1.5.2   # Jump to arbitrary version
```

- Extend `data Bump = Major | Minor | Patch | Set Version`
- Parse version string as alternative to bump keyword
- Validate SemVer format
- Warn on non-monotonic version changes


## 2. Init Enhancements

**Options:**

- `--name=NAME`
- `--default-env=ENV`
- `--no-scripts`
- `--dry-run`

**Interactive mode:**

```bash
hwm init --interactive

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

Write to hwm.yaml? (y/n)
```


## 3. Registry Management

Automate tedious Haskell maintenance tasks:

**Unused Dependency Detection:**

```bash
hwm registry prune --unused
```

- Import analysis/pruning: Cross-reference `build-depends` with actual `import` statements in `.hs` files (using `-ddump-minimal-imports`). Safely remove unused packages from `.cabal` and `stack.yaml`.
- Extra-deps: Inject missing `extra-deps` into Stack configs if not in snapshot; derive fixed versions from Cabal plan analysis.
- Circular dependencies: Detect cycles between internal workspace packages.
- Unreachable packages: List packages present in directory but not included in any environment matrix.


## 4. Workspace Management

Manage monorepo structure and member metadata:

- `ws add <dir>`: Scaffold a new package and register as workspace member.
- `ws ls`: Display workspace tree, grouping members by logical groups (e.g., `libs`, `apps`).


---

## Contributing

Interested in implementing a feature?

1. Open an issue to discuss the approach.
2. Check alignment with HWM philosophy.
3. Submit a PR with tests and documentation.
4. Update this roadmap when features are completed.

Features listed here are **suggestions**, not commitments. Prioritization depends on community needs and maintainer availability.
