# HWM Feature Roadmap

**Audience:** Contributors, Maintainers, Planners
**Last Updated:** February 15, 2026

This document outlines potential features and enhancements for HWM. These are suggestions for future consideration, not commitments.

---

## Init Enhancements

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

## Registry Management

Automate tedious Haskell maintenance tasks:

**Unused Dependency Detection:**

```bash
hwm registry prune --unused
```

- Import analysis/pruning: Cross-reference `build-depends` with actual `import` statements in `.hs` files (using `-ddump-minimal-imports`). Safely remove unused packages from `.cabal` and `stack.yaml`.
- Extra-deps: Inject missing `extra-deps` into Stack configs if not in snapshot; derive fixed versions from Cabal plan analysis.
- Circular dependencies: Detect cycles between internal workspace packages.
- Unreachable packages: List packages present in directory but not included in any environment matrix.

## Workspace Management

Manage monorepo structure and member metadata:

- `ws add <dir>`: Scaffold a new package and register as workspace member.
- `ws ls`: Display workspace tree, grouping members by logical groups (e.g., `libs`, `apps`).

## Binary Generation & Releases

Replace fragile bash scripts with declarative release orchestration. HWM will use group metadata to automatically build, rename, and collect binaries across platforms.

**Configuration (`hwm.yaml`):**
Use the `binaries` map to decouple your internal Cabal package name from the final shipped binary name.

```yaml
workspace:
  - name: cli-tools
    type: app
    binaries:
      morpheus: morpheus-graphql-code-gen # <final-binary>: <cabal-package>
```

**CLI Execution:**

```bash
# Builds targets, renames them, and extracts to ./dist
hwm run build --release --artifacts-dir=./dist

```

**Key Capabilities:**

- **Explicit Mapping:** Build `morpheus-graphql-code-gen` but output `morpheus` (or `morpheus.exe` on Windows).
- **Cross-Platform Normalization:** Automatically handles OS-specific extensions during artifact extraction.
- **Zero-Config CI:** Replaces complex GitHub Actions matrices with a single command to collect all release artifacts.

## Deep Nix Integration

Maintaining a `flake.nix` for a multi-package, multi-GHC monorepo is notoriously painful. HWM will act as the ultimate bridge, generating idiomatic Nix configurations directly from `hwm.yaml` with zero boilerplate.

**Planned Capabilities:**

- **Matrix to DevShells:** HWM environments map directly to Nix. Run `nix develop .#stable` (GHC 9.6) or `nix develop .#legacy` (GHC 8.10) without writing custom overlays.
- **Auto-Exported Packages:** Every workspace member (`lib` or `app`) is automatically exposed as a buildable Nix derivation.
- **Snapshot Translation:** HWM registry bounds and Stackage snapshots translate to pinned Nixpkgs inputs, guaranteeing `nix build` matches `stack build`.

**CLI Usage:**

```bash
hwm init --nix  # Scaffold workspace with flake.nix
hwm sync --nix  # Synchronize flake alongside cabal/stack
```

**The Value:** True hybrid workflows. Nix power-users get full reproducibility, while the rest of the team continues using standard Cabal/Stack. Both use the exact same single source of truth.

## Contributing

Interested in implementing a feature?

1. Open an issue to discuss the approach.
2. Check alignment with HWM philosophy.
3. Submit a PR with tests and documentation.
4. Update this roadmap when features are completed.

Features listed here are **suggestions**, not commitments. Prioritization depends on community needs and maintainer availability.
