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


### 🚀 Release Orchestration & Native Cross-Platform Pipelines

**Goal:** Implement a native, zero-dependency release pipeline under the `hwm release archive` command.

Replace fragile release scripts and external system dependencies (`7z`, `tar`, `shasum`, `strip`) with a declarative, pure-Haskell archiving engine. HWM natively orchestrates binary releases, optimization, zipping, and cryptographic hashing for seamless handoff to any CI provider.

**Convention over Configuration (`hwm.yaml`):**
HWM uses smart defaults for enterprise-grade releases. Developers only need to define their executable mapping, and HWM automatically handles stripping, platform naming, and zipping.

*The "Zero-Config" Default:*

```yaml
workspace:
  - name: cli-tools
    type: app
    archive:
      executables:
        morpheus: morpheus-cli  # <final-binary-name>: <cabal-target>
      # HWM automatically defaults to: zip, stripped, static, sha256, and bundles LICENSE/README.md

```

*The "Opt-Out" Full Configuration (For Advanced Overrides):*

```yaml
workspace:
  - name: legacy-api
    type: app
    archive:
      executables:
        backend: legacy-server
      build:
        strip: false            # Opt-out: Keep DWARF debug symbols
        static: false           # Opt-out: Disable static linking
      format: raw               # Opt-out: Output raw binary instead of zip
      name_template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"
      checksum: sha256          
      include:                  
        - custom-config.yaml
```

**CLI & Universal CI Integration:**
Build, rename, optimize, bundle, zip, and hash artifacts in one step. Use `--out` to export the resulting file paths for zero-config handoff to any CI environment.

```bash
# Natively build, archive, hash, and export asset paths
hwm release archive cli-tools --out=release.env
```

**Key Capabilities:**

* **Explicit Target Mapping:** Decouple internal package names from shipped binaries (e.g., compile `morpheus-cli` but output `morpheus.exe`).
* **Smart Platform Detection:** Automatically populates `{{os}}` and `{{arch}}` for standard asset naming (e.g., `linux-x64`, `macos-arm64`).
* **Production Optimization:** Native binary stripping and static-linking support to guarantee small, portable executables without complex bash scripts.
* **Asset Bundling:** Automatically looks for and includes `LICENSE` and `README.md` inside the final release archive (if provided).
* **Cryptographic Checksums:** Natively generates `checksums.txt` to enable immediate, secure distribution via Homebrew, Nix, and AUR.
* **Zero External Dependencies:** Archiving and hashing run entirely in pure Haskell. No system tools required on the runner.
* **Universal CI Handoff:** CI-agnostic output seamlessly bridges HWM with GitHub Actions, GitLab CI, Jenkins, or bare-metal servers.


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
