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


## 🗺️ Roadmap: Professional Distribution (v0.1.0)

### **The Goal**

Transition from manual "side-loading" via `install.sh` to a standard, versioned distribution model using **Homebrew** for macOS and Linux.
---

### **1. Artifact Maturity (Current Work)**

* **Standardized Pipeline:** Finalize `hwm release artifacts` to produce consistent, versioned `.tar.gz` and `.sha256` pairs.
* **Release Automation:** Integrate `--gh-upload` to ensure the cloud binaries are always the source of truth.

### **2. The Homebrew Integration**

* **Create `homebrew-hwm`:** Establish a dedicated GitHub repository (e.g., `nalchevanidze/homebrew-hwm`) to act as your personal "Tap."
* **Formula Automation:** Add a post-build hook in HWM that:
1. Calculates the SHA-256 of the new release.
2. Updates the Ruby formula (`hwm.rb`) with the new URL and checksum.
3. Pushes the change to the Tap repository automatically.


* **Deprecate `install.sh`:** Replace the manual script with a simple one-liner for users:
```bash
brew tap nalchevanidze/hwm && brew install hwm
```

### **3. Strategic Benefits**

* **Automatic Updates:** Users gain `brew upgrade hwm`, removing the need for them to manually re-run an installation script.
* **Environment Safety:** Homebrew handles `$PATH` conflicts and ensures the binary is placed in `/usr/local/bin` or `/opt/homebrew/bin` correctly.
* **Security:** Cryptographic signing (via the `.sha256` seal) becomes a first-class citizen of the installation process.

## Contributing

Interested in implementing a feature?

1. Open an issue to discuss the approach.
2. Check alignment with HWM philosophy.
3. Submit a PR with tests and documentation.
4. Update this roadmap when features are completed.

Features listed here are **suggestions**, not commitments. Prioritization depends on community needs and maintainer availability.
