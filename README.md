# HWM: Haskell Workspace Manager

> **Infrastructure-as-Code for your Haskell Monorepo.**

**HWM is not a build tool.** It is the missing link that orchestrates the tools you already use (`stack`, `cabal`, `hls`).

**Think of HWM as Terraform for your local repository.**
It ensures the state of your project files matches your declared intent across all packages and environments.

But HWM is not just a static generator—it is an **active workspace maintainer** that helps you:

- **Manage & Validate:** Add, remove, and analyze files to ensure structural consistency.
- **Derive & Verify:** Automatically calculate safe dependency bounds verified against your build matrix.
- **Release & Publish:** Fully automate releases (versioning & publishing) via simple workflow triggers.
- **Test & Orchestrate:** Run complex matrix tests locally without configuring CI scripts.

[![Status](https://img.shields.io/badge/Status-Alpha-orange)](https://github.com/nalchevanidze/hwm)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Powers](https://img.shields.io/badge/Powers-Morpheus%20GraphQL-purple)](https://github.com/morpheusgraphql/morpheus-graphql)

---

### 🛑 The Problem

In a typical Haskell monorepo, maintaining consistency is a full-time job:

1. **Dependency Drift:** "Why does `core` use `aeson-2.0` but `api` uses `aeson-1.5`?"
2. **Matrix Complexity:** Testing GHC 9.6 (Stable) and 8.10 (Legacy) requires maintaining multiple fragile `stack.yaml` files.
3. **Broken IDEs:** Adding a module often breaks HLS, requiring manual updates to a complex `hie.yaml`.
4. **CI Hell:** You only find out your bounds are wrong after waiting 15 minutes for GitHub Actions to fail.

### ✅ The Solution

You define the **"what"** (structure, bounds, matrix) in a single file: `hwm.yaml`.
HWM handles the **"how"** by generating the 30+ config files your tools expect.

<p align="center">
<img src="images/status.png" alt="HWM Status Output" width="600">
</p>

---

## 🧩 The "Missing Link" Architecture

HWM sits one layer above your toolchain. It acts as the **Single Source of Truth**.

```mermaid
graph TD
    HWM[hwm.yaml] ===>|Source of Truth| Core

    subgraph "Root Configs"
    Core --> Stack[stack.yaml]
    Core --> HIE[hie.yaml]
    end

    subgraph "Package Configs"
    Core --> Pkg[packages/*/package.yaml]
    Pkg -->|Cabal Gen| Cabal[*.cabal]
    end

    subgraph "Matrix Engines (Internal)"
    Core -.->|Generates| Matrix[".hwm/matrix/stack-*.yaml"]
    Matrix -.->|Powers| Run["hwm run test --env=legacy"]
    end

    style HWM fill:#f9f,stroke:#333,stroke-width:4px
    style Matrix stroke-dasharray: 5 5


```

- **You write:** `hwm.yaml` (1 file).
- **HWM generates:** `stack.yaml`, `package.yaml`, `.cabal`, `hie.yaml` (The noise you shouldn't have to manage).
- **You run:** Standard `stack` or `cabal` commands (or use `hwm run` wrappers).

---

## 🟢 Quick Start

### Installation

```bash
stack install hwm
# or
cabal install hwm

```

### Zero-Config Onboarding

Already have a Stack project? Transform it into an HWM workspace in seconds.

```bash
# 1. Generate hwm.yaml from your existing stack.yaml
# HWM automatically discovers packages and infers dependencies
hwm init

# 2. Sync configuration and build
hwm sync
hwm run build

```

<p align="center">
<img src="images/init.png" alt="HWM Init Auto-Discovery" width="600">
</p>


## 🧪 Born from Necessity

HWM was not built in a vacuum. It was created to solve the massive orchestration headaches of the **[Morpheus GraphQL](https://github.com/morpheusgraphql/morpheus-graphql)** ecosystem.

Today, it powers the entire Morpheus monorepo, managing:

- **15+ Packages:** Keeping `core`, `api`, and `client` in sync without version drift.
- **Hybrid Matrix:** Simultaneously testing `stable` (GHC 9.6) and `legacy` (GHC 8.10) environments.
- **Unified Registry:** A single source of truth for version bounds across the entire repository.

> **Tip:** You can view the [live configuration here](https://github.com/morpheusgraphql/morpheus-graphql/blob/main/hwm.yaml) to see a full-scale example of HWM in production.

---

## 🛠️ Key Workflows

### 1. Registry & Dependency Management

Stop guessing version bounds. The `registry` in `hwm.yaml` is the central source of truth for all packages.

**Smart Add:**
`hwm registry add` uses a "Sandwich" discovery logic to find the safest bounds by auditing your `legacy` (min) and `nightly` (max) snapshots.

```bash
# Add to a specific package
hwm registry add aeson libs/core

# Add to an entire group (all packages in 'libs')
hwm registry add servant libs


```

**Audit & Fix:**
Audit your bounds against **real** Stackage snapshots. Ensure you only claim support for versions you actually test.

```bash
# Auto-update bounds in hwm.yaml to match tested snapshots
hwm registry audit --fix

```

<p align="center">
<img src="images/audit.png" alt="HWM Audit Command" width="600">
</p>

### 2. Workspace Scaffolding

Stop manually creating folders, copying `package.yaml` files, and breaking your LSP. HWM scaffolds new packages natively and instantly wires them into your global monorepo configuration.

```bash
# Interactively or directly scaffold a new package in a specific group
hwm workspace add libs/core

```

<p align="center">
<img src="images/ws-add.png" alt="HWM Workspace Add Command" width="600">
</p>

**What HWM does instantly:**


- Natively generates the package directory and `package.yaml` (using the modern object format).
- Registers the new package under the correct group in `hwm.yaml`.
- Automatically wires the package into `stack.yaml` so it builds immediately.
- Automatically injects the package into `hie.yaml`.

### 3. Local Matrix Strategy

In most projects, the Build Matrix only exists in the cloud (`.github/workflows`). HWM brings that matrix down to your machine.

**The HWM Way:** Run your entire matrix locally. Catch failures on "Legacy GHC" before you push.

```bash
# Test across all defined environments (Stable, Nightly, Legacy)
hwm run test --env=all

```

<p align="center">
<img src="images/matrix.png" alt="HWM Matrix Build Output" width="700">
</p>

### 4. Automated Release Workflows

Eliminate manual release steps. By combining HWM with `relasy`, you can fully automate the release lifecycle with a single CI workflow trigger.

- **Auto-Versioning:** Calculate the next semantic version based on changes.
- **Auto-Publishing:** Upload synchronized packages to Hackage.

```bash
# Triggered by your CI pipeline:
hwm version minor   # Bumps versions atomically
hwm publish libs    # Uploads the entire workspace group


```

---

### 📦 Artifact Pipeline

The `artifacts` pipeline is HWM's **End-User Distribution Engine**. It transforms raw binaries into optimized, compressed, and hashed distribution units. All logic is implemented natively in Haskell for maximum portability.

#### Configuration

HWM uses a **Hybrid-Flat** structure, where targets and settings share the same namespace for clarity and simplicity.

```yaml
# hwm.yaml
project: morpheus
version: 0.1.2

release:
  artifacts:
    # Binary Targets (Internal Path)
    morpheus: libs/hwm-cli:morpheus
    hwm-daemon: apps/daemon:hwm-daemon

    # Pipeline Settings
    formats: [Zip, TarGz]
    name_template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"
```

#### Usage

```bash
# Local staging (wipes and fills .hwm/dist)
hwm release artifacts

# Publish to GitHub Releases with a masked upload URL
hwm release artifacts --gh-publish <upload_url> --tag v0.1.2

```

### 5. Environment Management

Manage multiple GHC versions and resolvers effortlessly.

```yaml
# hwm.yaml
matrix:
  default-environment: stable
  environments:
    - { name: stable, ghc: 9.6.3, resolver: lts-22.6 }
    - { name: nightly, ghc: 9.10.1, resolver: nightly-2024-05-22 }
```

- **Add:** `hwm environment add stable lts-24.25` (Validates against Stackage).
- **Switch:** `hwm environment set-default stable`.
- **List:** `hwm environment ls`.

---

## 🤖 Built for the AI Era

HWM transforms your monorepo into a format LLMs can actually understand.

 - **Context Window Friendly:** Instead of pasting 20+ `.cabal` files to provide AI context, paste one `hwm.yaml`.
---

## ⚖️ Comparison

Most Haskell teams are stuck between "Manual Chaos" and "Nix Overkill." HWM offers a middle ground.

| Feature                | 🐢 Manual Config          | ❄️ Nix / Bazel            | 🚀 HWM                            |
| ---------------------- | ------------------------- | ------------------------- | --------------------------------- |
| **Config Source**      | Decentralized (30+ files) | Centralized (`flake.nix`) | **Centralized (`hwm.yaml`)**      |
| **Smart Add**          | ❌ Manual Search          | ❌ Manual Edit            | **✅ `hwm add` (Auto-Discovery)** |
| **Release Automation** | ❌ Manual Upload          | ❌ Custom Scripts         | **✅ CI Workflow Trigger**        |
| **Artifacts**          | ❌ Manual Scripts         | ⚠️ Complex                | **✅ Native `.zip/.tar.gz**`      |

---

## 🔮 Roadmap

HWM is actively evolving. While the core Workspace and Matrix engines are stable, we are actively developing the following "Day 2" operations:

- **Professional Distribution (v0.1.0):** Automated Homebrew Formula generation and universal installers (`.deb`, `.rpm`).
- **Deep Nix Integration:** Generating `flake.nix` directly from `hwm.yaml`.
- **Smart Registry Pruning:** Automated detection of unused imports and missing `extra-deps`.

---

## 🧬 Status

HWM is currently in **Alpha**. We value your feedback—please [open an issue](https://github.com/nalchevanidze/hwm/issues) if you encounter bugs or have feature suggestions.
