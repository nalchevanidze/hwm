# HWM: Haskell Workspace Manager

> **Infrastructure-as-Code for your Haskell Workspaces.**

**HWM is a universal, build-tool agnostic orchestrator.** It is the missing link that unites the tools you already rely on (`cabal`, `stack`, `nix`, `hls`), transforming them into a single declarative pipeline.

**Think of HWM as Terraform for your local Haskell repository.** Whether you are a Nix purist, a Stack loyalist, or rely purely on Cabal, HWM ensures the state of your project files matches your declared intent across all environments.

HWM is an **active workspace maintainer** that provides:

- **The Universal Translator:** Write one `hwm.yaml`. HWM automatically derives and generates `cabal.project`, `stack.yaml`, `hie.yaml`, `flake.nix`, and `.cabal` files.
- **Zero Lock-in:** HWM materializes standard configuration files directly at your project root. You can uninstall HWM at any time, and your repository will still build perfectly using standard native tools.
- **Smart Bounds Synchronization:** Maintain a beautifully aligned, single-source-of-truth dependency registry. HWM automatically injects these bounds across your entire monorepo.
- **Zero-Overhead IDE Support:** Because standard files are generated at the root, Haskell Language Server (HLS) works instantly. HWM automatically generates `hie.yaml` tailored to your active toolchains.
- **Flexible Toolchain Toggles(v0.2.0):** You are in total control. Explicitly enable or disable `stack`, `nix` globally, or toggle them on a per-profile basis.

<p align="center">
<img src="images/status.png" alt="HWM Status Output" width="600">
</p>

## 🧩 The Agnostic Architecture

HWM sits one layer above your toolchain. It separates your **workspace intent** from your **build implementation**.

```mermaid
graph TD
    HWM[hwm.yaml] ===>|Single Source of Truth| Engine((HWM Engine))

    subgraph "Native Configurations (Managed & Idempotent)"
    Engine --> Cabal[cabal.project & *.cabal]
    Engine --> Nix[flake.nix]
    Engine --> Stack[stack.yaml]
    Engine --> HLS[hie.yaml]
    end

    subgraph "Your Build Tools"
    Cabal -.-> RunCabal[cabal build]
    Nix -.-> RunNix[nix develop]
    Stack -.-> RunStack[stack build]
    end

    style HWM fill:#f9f,stroke:#333,stroke-width:4px

```

---

## 🟢 Quick Start

### Installation

```bash
cabal install hwm
```

### Zero-Config Onboarding

Convert any existing repository into an HWM workspace in seconds.

```bash
# 1. Generate hwm.yaml. HWM automatically discovers packages and infers dependencies.
hwm init

# 2. Sync configuration (Generates cabal.project, stack.yaml, flake.nix, hie.yaml)
hwm sync

# 3. View the visual dashboard of your workspace
hwm status

```

<p align="center">
<img src="images/init.png" alt="HWM Init Auto-Discovery" width="600">
</p>

## 🛠️ Key Workflows

### 1. The Global Registry & Dependency Sync

HWM uses a gorgeous, tabular dictionary to manage your dependencies. You define the bounds once in `hwm.yaml`, and HWM automatically injects them into every package in your monorepo.

```yaml
registry:
  Cabal: ">= 3.8      && <= 3.16.1.0"
  aeson: ">= 1.5.6.0  && <= 2.2.3.0"
  mtl: ">  2.0.0    && <  2.6.0"
```

**Audit & Fix:**
Audit your bounds against actual snapshots to ensure you only claim support for versions validated by your build matrix.

```bash
hwm registry audit --fix

```

<p align="center">
<img src="images/audit.png" alt="HWM Audit Command" width="600">
</p>

### 2. Smart Workspace Routing

Managing monorepos with dozens of packages is finally clean. HWM uses `prefix` grouping to elegantly decouple your internal structure from your globally unique Hackage package names.

```bash
# Interactively or directly scaffold a new package in a specific group
hwm workspace add libs
```

<p align="center">
<img src="images/ws-add.png" alt="HWM Workspace Add Command" width="600">
</p>

```yaml
workspace:
  libs:
    prefix: morpheus-graphql
    members:
      - core
      - client
      - server
```

_When HWM generates your configs, it automatically builds the exact relative paths (`morpheus-graphql-core`), saving you from writing out bloated package names over and over._

### 3. Environments, Builders & CI Profiles

Bring the power of CI matrices directly into your local workspace. HWM allows you to define logical **environments** that map to specific GHC versions, toolchain toggles, and specific builders (`stack`, `cabal`, or `nix`). 

Instead of writing complex bash routing in GitHub Actions or juggling multiple config files locally, you define your targets exactly once.

```yaml
environments:
  # Global defaults for the workspace
  builder: stack 
  default: stable
  nix: true # generates flake.nix on hwm sync
  stack: true # generates stack.yaml on hwm sync
  profiles:
    legacy:
      ghc: 8.10.7
      # Override global settings: inject specific stack dependencies
      stack:
        extra-deps:
          base-orphans: 0.8.1
          fastsum: 0.1.0.0

    stable:
      ghc: 9.6.3
      stack:
        extra-deps:
          fastsum: 0.1.1.1

    # Purpose-built CI profiles
    ci-windows:
      ghc: 9.6.3
      builder: cabal
    ci-nix:
      ghc: 9.6.3
      builder: nix

```

**Seamless CI Integration:**
By defining `ci-nix` and `ci-windows`, your GitHub Actions workflow becomes incredibly dumb (which is exactly what you want). You just tell HWM to sync the environment, and it instantly pivots the workspace to use the correct underlying toolchain.

```bash
# On Ubuntu/macOS runners:
hwm sync ci-nix
hwm build
hwm test 

# On Windows runners:
hwm sync ci-windows
hwm build
hwm test
```

**Run Your Matrix Locally:**
You can also run your tests across all defined environments to guarantee your changes won't break legacy users before you even push to CI. *(Note: Matrix testing currently supports Stack; Nix and Cabal support is coming soon).*

```bash
hwm test --env=all

```

<p align="center">
<img src="images/matrix.png" alt="HWM Matrix Build Output" width="700">
</p>

**Manual Environment Switching:**
Need to debug a legacy GHC issue locally? Just switch environments. HWM instantly overwrites your root configs (`stack.yaml`, `cabal.project`, `flake.nix`) to match that specific profile.

```bash
hwm sync legacy

```

### 4. Task Runner & Scripts

HWM includes a lightweight, pass-through task runner. Define simple aliases for your most common workflows directly in `hwm.yaml`.

```yaml
scripts:
  build: cabal build all
  clean: find . -name "*.cabal" -exec rm -rf {} \; && cabal clean
  test: cabal test all
```

Pass arguments seamlessly to your underlying tools:

```bash
# Translates to: cabal test morpheus-graphql-core --test-show-details=direct
hwm run test -- morpheus-graphql-core --test-show-details=direct

```

### 5. Release & Distribution

HWM introduces **Release Trains**, a high-integrity system for decoupling workspace structure from distribution strategy while ensuring topological correctness.

#### 📦 Artifact Pipeline

Transform raw binaries into hashed, compressed distribution units using your preferred engine. HWM ensures every artifact is strictly validated before the publication phase begins.

```yaml
environments:
  builder: stack # or nix or cabal
release:
  artifacts:
    hwm: libs/_root_:hwm

```

#### 🚢 Publication Trains

Define groups of packages to be published to Hackage. HWM enforces a **topological sort**, ensuring "core" dependencies are published before the packages that rely on them.

```yaml
release:
  publish:
    main:
      - libs

```

**Usage:**

```bash
# Bump version across the workspace
hwm version minor

# Build local binaries and hashes with builder of choice
hwm release artifacts --builder=nix

# Push a train to Hackage (Requires HACKAGE_AUTH_TOKEN in environment with a valid API token)
hwm release publish main
```



<p align="center">
<img src="images/publish.png" alt="HWM Publish Output" width="600">
</p>

## ⚖️ The Haskell Tooling Landscape

| Feature                  | Standard Setup    | Nix / Bazel   | 🚀 HWM v0.2.0                       |
| ------------------------ | ----------------- | ------------- | ----------------------------------- |
| **Config Source**        | Decentralized     | Centralized   | **Centralized (`hwm.yaml`)**        |
| **Build System Support** | Single Tool       | High Friction | **✅ Agnostic (Nix, Cabal, Stack)** |
| **Idempotency**          | Manual Edits      | Varies        | **✅ Silent Writes (mtime-safe)**   |
| **IDE Setup**            | Manual `hie.yaml` | Complex       | **✅ Auto-Generated (Smart)**       |
| **Lock-in**              | High              | Extreme       | **✅ Zero Lock-in**                 |

## 🧬 Status

HWM is currently in **v0.2.0 (Beta)**. It was built to solve the orchestration needs of the **[Morpheus GraphQL](https://github.com/morpheusgraphql/morpheus-graphql)** ecosystem, where it successfully synchronizes 15+ packages across legacy and modern GHC profiles.

Your feedback is highly valued! Please [open an issue](https://github.com/nalchevanidze/hwm/issues) if you encounter bugs or want to share how you are using HWM.
