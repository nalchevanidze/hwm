# HWM: Haskell Workspace Manager

**HWM is not a build tool.** It is the missing link that orchestrates the tools you already use.

Haskell has excellent build systems (`stack`, `cabal`, `nix`) and a powerful IDE (`hls`), but they don't talk to each other in a monorepo. HWM bridges this gap by acting as a **single source of truth**, automatically generating and synchronizing the configuration files those tools expect.

## The Problem

In a typical Haskell monorepo:

* **Dependency Drift:** Different packages use different versions of the same dependency
* **Matrix Complexity:** Testing multiple GHC versions requires maintaining separate config files
* **Broken IDEs:** Adding modules breaks HLS until you manually update `hie.yaml`
* **Release Friction:** Releasing requires manually bumping versions across dozens of files

## The Solution


You define the "what" in `hwm.yaml`. HWM handles the "how."

**Automatic Bounds Auditing:**
HWM checks your dependency bounds using the oldest environment in your build matrix and the latest Stackage nightly. Bounds must not be inside the tested matrix window (error), and if they are outside, you get a warning (may break on untested versions). This ensures your registry bounds are always safe and meaningful.

```yaml
name: my-project
version: 0.1.0

workspace:
  - name: libs
    prefix: my-app
    members: [core, api, client]

registry:
  - aeson >= 2.0 && < 3.0
  - text  >= 2.0 && < 3.0

matrix:
  default-environment: stable
  environments:
    - { name: stable, ghc: 9.6.3, resolver: lts-22.6 }
    - { name: nightly, ghc: 9.10.1, resolver: nightly-2024-05-22 }
```

One command regenerates everything:

```bash
hwm sync
```

HWM generates and keeps in sync:
* `stack.yaml` / `cabal.project`
* `package.yaml` files → `.cabal` files
* `hie.yaml` for HLS
* Build matrix configurations

## Quick Start

```bash
# Install
stack install hwm
# or
cabal install hwm

# In an existing Stack project
hwm init
hwm sync
hwm run build
```

## Key Commands

* `hwm status` - Check if generated files are in sync
* `hwm sync` - Regenerate all configuration files
* `hwm run <script>` - Run scripts across build matrix
* `hwm outdated` - Check for dependency updates and audit bounds safety
  * `--fix` will only fix errors (bounds that are inside the matrix window).
  * To also fix warnings (bounds outside the matrix window), use `--fix --force`.
* `hwm version <bump>` - Atomically bump versions
* `hwm publish <group>` - Publish to Hackage

## Full Documentation

📚 **[Complete Documentation & Examples →](https://github.com/nalchevanidze/hwm)**

Visit the GitHub repository for:
* Detailed configuration reference
* Architecture documentation
* Contributing guidelines
* Examples and use cases

## More Than a Config Generator: Your Project Auditor & Assistant

HWM is not just a config generator—it is an active assistant for project maintenance and safety:

- **Auditing Power:** HWM audits your dependency bounds against real, tested package sets from Stackage LTS and Nightly snapshots. This means:
  - You only claim support for versions you actually test in CI.
  - You avoid breakage from untested versions (e.g., new releases on Hackage that aren't in Stackage yet).
  - You get clear errors if your bounds are too narrow (missing tested versions) or warnings if they're too wide (including untested versions).
- **Why Stackage Snapshots?**
  - Stackage snapshots are curated, reproducible sets of package versions. By aligning your bounds with these, you guarantee that your project is always buildable and testable in real environments.
- **Automated Fixes:** With `hwm outdated --fix` and `--force`, you can automatically update your bounds to match the tested window, keeping your project safe and future-proof with minimal effort.

### Example: Auditing in Action

Suppose your registry claims to support `aeson >= 1.4.4 && < 3.0.0`, but your matrix only tests up to `aeson-2.2.3.0` (latest Nightly). HWM will warn you that you are claiming support for versions you don't test. If you restrict your bounds to `>= 2.0 && < 2.2.0` but your matrix tests from `1.4.4` to `2.2.3.0`, HWM will error, since you are missing tested versions.

**This makes HWM a true assistant for long-term project health, not just a file generator.**

- **No More Bounds Headaches:** HWM automates and audits your dependency bounds, so you never have to manually guess or maintain safe version ranges again. Your bounds always reflect what is actually tested, eliminating a major source of maintenance pain in Haskell projects.

## Origin

HWM was born out of necessity to manage the [Morpheus GraphQL](https://github.com/morpheusgraphql/morpheus-graphql) ecosystem with 15+ packages across multiple GHC versions.
