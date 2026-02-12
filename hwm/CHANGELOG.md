# Changelog

All notable changes to this project will be documented in this file.

For detailed release notes, see [GitHub Releases](https://github.com/nalchevanidze/hwm/releases).

## 0.0.1 - 2026-02-15

### Initial Release

* Core workspace management from single `hwm.yaml` configuration
* Automatic generation of `stack.yaml`, `package.yaml`, `.cabal`, and `hie.yaml`
* Multi-GHC build matrix support with environment switching
* Centralized dependency registry with version bounds
* Package status checking and synchronization
* Dependency update checking (`hwm outdated`)
* Atomic version bumping across workspace
* Coordinated Hackage publishing
* Zero-config onboarding from existing Stack projects (`hwm init`)
