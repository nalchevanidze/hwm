# HWM Roadmap

**Audience:** Contributors, maintainers, planners  
**Last Updated:** 2026-03-24

This document tracks **future work only**.
If something is already implemented, it should not stay on this roadmap.

---

## Priority roadmap

## 1) Nix parity for install and publish

### Goal
Bring `nix` and `nix/cabal` to feature parity for delivery workflows.

### Why
Today, install/publish operations for these builder modes are not fully supported and may fail with explicit errors. We should close this gap.

### Planned outcomes

- Support `hwm install` for:
  - `builder: nix`
  - `builder: nix/cabal`
- Support `hwm release publish` flows when workspace is using Nix-driven builders.
- Keep deterministic behavior and clear logs for Nix paths/artifacts.
- Preserve current fail-fast errors until full support is complete.

### Acceptance criteria

- `hwm install --env=<nix-env>` works end-to-end.
- `hwm release publish <group>` works in Nix/Nix-Cabal environments.
- CI examples include at least one nix install path and one nix publish path.

---

## 2) Cabal-first source inclusion (reduce `package.yaml` dependency)

### Goal
Make `.cabal` files self-sufficient by automatically maintaining source/module inclusion patterns.

### Why
Many workspaces want to avoid `package.yaml`/hpack and rely directly on `.cabal` files.

### Planned outcomes

- Add automatic source inclusion support for `.cabal`-managed packages using a glob strategy (initial target: `*/*.hs`, with extension points for broader patterns).
- Keep generated/updated `.cabal` metadata aligned with discovered source files.
- Enable workflows where `package.yaml` is optional or unnecessary for standard package layouts.

### Acceptance criteria

- `hwm sync` updates `.cabal` package source/module inclusion from project files.
- New packages can be maintained without requiring `package.yaml`.
- Behavior is documented with migration examples from hpack to cabal-only setups.

---

## 3) Registry maintenance ergonomics

### Goal
Reduce long-term dependency drift in large monorepos.

### Planned outcomes

- Add `hwm registry prune --unused` (or equivalent) to identify/remove stale registry entries.
- Add clearer reporting for “in registry but unused in workspace.”

---

## Contributing

If you want to implement roadmap items:

1. Open an issue with a short design proposal.
2. Confirm CLI/API compatibility impact.
3. Submit PR with tests and docs.
4. Update this roadmap by moving delivered items into release notes/changelog and removing them from active roadmap.

Roadmap items are priorities, not guarantees; order may change based on user demand and maintainer capacity.
