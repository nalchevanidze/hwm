# HWM Roadmap

**Audience:** Contributors, maintainers, planners  
**Last Updated:** 2026-03-25

This document tracks **future work only**.
If something is already implemented, it should not stay on this roadmap.

---

## Priority roadmap

## 1) Nix parity for install

### Goal
Bring `nix` and `nix/cabal` to feature parity for install workflows.

### Why
Today, `install` for these builder modes is unsupported (explicit errors). We should close this gap.

### Planned outcomes

- Support `hwm install` for:
  - `builder: nix`
  - `builder: nix/cabal`
- Keep deterministic behavior and clear logs for Nix install outputs.
- Preserve current fail-fast errors until full support is complete.

### Acceptance criteria

- `hwm install --env=<nix-env>` works end-to-end.
- CI examples include at least one nix install path.

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
