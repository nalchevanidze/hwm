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

- `hwm install` works end-to-end for `nix` and `nix/cabal` using the current/default environment.
- CI examples include at least one nix install path.



## 2) Registry maintenance ergonomics

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
