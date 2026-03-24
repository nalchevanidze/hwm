# HWM Roadmap

**Audience:** Contributors, maintainers, planners  
**Last Updated:** 2026-03-24

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

## 4) UX transparency and conceptual consistency

### Goal
Remove behavior surprises that can frustrate users.

### Planned outcomes

- Improve unknown command handling:
  - avoid silently treating typos as script names unless script exists
  - provide clearer CLI typo errors and suggestions
- Improve script runner UX:
  - document and/or redesign arg forwarding behavior
  - reduce shell-specific surprises
- Improve HLS integration consistency:
  - align generated `hie.yaml` with active builder/profile (not stack-only assumptions)
- Improve generated-file UX:
  - clearly communicate overwrite/ephemeral behavior for generated files
- Protect environment integrity:
  - prevent removing the default/last environment without an explicit migration path
- Improve cross-platform script execution behavior (avoid hard dependency on `/bin/sh` semantics where possible)

### Acceptance criteria

- CLI typos produce command-oriented errors, not script-not-found confusion.
- `hwm run` argument behavior is explicit and predictable.
- `hie.yaml` generation is builder/profile-aware.
- removing environments cannot leave config in an invalid default-env state.
- docs include a single transparent “known limitations/behavior notes” section.

---

## 5) Release behavior consistency (deep-dive findings)

### Goal
Make release/install behavior predictable and aligned with configuration intent.

### Problematic behaviors observed

- `release.artifacts[*].environments` is currently not honored by `hwm release artifacts` (active/default env is used instead).
- `--output-dir` handling in artifacts flow should be audited for consistency with directory preparation behavior.
- `--ghc-options` parser/help UX should be aligned (help suggests repeatable flags, parser currently expects a single CSV option).

### Planned outcomes

- Keep publish behavior explicitly documented as Cabal-`sdist`/Hackage-oriented.
- Make artifact builds honor per-artifact environments.
- Ensure output-dir semantics are consistent and test-covered.
- Align CLI parser behavior with help text for release flags.

### Acceptance criteria

- release behavior is deterministic and matches docs/examples.
- artifact environment targeting works as declared in `hwm.yaml`.
- CLI help and parser behavior match exactly.

---

## Contributing

If you want to implement roadmap items:

1. Open an issue with a short design proposal.
2. Confirm CLI/API compatibility impact.
3. Submit PR with tests and docs.
4. Update this roadmap by moving delivered items into release notes/changelog and removing them from active roadmap.

Roadmap items are priorities, not guarantees; order may change based on user demand and maintainer capacity.
