# Roadmap Item 2 Plan (Cabal-first source inclusion)

1. Keep current `package.yaml` policy unchanged:
   - If `package.yaml` exists, do not directly sync `.cabal` source/module inclusion.
   - Keep existing validation + hpack-driven behavior for hpack packages.

2. Add cabal-first source/module inclusion sync for cabal-only packages:
   - During `hwm sync`, discover Haskell source files from component source dirs.
   - Update `.cabal` component module inclusion fields deterministically.

3. Match package.yaml-style inclusion behavior:
   - Use discovered modules from source dirs (initial `.hs` scope).
   - Keep ordering stable to avoid noisy diffs.

4. Integrate into existing package sync flow:
   - Run dependency/version sync as before.
   - Run source inclusion sync only on cabal-only packages.

5. Preserve validation behavior:
   - Keep Cabal/Hackage validation after rewrite.
   - Keep hpack packages on current validate/hpack path.

6. Add golden coverage:
   - Cabal-only package source inclusion gets synced.
   - `package.yaml` packages are not direct-cabal source-synced.
   - Sync is idempotent on re-run.
