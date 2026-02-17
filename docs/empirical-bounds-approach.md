# Outdated Command : Smart Bounds Detection(validation and fixing)

## Overview
This document details the approach for implementing empirical dependency bound checking and fixing in the `hwm outdated` command, leveraging existing utilities and toolchain integrations.

## Goals
- **Empirically derive** the minimum and maximum safe version bounds for each dependency, based on all environments in the matrix and the latest available snapshot (e.g., nightly).
- **Warn or fix** bounds that are too flexible (lower than tested) or too restrictive (upper less than latest available).
- **Provide granular, descriptive error and warning messages** to guide users.
- **Reuse existing utilities** for fetching versions, parsing snapshots, and updating bounds.

## Detailed Approach

### 1. Collecting Empirical Version Data
- For each dependency, gather all versions actually tested in all matrix environments.
  - Use existing matrix/environment utilities to extract tested versions.
  - The minimum and maximum of these versions become the empirically tested range.

### 2. Determining Latest Available Version
- Use `getSnapshotGHC` and similar logic to fetch the latest nightly snapshot.
- Parse the snapshot to get the latest available versions for all dependencies.
- This provides the reference for the maximum possible upper bound.

### 3. Bound Checking and Fixing
- For each dependency:
  - **Lower Bound:**
    - If the current lower bound is less than the minimum tested version, warn (and optionally fix).
  - **Upper Bound:**
    - If the current upper bound is less than the latest available version, warn (and optionally fix).
- Use existing helpers (e.g., `updateDepBounds`, `printUpperBound`, `getBound`) for bound manipulation and reporting.

### 4. Outdated Command Behavior
- `hwm outdated` (check mode):
  - Warn if bounds are too flexible or too restrictive, with clear, actionable messages.
  - Example: "Lower bound for `aeson` is 1.0.0, but the minimum tested is 1.4.4.0. Consider raising the lower bound."
  - Example: "Upper bound for `aeson` is 2.0.0, but the latest available is 2.2.0. Consider relaxing the upper bound."
- `hwm outdated --fix` (fix mode):
  - Update upper bounds to the latest available version.
  - By default, **do not increase lower bounds automatically** (to avoid breaking users who may rely on older versions), but provide a strong warning and an option to fix if desired.

### 5. Granular and Descriptive Messages
- All warnings and errors should:
  - Specify the dependency, the current bound, the empirically derived bound, and the recommended action.
  - Indicate whether the issue is a warning (safe to ignore) or an error (should be fixed for correctness).

### 6. Reuse and Extensibility
- Prefer using existing utilities and toolchain integrations for all version and bound operations.
- Only introduce new helpers if no suitable utility exists.

## Open Question: Should --fix Increase Lower Bounds?
- **Default:** Only warn about lower bounds that are too low, do not auto-increase them.
- **Rationale:** Raising lower bounds can break downstream users who depend on older versions. It is safer to let maintainers make this decision explicitly.
- **Option:** Provide a flag (e.g., `--fix-lower-bounds`) to allow maintainers to opt-in to automatic lower bound increases.

## Summary
This approach ensures that dependency bounds are empirically justified, up-to-date, and safe, while minimizing disruption and maximizing clarity for maintainers.
