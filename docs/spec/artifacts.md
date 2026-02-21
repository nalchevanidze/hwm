

# 📦 HWM Artifacts Pipeline: Unified Specification (2026)

## 1. Conceptual Overview

The HWM artifacts pipeline is the end-user distribution engine. It transforms source code into verified, optimized, and compressed artifacts—each consisting of a payload (the compressed binary) and a seal (SHA-256 checksum)—ready for global distribution. All logic is implemented natively in Haskell for maximum portability and CI/CD integration.

---

## 2. Configuration (`hwm.yaml`)

HWM uses a hybrid-flat YAML structure. Binary targets and pipeline settings share the same namespace for clarity and simplicity.

```yaml
project: morpheus
version: 0.1.2

release:
  artifacts:
    morpheus: libs/hwm-cli:morpheus
    hwm-daemon: apps/daemon:hwm-daemon
    formats: [Zip, TarGz]
    name_template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"
```

#### Advanced Example

```yaml
release:
  artifacts:
    morpheus: libs/code-gen:morpheus
    legacy-api:
      source: legacy/app:server
      formats: [TarGz]
      name_template: "{{binary}}-v{{version}}-ubuntu"
      include: [custom-config.yaml]
```

---

## 3. CLI Usage & Options

### Command

```
hwm release artifacts [OPTIONS]
```

### Options

| Option | Default | Description |
| --- | --- | --- |
| `--out <path>` | `.hwm/dist` | Staging directory (wiped before every run). |
| `--gh-publish` | `False` | Triggers GitHub asset upload. |
| `--format <format>` | `None` | Artifact format to generate (`zip`, `tar.gz`, etc.). |
| `--ghc-options <options>` | `-O2 -threaded -split-sections` | Custom GHC flags. |
| `--name-template <template>` | `{{binary}}-v{{version}}-{{os}}-{{arch}}` | Artifact naming template. |

---

## 4. Pipeline Phases

### A. Preparation
- Output directory (`--out`) is forcibly wiped and recreated before each run.

### B. Compilation & Optimization
- Builds use production flags: `-O2 -threaded -split-sections`.
- Static linking is supported (configurable).

### C. Bundling & Fault Tolerance
- Artifacts are created in all configured formats (e.g., Zip, TarGz).
- Zip archiving is performed natively in Haskell (pure Haskell, no system dependencies).
- TarGz archiving is OS-specific and uses a system tool (e.g., `tar`).
- If a format fails, HWM logs a warning and continues with other formats.

### D. Integrity Sealing
- Each artifact is accompanied by a `.sha256` checksum file.
- Example: `morpheus.zip` and `morpheus.zip.sha256`.

### E. Asset Bundling
- LICENSE and README.md are included in the artifact if present.
- Additional files can be included via configuration.

---

## 5. GitHub Publishing

If `--gh-publish` is set, HWM:
1. Resolves the release tag to a GitHub release ID.
2. Uploads both the artifact and its checksum.
3. Verifies successful upload (HTTP 201).

---

## 6. Resilience & Error Handling

| Failure Point | HWM Response |
| --- | --- |
| Output dir exists | Wiped clean before build. |
| Format tool missing | Logs warning, skips format, continues. |
| Network failure | Aborts to prevent partial releases. |
| Missing GitHub tag | Fails early with error. |

---

## 7. CI/CD & Universal Integration

- Artifacts pipeline is CI-agnostic: output paths can be exported for use in any CI/CD system.
- Smart platform detection for naming (`{{os}}`, `{{arch}}`).
- All steps are pure Haskell for maximum portability.

---

## 8. Key Capabilities

- Explicit target mapping: decouple internal package names from shipped binaries.
- Smart defaults, but fully configurable for advanced use.
- Native, zero-dependency artifact creation, hashing, and publishing.
- Universal handoff for CI/CD, Homebrew, Nix, AUR, etc.

---

This document supersedes all previous references to "archive" with "artifacts" and is aligned with the current codebase and CLI as of 2026.
