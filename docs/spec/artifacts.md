

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
| `--output-dir <path>` | `.hwm/dist` | Staging directory (wiped before every run). |
| `--gh-publish <upload-url>` | `none` | Triggers GitHub asset upload with provided upload URL. |
| `--format <format>` | `none` | Artifact format to generate (`zip`, `tar.gz`, etc.). |
| `--ghc-options <options>` | `-O2 -threaded -split-sections` | Custom GHC flags. |
| `--name-template <template>` | `{{binary}}-v{{version}}-{{os}}-{{arch}}` | Artifact naming template. |

---

## 4. Pipeline Phases

### A. Preparation
- Output directory (`--output-dir`) is forcibly wiped and recreated before each run.

### B. Compilation & Optimization
- Builds use production flags: `-O2 -threaded -split-sections`.
- Static linking is supported (configurable).

### C. Bundling & Fault Tolerance
- Artifacts are created in all configured formats (e.g., Zip, TarGz).
- Zip archiving is performed natively in Haskell (pure Haskell, no system dependencies).
- TarGz archiving is OS-specific and uses a system tool (e.g., `tar`).
- If a tar format fails, HWM logs a warning and continues with other formats.

### D. Integrity Sealing
- Each artifact is accompanied by a `.sha256` checksum file.
- Example: `morpheus.zip` and `morpheus.zip.sha256`.


## 5. GitHub Publishing

If `--gh-publish <upload-url>` is set, HWM:
2. Uploads artifacts and its checksum Verifies successful upload (HTTP 201).

## 6. Resilience & Error Handling

| Failure Point | HWM Response |
| --- | --- |
| Output dir exists | Wiped clean before build. |
| Format tool missing | Logs warning, skips format, continues. |
| Network failure | Aborts to prevent partial releases. |
| Missing GitHub tag | Fails early with error. |



## Key Capabilities

- Explicit target mapping: decouple internal package names from shipped binaries.
- Smart defaults, but fully configurable for advanced use.
- Native, zero-dependency artifact creation, hashing, and publishing.
- Universal handoff for CI/CD, Homebrew, Nix, AUR, etc.