
## 📖 Specification: The HWM Release Pipeline

### 1. Overview

The `hwm release` subsystem provides a declarative, platform-aware pipeline for transforming raw Haskell binaries into production-ready distribution assets. It automates the "Optimization-Archive-Checksum-Publish" lifecycle.

### 2. The Artifact Lifecycle

#### 2.1 Preparation (The "Clean" Step)

Before any build begins, HWM ensures a stateless environment:

* **Target:** The directory defined in `out_dir` (Default: `.hwm/dist`).
* **Action:** Forcibly removes the directory and recreates it.
* **Reason:** Prevents stale binaries or mismatched checksums from previous versions leaking into new releases.

#### 2.2 Compilation (Optimization Injection)

HWM triggers the underlying build tool (`stack` or `cabal`) with a "Production Profile."

* **Mandatory Flags:**
* `-O2`: Maximum high-level optimization.
* `-split-sections`: Enables the linker to discard unreachable code.
* `-threaded`: Ensures the binary supports multicore runtime for modern CLIs.


* **Stripping:** After compilation, HWM invokes the linker's strip flag (`-optl-s`) or a system `strip` command to remove DWARF debug symbols, typically reducing binary size by ~70%.

#### 2.3 Archiving (The "Auto" Logic)

HWM supports multiple container formats. The pipeline iterates through the `formats` list provided in `hwm.yaml`.

| Format | Engine | OS Behavior | Benefit |
| --- | --- | --- | --- |
| **Zip** | Native Haskell | Cross-platform | Universal extraction; standard for Windows. |
| **TarGz** | System `tar` | Resilient/Try-Catch | Preserves Unix `+x` permissions; industry standard for Linux/macOS. |

* **Resiliency Layer:** If a system-level archive command (like `tar`) fails, HWM logs a warning but continues with other requested formats, ensuring the pipeline is not halted by environmental missing dependencies.

#### 2.4 Verification (Sidecar Generation)

For every archive generated, HWM produces a **Cryptographic Sidecar**:

* **Algorithm:** SHA-256.
* **Output:** A `.sha256` file containing the hex-encoded hash.
* **Naming:** Matches the archive name exactly (e.g., `hwm-v0.1.0.zip.sha256`).

---

### 3. GitHub Orchestration (The "Handshake")

#### 3.1 The Discovery Phase

To avoid hardcoding internal IDs, HWM performs a **Discovery Handshake**:

1. **Input:** Git Tag (e.g., `v0.1.2`) and `GITHUB_TOKEN`.
2. **Request:** Queries the GitHub API: `/repos/:owner/:repo/releases/tags/:tag`.
3. **Extraction:** Plucks the `id` and `upload_url` from the JSON response.

#### 3.2 The Publishing Phase

HWM iterates through the `.hwm/dist` manifest and performs a high-speed streaming upload to the discovered `upload_url`.

* **Asset Pairings:** Every binary archive is uploaded alongside its corresponding `.sha256` file.
* **Authentication:** Requires a `Bearer` token in the `Authorization` header.

### 4. Configuration Schema (YAML)

```yaml
release:
  archive:
    morpheus: libs/code-gen:morpheus # Map binary name to Cabal target
    
    # Optional Overrides
    archive_options:
      formats: [Zip, TarGz]         # List of desired formats
      name_template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"

```