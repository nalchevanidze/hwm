
## 📖 The HWM Artifact Pipeline: Complete Specification

### 1. Conceptual Framework

The `artifacts` pipeline is the **End-User Distribution Engine**. It transforms raw source code into verified, optimized, and compressed "Artifacts"—consisting of the **Payload** (the compressed binary) and the **Seal** (the SHA-256 checksum)—ready for global distribution.

---

### 2. Configuration (`hwm.yaml`)

HWM uses a **Hybrid-Flat** structure. Binary targets and pipeline settings share the same namespace for maximum readability.

```yaml
project: morpheus
version: 0.1.2

release:
  artifacts:
    # 1. Binary Targets (Name: Target)
    morpheus: libs/hwm-cli:morpheus
    hwm-daemon: apps/daemon:hwm-daemon

    # 2. Pipeline Settings (Shared)
    formats: 
      - Zip
      - TarGz
    name_template: "{{binary}}-v{{version}}-{{os}}-{{arch}}"

```

---

### 3. Command Execution & Options

The CLI determines the destination and execution mode.

#### **Command Syntax**

`hwm release artifacts [OPTIONS]`

#### **Execution Options**

| Option | Default | Description |
| --- | --- | --- |
| `--out <path>` | `.hwm/dist` | Staging directory. **Warning:** Wiped before every run. |
| `--gh-publish` | `False` | Triggers GitHub Discovery and Asset Upload. |
| `--format <format>` | `None` | The archive format to generate (e.g., `zip`, `tar.gz`). |
| `--ghc-options <options>` | `-O2 -threaded -split-sections` | Custom GHC flags for compilation. |
| `--name-template <template>` | `{{binary}}-v{{version}}-{{os}}-{{arch}}` | Template for naming artifacts. Placeholders: `{{binary}}`, `{{version}}`, `{{os}}`, `{{arch}}`. |
---

### 4. Internal Logic & Resilience

#### **Phase A: Preparation (Stateless Staging)**

HWM ensures a clean slate by invoking `prepareDistDir`. It forcibly removes the `--out` directory and recreates it. This prevents "Ghost Releases" where artifacts from previous builds accidentally persist.

#### **Phase B: Compilation & Optimization**

HWM invokes the build tool with production injections:

* **Flags:** `-O2`, `-threaded`, `-split-sections`.
* **Stripping:** Removes DWARF symbols and metadata to reduce binary size by ~70%.

#### **Phase C: Resilient Bundling**

HWM iterates through the `formats` list:

* **ZIP (Native):** Pure Haskell implementation for universal compatibility.
* **TarGz (System):** Preserves Unix `+x` permissions.
* **Fault Tolerance:** System calls (like `tar`) are wrapped in **Try-Catch** blocks. If `tar` is missing (common on minimal Windows environments), HWM logs a warning and continues with the remaining formats.

#### **Phase D: Integrity Sealing**

Every successful archive generates a **SHA-256 sidecar**.

* **Result:** `morpheus.zip` and `morpheus.zip.sha256`.

---

### 5. Cloud Handshake (GitHub Publishing)

If `--gh-publish` is active, HWM transitions to the GitHub API.

1. **Discovery:** Resolves the `--tag` to a GitHub `release_id` via `/releases/tags/:tag`.
2. **Streaming:** Performs high-speed uploads for both the **Payload** and the **Seal**.
3. **Verification:** Validates HTTP 201 responses for every asset.

---

### 🛠 Summary Table: Pipeline Resilience

| Failure Point | HWM Response |
| --- | --- |
| **Out Dir exists** | `removePathForcibly` (Wiped clean). |
| **Tar command missing** | Logs warning, skips `.tar.gz`, proceeds to `.zip`. |
| **Network Failure** | Aborts with error to prevent partial/broken releases. |
| **Missing Tag** | Fails early during Discovery Handshake. |
