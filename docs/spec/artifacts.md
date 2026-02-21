## 📖 The HWM Artifact Pipeline Specification

### 1. Conceptual Framework

The `artifacts` pipeline is the **End-User Distribution Engine**. It transforms raw source code into verified, optimized, and compressed "Artifacts"—the payload (binary) and the seal (checksum)—ready for global distribution.

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

The CLI handles the execution context. While settings are in the YAML, the *destination* and *action* are defined at runtime.

#### **Command Syntax**

`hwm release artifacts [OPTIONS]`

#### **Execution Options**

| Option | Default | Description |
| --- | --- | --- |
| `--out <path>` | `.hwm/dist` | Staging directory. **Warning:** Wiped before every run. |
| `--publish` | `False` | Triggers GitHub Discovery and Asset Upload. |
| `--tag <name>` | `None` | The Git tag (e.g., `v0.1.2`) to target on GitHub. |
| `--token <key>` | `ENV` | GitHub Oauth token (Defaults to `GITHUB_TOKEN` env var). |

---

### 4. The Artifact Lifecycle (Internal Logic)

#### **Phase A: Preparation (Stateless Staging)**

HWM ensures a clean slate. It forcibly removes the `--out` directory and recreates it. This prevents "Ghost Releases" (old files accidentally included in new versions).

#### **Phase B: Compilation & Optimization**

HWM invokes `stack` or `cabal` with production injections:

* **Flags:** `-O2`, `-threaded`, `-split-sections`.
* **Stripping:** Removes DWARF symbols and metadata to reduce binary size by ~70%.

#### **Phase C: Resilient Bundling**

HWM iterates through the `formats` list.

* **ZIP (Native):** Guaranteed cross-platform support.
* **TarGz (System):** Preserves Unix `+x` executable permissions.
* **Fault Tolerance:** System calls (like `tar`) are wrapped in **Try-Catch** blocks. If `tar` is missing (common on legacy Windows), HWM logs a warning and continues with other formats.

#### **Phase D: Integrity Sealing**

Every successful archive triggers the generation of a **SHA-256 sidecar**.

* **Result:** `morpheus.zip` + `morpheus.zip.sha256`.

---

### 5. Cloud Handshake (GitHub Publishing)

If `--publish` is active, HWM transitions from local files to the GitHub API.

1. **Discovery:** Resolves the `--tag` to a GitHub `release_id`.
2. **Streaming:** Performs a high-speed multipart upload of both the **Payload** and the **Seal**.
3. **Verification:** Validates the HTTP 201 response for every asset before finishing.

---

### 🛠 Summary Table: Pipeline Resilience

| Failure Point | HWM Response |
| --- | --- |
| **Out Dir exists** | `removePathForcibly` (Wipes it clean). |
| **Tar command missing** | Logs warning, skips `.tar.gz`, proceeds to `.zip`. |
| **Network Failure** | Retries Discovery; aborts on persistent upload failure. |
| **Wrong Tag** | Fails early during Discovery Handshake. |
