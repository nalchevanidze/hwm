
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


You just had the ultimate "Senior Engineer" realization.

## 🚀 Feature Roadmap: The Release Pipeline (`hwm release`)

**Goal:** Eliminate the friction of cross-platform Haskell distribution. Provide a "Convention over Configuration" pipeline that compiles, optimizes, zips, and hashes binaries with zero boilerplate.

### Phase 1: The Zero-Boilerplate MVP (v0.1.0)

_Currently in Development_

The initial release focuses on a magical, zero-configuration experience. Developers simply map their desired output binary name to their internal Cabal target.

**Configuration:**

```yaml
# hwm.yaml
release:
  archive:
    morpheus: libs/code-gen:morpheus
```

**Features under the hood:**

- **Automatic Target Resolution:** Instructs `stack`/`cabal` to build the specific target.
- **Smart Renaming:** Automatically renames `morpheus-cli.exe` to `morpheus.exe` inside the archive.
- **Native Zipping:** Uses a pure-Haskell, cross-platform engine (`zip-archive`) to bundle the binary.
- **Cryptographic Hashing:** Automatically generates a `.sha256` checksum file alongside the `.zip` for secure distribution.
- **Implicit Defaults:** Hardcoded to strip DWARF symbols for small binary sizes and use a standard `{{binary}}-{{os}}-{{arch}}.zip` naming convention.

---

### Phase 2: Power-User Overrides (v0.2.0)

_Planned_

Introduce polymorphic YAML parsing to allow advanced users to override the implicit defaults without changing the top-level schema.

**Configuration:**

```yaml
# hwm.yaml
release:
  archive:
    morpheus: libs/code-gen:morpheus

    legacy-api: # The advanced (Phase 2) usage
      source: legacy/app:server
      format: tar.gz # Override the zip default
      strip: false # Keep debug symbols for this specific binary
      name_template: "{{binary}}-v{{version}}-ubuntu"
```

**Features:**

- **Format Selection:** Support for `.tar.gz` via the `tar` package.
- **Build Tuning:** Opt-out of stripping, or pass custom Cabal flags just for the release build.
- **Template Engine:** Allow dynamic injection of `{{version}}` (read from `package.yaml`) into the output file names.

---

### Phase 3: Beyond Archives (v0.3.0+)

_Future Vision_

Expand the top-level `release:` block to orchestrate the entire distribution lifecycle, perfectly decoupled from the local `workspace:` developer environment.

**Configuration:**

```yaml
release:
  archive:
    morpheus: morpheus-cli

  publish:
    hwm-core: true # Automatically run `cabal sdist` and push to Hackage

  docker:
    auth-service:
      registry: ghcr.io # Automatically containerize the binary
```

# Iteration 3: The "Release Pipeline Orchestrator"

## 🛠 HWM Release Workflow: The "Deep Dive"

The workflow is designed to be **Stateless** and **Matrix-Safe**. It ensures that no matter how many operating systems you are building for, the result is consistent and secure.

### 1. The Configuration Phase (Declarative)

The developer defines the "What" in the `hwm.yaml`.

- **Target Mapping:** Maps a friendly name (e.g., `morpheus`) to a specific workspace and executable (`libs/hwm:hwm`).
- **Implicit Defaults:** HWM automatically assumes production optimizations (`-O2`), code stripping, and `zip` formatting unless overridden.

### 2. The Build & Optimize Phase (Local)

HWM invokes the Haskell build tool (`stack` or `cabal`) with "Production Injection":

- **GHC Flags:** HWM forces `-O2` (max optimization) and `-split-sections` (to prepare for dead-code elimination).
- **Binary Stripping:** Once compiled, HWM runs the `strip` command (or the internal linker equivalent) to remove all debug symbols and metadata.
- _Result:_ The binary size typically drops by **70-80%**.

### 3. The Archiving & Security Phase (Local)

HWM moves the optimized binary into a temporary release directory and secures it:

- **Zipping:** Creates `morpheus-<os>-<arch>.zip`.
- **Hashing:** HWM runs the `SHA256` algorithm over the `.zip` file.
- **Sidecar Generation:** It creates a small text file, `morpheus-<os>-<arch>.zip.sha256`, containing only the hash. This allows users to verify the download without unzipping it first.

### 4. The Orchestration Phase (CI/CD)

This is where the "Boss and Worker" pattern handles the GitHub Matrix:

- **The Coordinator (Boss):** A single CI job creates a GitHub Release and generates an `upload_url`.
- **The Matrix (Workers):** Multiple machines (Linux, macOS, Windows) receive that URL.
- **The Command:** `hwm release archive --gh-publish <upload-url>`

### 5. The Publishing Phase (Cloud)

If the `--gh-publish` flag is present, HWM performs a high-speed streaming upload:

- **Binary Upload:** Sends the `.zip` to the GitHub `upload_url`.
- **Checksum Upload:** Sends the `.sha256` file to the same release.

## 🏗 Why this works better than manual scripts

| Step             | Manual Way                          | HWM Way                                 |
| ---------------- | ----------------------------------- | --------------------------------------- |
| **Optimization** | Forget to add `-O2` or `-threaded`. | Automatically injected.                 |
| **Size**         | 120MB binary (too big for GitHub).  | 18MB binary (stripped & zipped).        |
| **Security**     | No checksum provided.               | Automatic `.sha256` sidecar.            |
| **Matrix**       | 3 OSs fight over the same Release.  | Workers target a specific `upload_url`. |
