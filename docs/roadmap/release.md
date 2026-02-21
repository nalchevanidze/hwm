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

## ✅ Plan

- [x] Map output binary names to Cabal targets in `hwm.yaml`.
- [x] Automatic target resolution for `stack`/`cabal`.
- [x] Smart renaming of binaries inside the archive.
- [x] Native zipping via pure-Haskell engine (`zip-archive`).
- [x] Generate `.sha256` checksum alongside `.zip`.
- [x] Use `{{binary}}-{{os}}-{{arch}}.zip` naming convention.
- [ ] Allow advanced YAML overrides (e.g., format, strip, name template).
- [ ] Support `.tar.gz` archives via `tar` package.
- [x] Orchestrate full distribution lifecycle in `release:` block.
- [ ] Add `publish:` for Hackage uploads (`cabal sdist`).
- [ ] Add `docker:` for containerizing binaries.
- [X] Build with `stack` or `cabal` using `-O2` and `-split-sections`.
- [ ] Move binaries to release directory.
- [x] Zip binaries as `<binary>-<os>-<arch>.zip`.
- [x] Generate SHA256 hash and `.sha256` sidecar file.
- [x] Coordinator job creates GitHub Release and `upload_url`.
- [x] Matrix workers upload artifacts using `hwm release archive --gh-publish <upload-url>`.
- [x] Upload `.zip` and `.sha256` to GitHub Release.
