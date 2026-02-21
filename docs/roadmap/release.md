You just had the ultimate "Senior Engineer" realization.
## 🚀 Feature Roadmap: The Release Pipeline (`hwm release`)

**Goal:** Eliminate the friction of cross-platform Haskell distribution. Provide a "Convention over Configuration" pipeline that compiles, optimizes, zips, and hashes binaries with zero boilerplate.

### Phase 1: The Zero-Boilerplate MVP (v0.1.0)

*Currently in Development*

The initial release focuses on a magical, zero-configuration experience. Developers simply map their desired output binary name to their internal Cabal target.

**Configuration:**

```yaml
# hwm.yaml
release:
  archive:
    morpheus: libs/code-gen:morpheus 
```

**Features under the hood:**

* **Automatic Target Resolution:** Instructs `stack`/`cabal` to build the specific target.
* **Smart Renaming:** Automatically renames `morpheus-cli.exe` to `morpheus.exe` inside the archive.
* **Native Zipping:** Uses a pure-Haskell, cross-platform engine (`zip-archive`) to bundle the binary.
* **Cryptographic Hashing:** Automatically generates a `.sha256` checksum file alongside the `.zip` for secure distribution.
* **Implicit Defaults:** Hardcoded to strip DWARF symbols for small binary sizes and use a standard `{{binary}}-{{os}}-{{arch}}.zip` naming convention.

---

### Phase 2: Power-User Overrides (v0.2.0)

*Planned*

Introduce polymorphic YAML parsing to allow advanced users to override the implicit defaults without changing the top-level schema.

**Configuration:**

```yaml
# hwm.yaml
release:
  archive:
    morpheus: libs/code-gen:morpheus

    legacy-api:                     # The advanced (Phase 2) usage
      source: legacy-server
      format: tar.gz                # Override the zip default
      strip: false                  # Keep debug symbols for this specific binary
      name_template: "{{binary}}-v{{version}}-ubuntu"

```

**Features:**

* **Format Selection:** Support for `.tar.gz` via the `tar` package.
* **Build Tuning:** Opt-out of stripping, or pass custom Cabal flags just for the release build.
* **Template Engine:** Allow dynamic injection of `{{version}}` (read from `package.yaml`) into the output file names.

---

### Phase 3: Beyond Archives (v0.3.0+)

*Future Vision*

Expand the top-level `release:` block to orchestrate the entire distribution lifecycle, perfectly decoupled from the local `workspace:` developer environment.

**Configuration:**

```yaml
release:
  archive:
    morpheus: morpheus-cli
  
  publish:
    hwm-core: true          # Automatically run `cabal sdist` and push to Hackage
  
  docker:
    auth-service: 
      registry: ghcr.io     # Automatically containerize the binary

```