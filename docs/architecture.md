# HWM Architecture Documentation

**Target Audience:** AI Agents, System Architects, Core Contributors
**Version:** 0.0.1
**Last Updated:** February 15, 2026

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                       CLI Entry Point                        │
│                        (app/Main.hs)                         │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                 Command Parser & Router                      │
│                 (HWM.CLI.App + Command)                      │
├─────────────────────────────────────────────────────────────┤
│   init | sync | run | outdated | version | publish | status │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                 Configuration Monad (ConfigT)                │
│              State: Config + Cache + Options                 │
└────────────────────────┬────────────────────────────────────┘
                         │
        ┌───────────────┼───────────────┬───────────────┐
        ▼               ▼               ▼               ▼
    ┌────────┐     ┌──────────┐    ┌─────────┐    ┌──────────┐
    │ Domain │     │Toolchain │    │ Runtime │    │   Core   │
    │ Models │     │Integration    │ Services│    │ Utilities│
    └────────┘     └──────────┘    └─────────┘    └──────────┘
```

---

## Layer Responsibilities

### Layer 1: CLI (HWM.CLI.*)

**Purpose:** User interface, argument parsing, command dispatch

**Modules:**
- App.hs - Main entry, optparse-applicative setup
- Command.hs - Command dispatcher
- Init.hs - Bootstrap hwm.yaml generation
- Sync.hs - Sync implementation
- Run.hs - Script execution
- Outdated.hs - Dependency updates
- Version.hs - Version management
- Publish.hs - Hackage publishing
- Status.hs - Status display

**Key Types:**
```haskell
data Command
  = Init {initOptions :: InitOptions}
  | Sync {tag :: Maybe Name}
  | Run {runOptions :: ScriptOptions}
  | Outdated {fix :: Bool}
  | Version {bump :: Maybe Bump}
  | Publish {groupName :: Maybe Name}
  | Status

data InitOptions = InitOptions
  { forceOverride :: Bool
  , projectName :: Maybe Text
  }

data ScriptOptions = ScriptOptions
  { scriptName :: Name
  , scriptTargets :: [Name]
  , scriptEnvs :: [Name]
  , scriptOptions :: [Text]
  }

data Options = Options
  { quiet :: Bool
  , stack :: FilePath
  , hie :: FilePath
  , config :: FilePath
  }
```

---

### Layer 2: Domain (HWM.Domain.*)

**Purpose:** Core business logic, domain models

#### Config.hs

```haskell
data Config = Config
  { name :: Name, version :: Version, bounds :: Bounds
  , workspace :: [WorkspaceGroup], matrix :: Matrix
  , registry :: Dependencies, scripts :: Map Name Text
  }

getRule :: PkgName -> PkgRegistry -> Config -> m Bounds
```

#### Workspace.hs

```haskell
data WorkspaceGroup = WorkspaceGroup
  { name :: Name, dir :: Maybe FilePath, members :: [Name]
  , prefix :: Maybe Text, publish :: Maybe Bool
  }

type PkgRegistry = Map PkgName WorkspaceGroup

-- Resolves directory as: {dir}/{prefix}-{member}
memberPkgs :: WorkspaceGroup -> m [Pkg]
resolveTargets :: [WorkspaceGroup] -> [Name] -> m [Pkg]
```

#### Matrix.hs

```haskell
data Matrix = Matrix
  { defaultEnvironment :: Name, environments :: [BuildEnv] }

data BuildEnv = BuildEnv
  { name :: Name, ghc :: Version, resolver :: Name
  , extraDeps :: Maybe (Map PkgName Version)
  , exclude :: Maybe [Text], allowNewer :: Maybe Bool
  }

data BuildEnvironment = BuildEnvironment
  { buildEnv :: BuildEnv, buildPkgs :: [Pkg]
  , buildName :: Name, buildExtraDeps :: Maybe VersionMap
  , buildResolver :: Name
  }

getBuildEnvironment :: Maybe Name -> m BuildEnvironment
getBuildEnvroments :: m [BuildEnvironment]
```

#### Dependencies.hs & Bounds.hs

```haskell
data Dependency = Dependency { name :: PkgName, bounds :: Bounds }
newtype Dependencies = Dependencies (Map PkgName Bounds)

data Restriction = Min | Max
data Bound = Bound { restriction :: Restriction, orEquals :: Bool, version :: Version }
newtype Bounds = Bounds [Bound]

-- Example: Version 0.28.0 -> ">=0.28.0 && <0.29.0"
versionBounds :: Version -> Bounds
```

#### ConfigT.hs

```haskell
data Env m = Env
  { options :: Options
  , config :: Config
  , cache :: Cache
  , pkgs :: PkgRegistry
  }

newtype ConfigT a = ConfigT (ReaderT Env (ResultT (UIT IO)) a)

runConfigT :: ConfigT () -> Options -> IO ()
updateConfig :: (Config -> ConfigT Config) -> ConfigT b -> ConfigT b
```

**Features:**
- Read-only environment via ReaderT
- Mutable cache (TVar-backed)
- Error accumulation (ResultT)
- UI operations via UIT monad
- Has type class for polymorphic access

---

### Layer 3: Integration (HWM.Integrations.Toolchain.\*)

**Purpose:** External tool integration

#### Package.hs

**package.yaml Synchronization:**

### Layer 3: Integration (HWM.Integrations.Toolchain.*)

**Purpose:** External tool integration

#### Package.hs - package.yaml Synchronization

```haskell
data Package = Package
  { name :: PkgName, version :: Version, library :: Maybe Library
  , dependencies :: Dependencies, tests :: Maybe Libraries
  , executables :: Maybe Libraries, benchmarks :: Maybe Libraries
  , internalLibraries :: Maybe Libraries, foreignLibraries :: Maybe Libraries
  }

syncPackages :: ConfigT ()
updatePackage :: Pkg -> Maybe Package -> ConfigT Package
```

#### Stack.hs - stack.yaml Generation

```haskell
data Stack = Stack
  { packages :: [FilePath], resolver :: Name
  , allowNewer :: Maybe Bool, saveHackageCreds :: Maybe Bool
  , extraDeps :: Maybe [Name], compiler :: Maybe Text
  }

syncStackYaml :: ConfigT ()
createEnvYaml :: Name -> ConfigT ()
sdist :: Pkg -> ConfigT [Issue]
upload :: Pkg -> ConfigT (Status, [Issue])
```

#### Hie.hs - hie.yaml Generation

```haskell
data Component = Component { path :: FilePath, component :: Name }

syncHie :: ConfigT ()
genComponents :: Pkg -> ConfigT [Component]
```

Maps lib → `{pkg}:lib`, test → `{pkg}:test:{name}`, exe → `{pkg}:exe:{name}`

#### Cabal.hs & Lib.hs

```haskell
-- Invokes hpack library to generate .cabal from package.yaml
syncCabal :: Pkg -> ConfigT Status

data Library = Library
  { sourceDirs :: Name, dependencies :: Maybe Dependencies
  , __unknownFields :: Maybe Object  -- Preserves unknown fields
  }

updateLibrary :: Library -> ConfigT Library
```

---

### Layer 4: Runtime (HWM.Runtime.*)

**Purpose:** System interactions (I/O, processes, state)

#### Cache.hs - State & Hackage Queries

```haskell
data Registry = Registry
  { currentEnv :: Name
  , versions :: Map PkgName (NonEmpty Version)
  }

newtype Cache = Cache (TVar Registry)

loadCache :: Name -> IO Cache
saveCache :: Cache -> IO ()
getVersions :: PkgName -> m (NonEmpty Version)
```

Caches Hackage queries at `https://hackage.haskell.org/package/{pkg}/preferred`

#### Files.hs - File I/O

```haskell
readYaml :: FromJSON a => FilePath -> m a
writeYaml :: ToJSON a => FilePath -> a -> m ()
rewrite_ :: (FromJSON a, ToJSON b) => FilePath -> (Maybe a -> m b) -> m ()

-- YAML options: camelCase → kebab-case, omit Nothing fields
aesonYAMLOptions :: Options
```

#### Process.hs - Command Execution

```haskell
inheritRun :: FilePath -> Text -> ConfigT ()  -- Interactive
silentRun :: FilePath -> Text -> ConfigT (Bool, Text)  -- Captured
```

#### Logging.hs & UI.hs

```haskell
-- Log to .hwm/logs/{env}-{timestamp}.log
log :: Name -> [(Text, Text)] -> Text -> ConfigT FilePath
logError :: Name -> [(Text, Text)] -> Text -> ConfigT FilePath

-- Formatted terminal output
section :: Text -> m a -> m a
sectionTableM :: Int -> Text -> [(Text, m Text)] -> m ()
statusIndicator :: Int -> Text -> Text -> m ()
```

---

### Layer 5: Core (HWM.Core.*)

**Purpose:** Shared utilities, type classes

```haskell
-- Common.hs
type Name = Text
class Check m a where check :: a -> m ()

-- Version.hs
data Version = Version { major :: Int, minor :: Int, revision :: [Int] }
data Bump = Major | Minor | Patch
nextVersion :: Bump -> Version -> Version

-- Pkg.hs
data Pkg = Pkg { pkgName :: PkgName, pkgVersion :: Version, pkgGroup :: Name, pkgMemberId :: Name, pkgDirPath :: FilePath }
newtype PkgName = PkgName Text
pkgId :: Pkg -> Text  -- Derived: pkgGroup <> "/" <> pkgMemberId

-- Result.hs
data Issue = Issue { issueTopic :: Name, issueMessage :: Text, issueSeverity :: Severity, issueDetails :: Maybe IssueDetails }
data Severity = SeverityWarning | SeverityError
type ResultT m a = ExceptT Issue m a

-- Has.hs
class Has env a where obtain :: env -> a
askEnv :: (MonadReader env m, Has env a) => m a

-- Formatting.hs & Parsing.hs
class Format a where format :: a -> Text
class Parse a where parse :: Text -> m a
data Color = Red | Green | Yellow | Cyan | Magenta | Gray | Bold
chalk :: Color -> Text -> Text
```

---
```

#### Logging.hs

**Error Logging:**

```haskell
log :: Name -> [(Text, Text)] -> Text -> ConfigT FilePath
logError :: Name -> [(Text, Text)] -> Text -> ConfigT FilePath
```

**Log Format:**

```
=== ENVIRONMENT ===
stable (9.6.3)

=== COMMAND ===
stack build morpheus-graphql-core --fast

=== OUTPUT ===
[... captured output ...]
```

**Log Location:** `.hwm/logs/{env}-{timestamp}.log`

#### UI.hs

**Terminal Output:**

```haskell
-- Section headers
section :: Text -> m a -> m a
sectionWorkspace :: m a -> m a
sectionEnvironments :: m a -> m a

-- Tables
sectionTableM :: Int -> Text -> [(Text, m Text)] -> m ()
forTable :: Int -> [a] -> (a -> (Text, Text)) -> m ()

-- Status indicators
statusIndicator :: Int -> Text -> Text -> m ()
putLine :: Text -> m ()
indent :: Int -> m a -> m a
```

**Output Format:**

```
┌─ section title ───────────────────────────────
│ key ··········· value
│ another-key ··· another value
├─┬─ nested section ────────────────────────────
│ │ item ········· ✓
│ └──────────────────────────────────────────────
```

---

### Layer 5: Core (HWM.Core.*)

**Purpose:** Shared utilities, type classes

```haskell
-- Common.hs
type Name = Text
class Check m a where check :: a -> m ()

-- Version.hs
data Version = Version { major :: Int, minor :: Int, revision :: [Int] }
data Bump = Major | Minor | Patch
nextVersion :: Bump -> Version -> Version

-- Pkg.hs
data Pkg = Pkg { pkgName :: PkgName, pkgVersion :: Version, pkgGroup :: Name, pkgMemberId :: Name, pkgDirPath :: FilePath }
newtype PkgName = PkgName Text
pkgId :: Pkg -> Text  -- Derived: pkgGroup <> "/" <> pkgMemberId

-- Result.hs
data Issue = Issue { issueTopic :: Name, issueMessage :: Text, issueSeverity :: Severity, issueDetails :: Maybe IssueDetails }
data Severity = SeverityWarning | SeverityError
type ResultT m a = ExceptT Issue m a

-- Has.hs
class Has env a where obtain :: env -> a
askEnv :: (MonadReader env m, Has env a) => m a

-- Formatting.hs & Parsing.hs
class Format a where format :: a -> Text
class Parse a where parse :: Text -> m a
data Color = Red | Green | Yellow | Cyan | Magenta | Gray | Bold
chalk :: Color -> Text -> Text
```

---

## Data Flow Diagrams

### Init Command Flow

```
User: hwm init [-f|--force] [NAME]
        │
        ├─> Parse Options
        │   └─> InitOptions { forceOverride, projectName }
        │
        ├─> Check Preconditions
        │   └─> Verify hwm.yaml doesn't exist (or --force)
        │
        ├─> Discover Stack Files
        │   ├─> Find stack.yaml, stack-*.yaml
        │   └─> Skip .hwm/matrix/stack-*.yaml
        │
        ├─> Parse Stack Configurations & Discover Packages
        │   ├─> Extract resolver, packages, extra-deps, allow-newer
        │   └─> Resolve GHC version from resolver
        │
        ├─> Analyze & Group Packages
        │   ├─> Detect common prefixes and directory patterns
        │   └─> Build WorkspaceGroup list
        │
        ├─> Infer Metadata
        │   ├─> Project name (from arg or directory name)
        │   ├─> Project version (max from packages)
        │   └─> Generate version bounds
        │
        ├─> Build Matrix & Registry
        │   ├─> Create BuildEnv for each stack file
        │   ├─> Derive registry from package dependencies
        │   └─> Generate default scripts
        │
        ├─> Write Config
        │   └─> Save hwm.yaml with hash
        │
        └─> Run Status
            └─> Display initial workspace state
```

**Key Functions:**
```haskell
initWorkspace :: InitOptions -> Options -> IO ()
scanStackFiles :: (MonadIO m, MonadError Issue m) => Options -> FilePath -> m (NonEmpty (Name, Stack))
deriveRegistry :: (Monad m, MonadError Issue m, MonadIO m) => [Pkg] -> m (Dependencies, DependencyGraph)
buildMatrix :: [Pkg] -> [FilePath] -> IO Matrix
buildWorkspaceGroups :: (Monad m, MonadError Issue m) => DependencyGraph -> [Pkg] -> m [WorkspaceGroup]
```

### Sync Command Flow

1. Parse tag (environment name)
2. Load configuration and cache
3. Get/set build environment
4. Generate files:
   - `syncStackYaml` → stack.yaml
   - `syncHie` → hie.yaml
   - `syncPackages` → package.yaml + .cabal files
5. Save cache

### Run Command Flow

1. Parse script name, targets, environments, args
2. Resolve script template from config
3. Resolve targets (packages) and environments
4. Create environment-specific stack files in `.hwm/matrix/`
5. Execute in parallel (if multi-env) or sequentially
6. Capture output to `.hwm/logs/{env}.log`
7. Display summary (exit 1 if failures)

### Outdated Command Flow

1. Clear version cache (if `--fix`)
2. For each dependency: query Hackage, compare with current bounds
3. If `--fix`: update hwm.yaml registry, recompute hash, run sync
4. Display outdated packages

---

## Type System Architecture

### Monad Stack

```
ConfigT a
  = ReaderT Env (ResultT (UIT IO)) a
  = ReaderT Env (ExceptT Issue (UIT IO)) a
```

**Capabilities:**
- `MonadReader Env`: Access config, cache, options, pkgs
- `MonadError Issue`: Error handling with accumulation
- `MonadIO`: File I/O, process execution, network requests
- `MonadUI`: Formatted terminal output with indentation

### Has Type Class Pattern

```haskell
class Has env a where
  obtain :: env -> a

-- Instances for Env
instance Has (Env m) Config where obtain = config
instance Has (Env m) Cache where obtain = cache
instance Has (Env m) Options where obtain = options
instance Has (Env m) [WorkspaceGroup] where obtain = workspace . config
instance Has (Env m) Matrix where obtain = matrix . config

-- Usage
askEnv :: (MonadReader env m, Has env a) => m a
askEnv = asks obtain
```

**Benefits:** Type-safe polymorphic environment access

### Error Handling Pattern

```haskell
data Issue = Issue
  { issueTopic :: Name
  , issueMessage :: Text
  , issueSeverity :: Severity
  , issueDetails :: Maybe IssueDetails
  }

data Severity = SeverityWarning | SeverityError

-- Fatal error
throwError :: Issue -> ConfigT a

-- Warning (continues)
injectIssue :: Issue -> ConfigT ()

-- Result accumulation
type ResultT m a = ExceptT Issue m a
```

---

## File Generation

### Atomic Rewrite Pattern

```haskell
rewrite_ :: (FromJSON a, ToJSON b) => FilePath -> (Maybe a -> m b) -> m ()
rewrite_ path transform = do
  existing <- tryReadYaml path
  new <- transform existing
  temp <- generateTempPath path
  writeYaml temp new
  renameFile temp path  -- Atomic on POSIX
```

Ensures no partial writes, preserves file on error.

### Unknown Field Preservation

```haskell
data Library = Library
  { sourceDirs :: Name
  , dependencies :: Maybe Dependencies
  , __unknownFields :: Maybe Object  -- Preserves unknown YAML fields
  }
```

Round-trip preserves fields not defined in Haskell types.

---

## State Management

### Cache (TVar-Based)

```haskell
newtype Cache = Cache (TVar Registry)

data Registry = Registry
  { currentEnv :: Name
  , versions :: Map PkgName (NonEmpty Version)
  }

-- Read (no lock)
getRegistry :: m Registry

-- Modify (atomic)
updateRegistry :: (Registry -> Registry) -> m ()

-- Persist to .hwm/cache/state.json
saveCache :: Cache -> IO ()
```

### Hash Integrity

```haskell
-- Compute SHA256 hash of config
computeHash :: Config -> Text

-- Verify on load, warn on mismatch
hasHashChanged :: Config -> Maybe Text -> Bool

-- Write with hash comment
saveConfig :: Config -> Options -> m ()
```

Detects manual edits to hwm.yaml.

---

## Performance & Concurrency

### Caching Strategy
- **Config:** Loaded once, immutable during execution
- **PkgRegistry:** Built once from workspace groups
- **Hackage versions:** Cached in TVar, persisted to `.hwm/cache/state.json`
- **File system:** Relies on OS page cache

### Parallel Execution

```haskell
-- Multi-environment runs use async
runMultiEnv envs cmd = do
  tasks <- for envs $ \env -> async (runInEnv env cmd)
  results <- traverse wait tasks
  analyzeResults results
```

- Each environment uses separate stack invocation
- Outputs logged to `.hwm/logs/{env}.log`
- Cache TVar provides thread-safe shared state

---

## Security

### File System
- Atomic writes via temp file + rename
- Path normalization prevents traversal
- No overwrites without explicit force flags

### Process Execution
- Uses `typed-process` (no shell injection)
- Explicit STACK_YAML environment variable
- Output captured and sanitized

### Network
- HTTPS-only via `req` library
- Credentials never logged

---

## Extension Guide

### Adding New Commands

1. Define command in `HWM.CLI.Command`:
   ```haskell
   data Command = ... | MyCmd {myOpts :: MyOptions}
   ```

2. Add parser in `HWM.CLI.App`:
   ```haskell
   parseCommand = commands [
     ...
     , ("mycmd", "Description", MyCmd <$> parseMyOptions)
   ]
   ```

3. Implement handler in `HWM.CLI.Command.MyCommand`:
   ```haskell
   runMyCommand :: MyOptions -> ConfigT ()
   ```

4. Add to dispatcher in `HWM.CLI.Command`:
   ```haskell
   command (MyCmd opts) = runMyCommand opts
   ```

### Adding Config Fields

1. Update `Config` in `HWM.Domain.Config`:
   ```haskell
   data Config = Config { ..., myField :: MyType }
   deriving (Generic, FromJSON, ToJSON)
   ```

2. Add validation if needed:
   ```haskell
   instance Check m Config where
     check Config{myField} = validateMyField myField
   ```

### Adding Toolchain Integration

1. Create module `HWM.Integrations.Toolchain.MyTool`
2. Define sync function:
   ```haskell
   syncMyTool :: ConfigT ()
   ```
3. Call from `HWM.CLI.Command.Sync`

---

## Troubleshooting

| Issue | Cause | Solution |
|-------|-------|----------|
| "Package not found" | Incorrect workspace resolution | Verify `dir` + `prefix` + `member` paths |
| "Hash mismatch" | Manual hwm.yaml edit | Run `hwm sync` to recompute hash |
| "Environment not found" | Invalid environment name | Check `matrix.environments[].name` |
| "Hackage timeout" | Network/rate limiting | Check `.hwm/cache/state.json` or retry |

---

## Dependency Graph

```
Core (no internal deps)
  ↓
Runtime (depends on Core)
  ↓
Domain (depends on Core + Runtime)
  ↓
Integrations (depends on Domain)
  ↓
CLI (depends on all)
```

**Detailed:**

```
Common ← Version ← Pkg ← Formatting ← Parsing ← Result ← Has ← Options
  ↓
Cache ← Files ← Process ← Logging ← UI
  ↓
Bounds ← Dependencies ← Workspace ← Matrix ← Config ← ConfigT
  ↓
Lib ← Package ← Stack ← Hie ← Cabal
  ↓
Command.* ← Command ← App ← Main
```

---

---

**Document Version:** 1.0  
**HWM Version:** 0.0.1  
**Completeness:** Comprehensive (all subsystems documented)  
**Status:** Production Ready
