{
  description = "HWM Managed Workspace - Reproducible CI and Release Artifacts";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, haskell-nix, ... }:
    let
      # 1. Define the specific systems HWM supports
      systems = [ 
        "x86_64-linux" 
        "aarch64-linux" 
        "x86_64-darwin" 
        "aarch64-darwin" 
      ];
      
      # 2. The "Pro" Helper: Loops through the systems array
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      # 3. We wrap 'packages' in our helper
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskell-nix) config;
            overlays = [ haskell-nix.overlay ];
          };

          # THE CI GROUP
          ciPackages = {
            hwm-ci-nix = (pkgs.haskell-nix.cabalProject {
              src = ./.;
              compiler-nix-name = "ghc963";
            }).hsPkgs.hwm.components.exes.hwm;
          };

          # THE RELEASE GROUP
          releaseArtifacts = let
            baseBinary = ciPackages.hwm-ci-nix; 
          in {
            hwm-portable = 
              if pkgs.stdenv.hostPlatform.isLinux then
                pkgs.pkgsStatic.haskell-nix.cabalProject {
                  src = ./.;
                  compiler-nix-name = "ghc963";
                }.hsPkgs.hwm.components.exes.hwm
              else if pkgs.stdenv.hostPlatform.isDarwin then
                pkgs.runCommand "hwm-macos-bundle" {
                  nativeBuildInputs = [ pkgs.macdylibbundler pkgs.darwin.autoSignDarwinBinariesHook ];
                } ''
                  mkdir -p $out/bin
                  cp ${baseBinary}/bin/hwm $out/bin/hwm
                  dylibbundler -b -x $out/bin/hwm -d $out/bin -p '@executable_path'
                  signDarwinBinariesInAllOutputs
                ''
              else baseBinary;
          };

        in
        # Return the package set for THIS specific system
        ciPackages // {
          default = ciPackages.hwm-ci-nix;
          release = pkgs.symlinkJoin {
            name = "hwm-release-set";
            paths = builtins.attrValues releaseArtifacts;
          };
        }
      );

      # 4. We also wrap 'devShells' in our helper
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskell-nix) config;
            overlays = [ haskell-nix.overlay ];
          };
        in
        {
          default = pkgs.haskell-nix.cabalProject {
            src = ./.;
            compiler-nix-name = "ghc963";
          }.shellFor {
            tools = { cabal = "latest"; };
          };
        }
      );
    };
}