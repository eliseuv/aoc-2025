{
  description = "AoC 2025 Haskell Dev Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        systemLibs = with pkgs; [
          zlib
          zlib.dev
          ncurses
          gmp
        ];

        hpkgs = pkgs.haskell.packages.ghc96;
        ghcPackages = with hpkgs; [
          ghc
          haskell-language-server
        ];

        helperDevelop = pkgs.writeShellScriptBin "dev" ''
          ${pkgs.lib.getExe pkgs.ghcid} --command="cabal repl" --test="Main.dispatch $1 \"\$(cat data/day$(printf "%02d" $1).txt)\""
        '';

        helperRun = pkgs.writeShellScriptBin "run" ''
          ${pkgs.lib.getExe pkgs.cabal-install} run aoc2025 -- "$@"
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            # Libraries needed for dynamic linking
            systemLibs
            # Main GHC packages
            ++ ghcPackages
            # Haskell tools
            ++ (with pkgs; [
              cabal-install
              haskellPackages.implicit-hie
              ghcid
              ormolu
            ])
            # Helper functions
            ++ [
              helperDevelop
              helperRun
            ];

          # Fix for common linking errors in C libraries
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath systemLibs;

          shellHook = ''
            echo "🎄 Welcome to AoC 2025 (GHC ${hpkgs.ghc.version}) 🎄"
            echo "----------------------------------------------------"
            echo "  ${baseNameOf (pkgs.lib.getExe helperDevelop)} n: Run Day n continuously with ghcid"
            echo "  ${baseNameOf (pkgs.lib.getExe helperRun)} n: Run Day n"
            echo "----------------------------------------------------"
            # Generate hie.yaml for LSP support
            ${pkgs.lib.getExe pkgs.haskellPackages.implicit-hie} > hie.yaml
          '';
        };
      }
    );
}
