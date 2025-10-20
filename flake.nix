{
  description = "Cardano Hoarding Node";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/5461b7fa65f3ca74cef60be837fd559a8918eaa0";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = { self, nixpkgs, git-hooks }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system}.extend (final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            eve = prev.haskellPackages.callCabal2nix "eve" (prev.fetchFromGitHub {
              owner = "cgeorgii";
              repo = "eve";
              rev = "nixified";
              sha256 = "sha256-wX1UTtO7e0FHk8eoGywocYCMUa9r+BYzR6RWFKjM2C8=";
            }) {};
          };
        };
      });
    in
    {
      # Main packages - Haskell project built with callCabal2nix
      packages.${system} = {
        hoard = pkgs.haskellPackages.callCabal2nix "hoard" ./. {};
        default = self.packages.${system}.hoard;
      };

      # Custom apps for code quality tools
      apps.${system} = {
        # Weeder: detects unused code by building HIE files and analyzing dependencies
        weeder = {
          type = "app";
          program = "${pkgs.writeShellScript "weeder-app" ''
            echo "Building project with HIE files..."
            ${pkgs.haskellPackages.cabal-install}/bin/cabal build --ghc-options=-fwrite-ide-info
            echo "Running weeder to detect unused code..."
            ${pkgs.haskellPackages.weeder}/bin/weeder
          ''}";
        };

        # HLint auto-fix: applies safe refactorings to all Haskell files
        hlint-fix = {
          type = "app";
          program = "${pkgs.writeShellScript "hlint-fix-app" ''
            echo "Running hlint --refactor on all Haskell files..."
            find src app test -name "*.hs" -exec ${pkgs.haskellPackages.hlint}/bin/hlint --refactor {} \;
            echo "Hlint refactoring complete!"
          ''}";
        };
      };

      # Build checks and validation
      checks.${system} = {
        hoard = self.packages.${system}.hoard;
        hoard-test = pkgs.haskell.lib.overrideCabal
          (pkgs.haskellPackages.callCabal2nix "hoard" ./. {})
          (oldAttrs: { doCheck = true; });
        git-hooks = git-hooks.lib.${system}.run {
          src = ./.;
          hooks =
            let
              cabal = nixpkgs.lib.getExe pkgs.haskellPackages.cabal-install;
            in
            {
              hpack.enable = true;
              ormolu.enable = true;
              hlint.enable = true;
            };
        };
      };

      # Development environment with tools and git hooks
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = p: [ self.packages.${system}.hoard ];

        # Haskell development tools
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          tasty-discover
          ghcid
          hlint
          weeder
        ];

        # Non-Haskell tools
        nativeBuildInputs = with pkgs; [
          hpack
          pre-commit
        ];

        # Git hooks integration
        shellHook = self.checks.${system}.git-hooks.shellHook;
      };
    };
}
