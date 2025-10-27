{ inputs, system }:
import inputs.haskell-nix.inputs.nixpkgs {
  inherit system;
  overlays = [ inputs.haskell-nix.overlay ];
  inherit (inputs.haskell-nix) config;
}
