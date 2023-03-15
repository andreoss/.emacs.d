{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  description = "emacs";
  outputs = { self, nixpkgs, flake-utils, emacs-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        systems = lib.systems.flakeExposed;
        lib = nixpkgs.lib;
        eachSystem = lib.genAttrs systems;
      in {
        legacyPackages = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };
        overlays.default = final: prev: {
          emacs = self.outputs.packages.${system}.emacs;
        };
        packages = {
          emacs = let pkgs = self.legacyPackages.${system};
          in pkgs.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./init.el;
            defaultInitFile = true;
            package = pkgs.emacs;
          };
          default = self.packages.${system}.emacs;
        };
      });
}
