{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    jc-themes = {
      url = "gitlab:andreoss/jc-themes/master";
      flake = false;
    };
    elisp-autofmt = {
      url = "git+https://codeberg.org/ideasman42/emacs-elisp-autofmt.git";
      flake = false;
    };
  };
  description = "emacs";
  outputs = { self, nixpkgs, flake-utils, emacs-overlay, jc-themes
    , elisp-autofmt, ... }:
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
            defaultInitFile = true;
            config = builtins.readFile (pkgs.substituteAll {
              src = ./init.el;
              jc = jc-themes;
              autofmt = elisp-autofmt;
            });
            package = pkgs.emacs;
          };
        };
      });
}
