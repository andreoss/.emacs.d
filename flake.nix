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
    {
      overlays.default = final: prev: {
        emacs = self.outputs.packages.${prev.system}.emacs;
        notmuch = self.outputs.packages.${prev.system}.notmuch;
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        systems = lib.systems.flakeExposed;
        lib = nixpkgs.lib;
        eachSystem = lib.genAttrs systems;
      in {
        legacyPackages = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };
        packages = let
          pkgs = self.legacyPackages.${system};
          mkEmacs = emacsPkg:
            pkgs.emacsWithPackagesFromUsePackage {
              alwaysEnsure = true;
              defaultInitFile = true;
              config = builtins.readFile (pkgs.substituteAll {
                src = ./init.el;
                jc = jc-themes;
                autofmt = elisp-autofmt;
                lein = pkgs.leiningen.out;
                mysql_jdbc = pkgs.mysql_jdbc.out;
                psql_jdbc = pkgs.postgresql_jdbc.out;
                sqlite_jdbc = pkgs.sqlite-jdbc.out;
              });
              package = emacsPkg;
            } // {
              name = "emacs";
            };
        in {
          notmuch = (pkgs.notmuch.override { }).overrideAttrs (_: {
            doCheck = false;
            withEmacs = true;
            withRuby = false;
          });
          emacs = mkEmacs (pkgs.emacs.overrideAttrs (old: {
            configureFlags = old.configureFlags
              ++ [ "--without-toolkit-scroll-bars" ];
          }));
          emacs-nox = mkEmacs pkgs.emacs-nox;
          emacs-pgtk = mkEmacs pkgs.emacsPgtk;
          emacs-git = mkEmacs (pkgs.emacsGit.overrideAttrs (old: {
            builtInput = old.buildInput ++ [ pkgs.lein ];
            configureFlags = old.configureFlags
              ++ [ "--without-toolkit-scroll-bars" ];
          }));
        };
      });
}
