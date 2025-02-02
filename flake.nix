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
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
      jc-themes,
      elisp-autofmt,
      ...
    }:
    {
      overlays.default = final: prev: {
        emacs = self.outputs.packages.${prev.system}.emacs;
        notmuch = self.outputs.packages.${prev.system}.notmuch;
      };
      nixosModules.home-manager =
        { config, pkgs, ... }:
        {
          options = { };
          config = {
            programs.emacs = {
              enable = true;
            };
            editorconfig = {
              enable = true;
              settings = {
                "*" = {
                  end_of_line = "lf";
                  trim_trailing_whitespace = true;
                  insert_final_newline = true;
                };
              };
            };
            xresources.properties = {
              "Emacs*toolBar" = 0;
              "Emacs*menuBar" = 0;
              "Emacs*font" = "-misc-Spleen-regular-normal-normal-*-24-*-*-*-c-120-iso10646-1";
              "Emacs*geometry" = "80x38";
              "Emacs*scrollBar" = "on";
              "Emacs*scrollBarWidth" = 6;
            };
            home = {
              file.".local/bin/et" = {
                executable = true;
                text = ''
                  #!/bin/sh
                  exec emacsclient -t "$@"
                '';
              };
              file.".local/bin/ec" = {
                executable = true;
                text = ''
                  #!/bin/sh
                  exec emacsclient -c "$@"
                '';
              };
              packages = with pkgs; [
                spleen
                coreutils-full
                (hunspellWithDicts [
                  hunspellDicts.ru_RU
                  hunspellDicts.es_ES
                  hunspellDicts.en_GB-large
                ])
              ];
              sessionVariables = {
                EDITOR = "ec";
              };
            };
            services.emacs = {
              enable = true;
              startWithUserSession = "graphical";
              client = {
                enable = true;
                arguments = [ "-c" ];
              };
            };
          };
        };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        systems = lib.systems.flakeExposed;
        lib = nixpkgs.lib;
        eachSystem = lib.genAttrs systems;
      in
      {
        legacyPackages = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };
        packages =
          let
            pkgs = self.legacyPackages.${system};
            mkEmacs =
              emacsPkg:
              pkgs.emacsWithPackagesFromUsePackage {
                alwaysEnsure = true;
                defaultInitFile = true;
                config = builtins.readFile (
                  pkgs.substituteAll {
                    src = ./init.el;
                    jc = jc-themes;
                    autofmt = elisp-autofmt;
                    lein = pkgs.leiningen.out;
                    mysql_jdbc = pkgs.mysql_jdbc.out;
                    psql_jdbc = pkgs.postgresql_jdbc.out;
                    sqlite_jdbc = pkgs.sqlite-jdbc.out;
                  }
                );
                package = emacsPkg;
              }
              // {
                name = "emacs";
              };
          in
          {
            notmuch = (pkgs.notmuch.override { }).overrideAttrs (_: {
              doCheck = false;
              withEmacs = true;
              withRuby = false;
            });
            emacs = mkEmacs (
              pkgs.emacs.overrideAttrs (old: {
                configureFlags = old.configureFlags ++ [ "--without-toolkit-scroll-bars" ];
              })
            );
            emacs-nox = mkEmacs pkgs.emacs-nox;
            emacs-pgtk = mkEmacs pkgs.emacsPgtk;
            emacs-git = mkEmacs (
              pkgs.emacsGit.overrideAttrs (old: {
                configureFlags = old.configureFlags ++ [ "--without-toolkit-scroll-bars" ];
              })
            );
          };
      }
    );
}
