{ inputs, pkgs, lib, project, ghc }:

let
  tools = {
    cabal = project.tool "cabal" "3.12.1.0";
    cabal-fmt = project.tool "cabal-fmt" "latest";
    haskell-language-server = project.tool "haskell-language-server" "2.12.0.0";
    stylish-haskell = project.tool "stylish-haskell" "latest";
    fourmolu = project.tool "fourmolu" "latest";
    hlint = project.tool "hlint" "latest";
  };

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = false;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = false;
        package = tools.fourmolu;
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  shell = project.shellFor {
    name = "marlowe-plutus-${project.args.compiler-nix-name}";

    buildInputs = [
      tools.haskell-language-server
      tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
      tools.stylish-haskell
      tools.fourmolu
      tools.cabal
      tools.hlint
      tools.cabal-fmt

      pkgs.shellcheck
      pkgs.nixpkgs-fmt
      pkgs.github-cli
      pkgs.act
      pkgs.bzip2
      pkgs.gawk
      pkgs.zlib
      pkgs.z3
      pkgs.cacert
      pkgs.curl
      pkgs.bash
      pkgs.git
      pkgs.which
    ];

    # To make shell lightweight compile only the packages
    # which pull in external dependencies like
    # cardano-crypto-class which brings in libsodium, secp256k1, etc.
    packages = p: [p.cardano-crypto-class];

    withHoogle = false;

    shellHook = ''
      ${preCommitCheck.shellHook}
    '';
  };
in
  shell
