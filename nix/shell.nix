{ inputs, pkgs, lib, project, ghc }:

let
  tools = {
    cabal = project.tool "cabal" "3.12.1.0";
    cabal-fmt = project.tool "cabal-fmt" "latest";
    fourmolu = project.tool "fourmolu" "latest";
    haskell-language-server = project.tool "haskell-language-server" "2.12.0.0";
    hlint = project.tool "hlint" "latest";
    implicit-hie = project.tool "implicit-hie" "latest";
    stylish-haskell = project.tool "stylish-haskell" "latest";
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
  cryptoShell = project.shellFor {
    packages = p: [p.cardano-crypto-class];
    withHoogle = false;
  };

  commonJail = {
    # jail.combinators.unshare-all
    #     jail.combinators.mount-cwd
    #     (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
    #   ];
    baseJailOptions =
      let
        jail = inputs.jailed-agents.lib.${pkgs.system}.internals.jail;
      in [
        jail.combinators.network
        jail.combinators.time-zone
        # jail.combinators.mount-dev
        # jail.combinators.mount-proc
        jail.combinators.no-new-session
        jail.combinators.mount-cwd
        # jail.combinators.tmpfs-tmp
        (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
      ];

    extraReadwriteDirs = [
      "/home/paluh/.config/cabal"   # exactly the path it complains about
      "/home/paluh/.cache/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.cabal-devx"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/state/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/cabal-plan"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/ghcid"          # also include the classic cabal dir (safe)
    ];
    extraPkgs = cryptoShell.nativeBuildInputs ++ cryptoShell.buildInputs ++ [
      # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames project.hsPkgs.cardano-crypto-class.components.library)) project)
      # (builtins.trace (lib.concatStringsSep ", " cryptoShell.nativeBuildInputs) cryptoShell)
      # pkgs.haskell-nix.compiler.${ghc}

      tools.cabal
      tools.cabal-fmt
      tools.fourmolu
      tools.haskell-language-server
      tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
      tools.hlint
      tools.implicit-hie
      tools.stylish-haskell

      pkgs.coreutils
      pkgs.fd
      pkgs.git
      pkgs.gnused
      pkgs.jq
      pkgs.perl
      pkgs.python3
      pkgs.ripgrep
      pkgs.z3
    ];
  };

  shell = project.shellFor {
    name = "marlowe-plutus-${project.args.compiler-nix-name}";

    buildInputs = [
      tools.cabal
      tools.cabal-fmt
      tools.fourmolu
      tools.haskell-language-server
      tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
      tools.hlint
      tools.implicit-hie
      tools.stylish-haskell

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
      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
      })
      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        name = "jailed-bash";
        pkg = pkgs.bashInteractive;
        # configPaths = [
        #   "~/.bashrc"
        #   "~/.inputrc"
        # ];
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
      })
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
