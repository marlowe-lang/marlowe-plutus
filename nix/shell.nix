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

  cardano-cli = inputs.cardano-node.packages.${pkgs.system}.cardano-cli;
  cardano-node = inputs.cardano-node.packages.${pkgs.system}.cardano-node;

  commonPackages = [
    cardano-cli

    tools.cabal
    tools.cabal-fmt
    tools.fourmolu
    tools.haskell-language-server
    tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
    tools.hlint
    tools.implicit-hie
    tools.stylish-haskell

    pkgs.act
    pkgs.bash
    pkgs.bzip2
    pkgs.cacert
    pkgs.coreutils
    pkgs.curl
    pkgs.fd
    pkgs.gawk
    pkgs.git
    pkgs.gnused
    pkgs.jq
    pkgs.nixpkgs-fmt
    pkgs.perl
    pkgs.postgresql
    pkgs.postgresql.lib
    pkgs.postgresql.dev
    pkgs.python3
    pkgs.ripgrep
    pkgs.shellcheck
    pkgs.which
    pkgs.z3
    pkgs.zlib
  ];
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
        jail.combinators.no-new-session
        jail.combinators.mount-cwd
        (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
        (jail.combinators.try-fwd-env "LD_LIBRARY_PATH")
        (jail.combinators.try-fwd-env "CARDANO_NODE_NETWORK_ID")
        (jail.combinators.try-fwd-env "CARDANO_NODE_SOCKET_PATH")
      ];

    extraReadwriteDirs = [
      "/home/paluh/.config/cabal"   # exactly the path it complains about
      "/home/paluh/.cache/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.cabal-devx"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/state/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/cabal-plan"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/ghcid"          # also include the classic cabal dir (safe)
    ];
    # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames project.hsPkgs.cardano-crypto-class.components.library)) project)
    # (builtins.trace (lib.concatStringsSep ", " cryptoShell.nativeBuildInputs) cryptoShell)
    # pkgs.haskell-nix.compiler.${ghc}
    extraPkgs = cryptoShell.nativeBuildInputs ++ cryptoShell.buildInputs ++ commonPackages ++ [
    ];
  };

  #   lib,
  #   coreutils,
  #   writeText,
  #   writeShellApplication,
  #   cardano-cli,
  #   formats,
  #   cardonnay,
  #   cardano-node,
  #   cardano-cli,
  # }: let

  cardonnay = pkgs.python313.pkgs.buildPythonApplication {
    pname = "cardonnay";
    version = "0.3.4";
    SETUPTOOLS_SCM_PRETEND_VERSION = "0.3.4";
    src = inputs.cardonnay-src;
    pyproject = true;
    build-system = with pkgs.python313.pkgs; [ setuptools setuptools-scm ];
    pythonRelaxDeps = [ "setuptools" ];
    nativeBuildInputs = with pkgs.python313.pkgs; [
      pythonRelaxDepsHook
    ];
    postPatch = ''
      # Reduce initial TX submission delay (safe for local testnets)
      find src/cardonnay_scripts/scripts \
        -name 'common-start-*' -type f -exec \
        sed -i 's/readonly TX_SUBMISSION_DELAY=60/readonly TX_SUBMISSION_DELAY=20/' {} +
    '';
    dependencies = with pkgs.python313.pkgs; [
      supervisor
      click
      pygments
      pydantic
      filelock
    ];
  };

  process-compose-testnet-yaml = pkgs.callPackage ./process-compose-testnet.nix {
    inherit cardonnay cardano-node cardano-cli;
  };

  process-compose-testnet = pkgs.writeShellApplication {
    name = "process-compose-testnet";
    runtimeInputs = [];
    text = ''
      ${pkgs.process-compose}/bin/process-compose up -f ${process-compose-testnet-yaml} -L "$RUN_DIR"/process-compose-testnet;
    '';
  };

  shell = project.shellFor {
    name = "marlowe-plutus-${project.args.compiler-nix-name}";

    nativeBuildInputs = commonPackages ++ [
      cardonnay
      cardano-node
      # inputs.process-compose
      pkgs.process-compose
      process-compose-testnet

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

    # export PC_CONFIG_FILES=${selfPkgs.process-compose-yaml}
    # export PC_CONFIG_FILES_REDEMPTION=${selfPkgs.redemption-process-compose-yaml}
    # export PC_CONFIG_FILES_PREPROD_REDEMPTION=${selfPkgs.preprod-redemption-process-compose-yaml}
    shellHook = ''
      ${preCommitCheck.shellHook}
      export ROOT_DIR="$(git rev-parse --show-toplevel)"
      export RUN_DIR="$ROOT_DIR/.run"
      export TESTNET_DIR="$RUN_DIR/testnet"
      export CARDONNAY_TESTNET_ID="9"
      source <(cardonnay control print-env -i "$CARDONNAY_TESTNET_ID" -w "$TESTNET_DIR")
      export CARDANO_NODE_NETWORK_ID=42
      eval "$(cardonnay inspect faucet -i "$CARDONNAY_TESTNET_ID" -w "$TESTNET_DIR" | jq -r 'to_entries[] | "export FAUCET_\(.key|ascii_upcase)=\(.value|@sh)"')"
      export PROCESS_COMPOSE_TESTNET_YAML=${process-compose-testnet-yaml}

      export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.bzip2 ]}:$LD_LIBRARY_PATH"
    '';
  };
in
  shell
