{
  lib,
  coreutils,
  writeText,
  writeShellApplication,
  formats,
  cardonnay,
  cardano-node,
  cardano-cli,
}: let
  validate-testnet-env = writeShellApplication {
    name = "validate-testnet-env";
    runtimeInputs = [coreutils];
    # Check if all variables are defined and if not stop the process.
    text = ''
      set -euo pipefail
      set -x
      : "''${TESTNET_DIR:?}"
      : "''${CARDONNAY_TESTNET_ID:?}"
      : "''${CARDANO_NODE_NETWORK_ID:?}"
      : "''${CARDANO_NODE_SOCKET_PATH:?}"
    '';
  };

  clear-testnet-state = writeShellApplication {
    name = "clear-testnet-state";
    runtimeInputs = [coreutils];
    text = ''
      set -x
      if [ -d "$TESTNET_DIR" ]; then
        rm -rf "$TESTNET_DIR"
      fi
      mkdir -p "$TESTNET_DIR"
    '';
  };

  initialize-testnet = writeShellApplication {
    name = "initialize-testnet";
    runtimeInputs = [cardonnay cardano-node cardano-cli];
    text = ''
      set -x
      cardonnay create -t conway_fast -w "$TESTNET_DIR" -i "$CARDONNAY_TESTNET_ID"
    '';
  };
in {
  validate-testnet-env = {
    namespace = "testnet";
    command = "${validate-testnet-env}/bin/validate-testnet-env";
  };

  clear-testnet-state = {
    namespace = "testnet";
    command = "${clear-testnet-state}/bin/clear-testnet-state";
    depends_on = {
      validate-testnet-env.condition = "process_completed_successfully";
    };
  };

  initialize-testnet = {
    namespace = "testnet";
    command = "${initialize-testnet}/bin/initialize-testnet";
    depends_on = {
      validate-testnet-env.condition = "process_completed_successfully";
      clear-testnet-state.condition = "process_completed_successfully";
    };
    is_daemon = true;
    availability = {
      restart = "always";
    };
    shutdown = {
      command = "cardonnay control stop -i $CARDONNAY_TESTNET_ID -w $TESTNET_DIR";
      timeout_seconds = 15;
    };
    readiness_probe = {
      exec = {
        command = "cardano-cli query tip --testnet-magic $CARDANO_NODE_NETWORK_ID --socket-path $CARDANO_NODE_SOCKET_PATH";
      };
      initial_delay_seconds = 10;      # after we reduced the internal sleep
      period_seconds = 2;
      timeout_seconds = 3;
      success_threshold = 1;
      failure_threshold = 60;
    };
  };
}

