# A lot of this is copied from cardano-parts, see https://github.com/input-output-hk/cardano-parts/issues/42
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

      : "''${RUN_DIR:?}"
      : "''${TESTNET_DIR:?}"
      : "''${CARDONNAY_TESTNET_ID:?}"
      : "''${CARDANO_NODE_NETWORK_ID:?}"
      : "''${CARDANO_NODE_SOCKET_PATH:?}"
    '';
  };

  initialize-testnet = writeShellApplication {
    name = "initialize-testnet";
    runtimeInputs = [cardonnay cardano-node cardano-cli];
    text = ''
      set -x
      rm -rf "$RUN_DIR"
      mkdir -p "$TESTNET_DIR"
      cardonnay create -t conway_fast -w "$TESTNET_DIR" -i "$CARDONNAY_TESTNET_ID"
    '';
  };
in
  (formats.yaml {}).generate "process-compose.yaml" {
    version = "0.5";
    processes = {
      validate-testnet-env = {
        namespace = "testnet";
        command = "${validate-testnet-env}/bin/validate-testnet-env";
      };

      initialize-testnet = {
        namespace = "testnet";
        command = "${initialize-testnet}/bin/initialize-testnet";
        depends_on = {
          validate-testnet-env.condition = "process_completed_successfully";
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
            command = "cardano-cli query tip";
          };
          initial_delay_seconds = 10;      # after we reduced the internal sleep
          period_seconds = 2;
          timeout_seconds = 3;
          success_threshold = 1;
          failure_threshold = 60;
        };
      };
    };
  }

