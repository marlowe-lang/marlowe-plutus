{
  cardano-cli,
  cardano-node,
  cardonnay,
  coreutils,
  formats,
  lib,
  postgresql,
  sqitchPg,
  writeShellApplication,
  writeText,
}: let
  testnet-processes = import ./testnet-processes.nix {
    inherit lib coreutils writeShellApplication writeText formats cardonnay cardano-node cardano-cli;
  };
  postgres-processes = import ./postgres-processes.nix {
    inherit lib postgresql coreutils writeText writeShellApplication sqitchPg;
  };
  processes = lib.attrsets.unionOfDisjoint testnet-processes postgres-processes;
in (formats.yaml {}).generate "process-compose.yaml" {
  version = "0.5";
  processes = processes;
}

