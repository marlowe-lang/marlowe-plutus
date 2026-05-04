#!/usr/bin/env bash
# Marlowe CLI lifecycle test: initialize → prepare → execute
#
# This script demonstrates a full contract lifecycle using marlowe-cli.
# The contract has one party deposit 12 ADA into the contract, which then
# closes and pays out 15 ADA (12 deposited + 3 pre-seeded min UTxO) back.
#
# Prerequisites:
#   - cardonnay testnet running (instance 9)
#   - CARDANO_NODE_SOCKET_PATH set (e.g. source .run/testnet/.source_cluster9)
#   - CARDANO_TESTNET_MAGIC=42
#   - PARTY_ADDR: funded Shelley address (needs ~30 ADA)
#   - PARTY_SKEY: signing key file for PARTY_ADDR
#
# Usage:
#   source /path/to/.run/testnet/.source_cluster9
#   export CARDANO_TESTNET_MAGIC=42
#   export PARTY_ADDR=addr_test1...
#   export PARTY_SKEY=/path/to/payment.skey
#   bash marlowe-cli/examples/lifecycle-test/run.sh

set -xeuo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# ─── Configuration ────────────────────────────────────────────────────────────

TESTNET_MAGIC="${CARDANO_TESTNET_MAGIC:-42}"
SOCKET="${CARDANO_NODE_SOCKET_PATH:?CARDANO_NODE_SOCKET_PATH must be set}"
PARTY_ADDR="${PARTY_ADDR:?PARTY_ADDR must be set - a funded Shelley address}"
PARTY_SKEY="${PARTY_SKEY:?PARTY_SKEY must be set - signing key for PARTY_ADDR}"
SLOT_LENGTH_MS="${SLOT_LENGTH_MS:?SLOT_LENGTH_MS must be set - slot length in milliseconds}"

TIP=$(cardano-cli query tip)
SLOTS_TO_EPOCH_END=$(echo "$TIP" | jq -r '.slotsToEpochEnd')

NOW_MS=$(( $(date +%s) * 1000 ))
EPOCH_END_MS=$(( NOW_MS + SLOTS_TO_EPOCH_END * SLOT_LENGTH_MS ))
TIMEOUT_MS=$(( EPOCH_END_MS + 3600000 ))  # 1 hour after epoch end

MARLOWE_CLI="cabal run -v0 --project-file $REPO_ROOT/cabal.project marlowe-cli --"

# ─── Helper ──────────────────────────────────────────────────────────────────

query_first_utxo() {
  local addr="$1"
  cardano-cli query utxo \
    --testnet-magic "$TESTNET_MAGIC" \
    --address "$addr" \
    --out-file /dev/stdout 2>/dev/null \
  | python3 -c "import json,sys; u=json.load(sys.stdin); print(list(u.keys())[0])"
}

# ─── Step 1: Write contract and state files ───────────────────────────────────

echo "=== Step 1: Writing contract and state files ==="
echo "Party address : $PARTY_ADDR"
echo "Contract timeout: $(date -d @$(( TIMEOUT_MS / 1000 )) 2>/dev/null || date -r $(( TIMEOUT_MS / 1000 )))"

cat > "$SCRIPT_DIR/contract.json" <<EOF
{
    "timeout": $TIMEOUT_MS,
    "timeout_continuation": "close",
    "when": [
        {
            "case": {
                "deposits": 12000000,
                "into_account": { "address": "$PARTY_ADDR" },
                "of_token": { "currency_symbol": "", "token_name": "" },
                "party": { "address": "$PARTY_ADDR" }
            },
            "then": "close"
        }
    ]
}
EOF

cat > "$SCRIPT_DIR/state.json" <<EOF
{
    "accounts": [
        [
            [{ "address": "$PARTY_ADDR" }, { "currency_symbol": "", "token_name": "" }],
            3000000
        ]
    ],
    "boundValues": [],
    "choices": [],
    "minTime": 1
}
EOF

# ─── Step 2: Initialize ───────────────────────────────────────────────────────

echo ""
echo "=== Step 2: run initialize ==="

cd "$REPO_ROOT"
$MARLOWE_CLI --conway-era run initialize \
  --testnet-magic "$TESTNET_MAGIC" \
  --socket-path "$SOCKET" \
  --contract-file "$SCRIPT_DIR/contract.json" \
  --state-file "$SCRIPT_DIR/state.json" \
  --out-file "$SCRIPT_DIR/marlowe.json" \
  --print-stats

SCRIPT_ADDR=$($MARLOWE_CLI --conway-era contract address --testnet-magic "$TESTNET_MAGIC")
echo "Marlowe script address: $SCRIPT_ADDR"

# ─── Step 3: Execute creation transaction ─────────────────────────────────────

echo ""
echo "=== Step 3: run execute (creation) ==="

$MARLOWE_CLI --conway-era run execute \
  --testnet-magic "$TESTNET_MAGIC" \
  --socket-path "$SOCKET" \
  --tx-in "$(query_first_utxo "$PARTY_ADDR")" \
  --required-signer "$PARTY_SKEY" \
  --marlowe-out-file "$SCRIPT_DIR/marlowe.json" \
  --change-address "$PARTY_ADDR" \
  --out-file "$SCRIPT_DIR/create-tx.body" \
  --submit 60 \
  --print-stats

echo "Creation tx submitted. Waiting for confirmation..."
sleep 5

# ─── Step 4: Prepare deposit step ─────────────────────────────────────────────

echo ""
echo "=== Step 4: run prepare (deposit 12 ADA) ==="

INVALID_BEFORE=$(( NOW_MS - 60000 ))       # 1 minute ago
INVALID_HEREAFTER=$(( EPOCH_END_MS - SLOT_LENGTH_MS * 30 ))  # 10 slots before epoch end

$MARLOWE_CLI --conway-era run prepare \
  --marlowe-file "$SCRIPT_DIR/marlowe.json" \
  --deposit-account "$PARTY_ADDR" \
  --deposit-party "$PARTY_ADDR" \
  --deposit-amount 12000000 \
  --out-file "$SCRIPT_DIR/marlowe-step1.json" \
  --invalid-before "$INVALID_BEFORE" \
  --invalid-hereafter "$INVALID_HEREAFTER" \
  --print-stats

# ─── Step 5: Execute deposit transaction ──────────────────────────────────────

echo ""
echo "=== Step 5: run execute (deposit) ==="

SCRIPT_UTXO="$(query_first_utxo "$SCRIPT_ADDR")"
echo "Script UTxO: $SCRIPT_UTXO"

$MARLOWE_CLI --conway-era run execute \
  --testnet-magic "$TESTNET_MAGIC" \
  --socket-path "$SOCKET" \
  --marlowe-in-file "$SCRIPT_DIR/marlowe.json" \
  --tx-in-marlowe "$SCRIPT_UTXO" \
  --tx-in "$(query_first_utxo "$PARTY_ADDR")" \
  --tx-in-collateral "$(query_first_utxo "$PARTY_ADDR")" \
  --required-signer "$PARTY_SKEY" \
  --marlowe-out-file "$SCRIPT_DIR/marlowe-step1.json" \
  --change-address "$PARTY_ADDR" \
  --out-file "$SCRIPT_DIR/deposit-tx.body" \
  --submit 60 \
  --print-stats

# ─── Done ─────────────────────────────────────────────────────────────────────

echo ""
echo "=== Lifecycle complete! ==="
echo "Final balance at party address:"
cardano-cli query utxo --testnet-magic "$TESTNET_MAGIC" --address "$PARTY_ADDR"
