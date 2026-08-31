# Lifecycle Test Example

A minimal Marlowe contract lifecycle demonstrating `run initialize` → `run prepare` → `run execute`
using the migrated `marlowe-cli` with **Plutus V3** scripts on a Conway-era testnet.

## Origin

This example is derived from two sources:

1. **`marlowe-cli/doc/simple-1.contract`** (in this repo) — a two-party simple deposit contract
   used as a unit-test fixture. We simplified it further to a single deposit + close.

2. **`marlowe-starter-kit` lesson 03** (`.external-references/marlowe-starter-kit/lessons/03-marlowe-cli/`)
   — the Zero-Coupon Bond example demonstrating the full `marlowe-cli run` workflow on `preprod`.
   That example uses two parties (lender + borrower) with templates (`marlowe-cli template`).

## Differences from the Originals

| Aspect | Original (`simple-1`) | Original (ZCB starter-kit) | This Example |
|--------|----------------------|---------------------------|--------------|
| Contract | 3-step (deposit → notify → pay → notify → close) | 2-party lend/repay | 1-step deposit → close |
| Parties | 1 address | 2 addresses (lender, borrower) | 1 address (party) |
| Timeouts | Past (Sept 2022) — not executable | Future via template params | Future (2 hours from run time) |
| Templates | N/A | Uses `marlowe-cli template zcb` | Inline JSON (no template) |
| Era | Plutus V2 (original) | Plutus V2 (original) | **Plutus V3, Conway era** |
| CLI style | Low-level (`contract marlowe`) | High-level (`run` subcommands) | High-level (`run` subcommands) |
| State | Pre-seeded with 3 ADA | Empty | Pre-seeded with 3 ADA (min UTxO) |
| Network | `preprod` or local | `preprod` | Local testnet (magic=42) |

## Contract Description

```
When
  [Case (Deposit PARTY PARTY ADA 12000000)
    Close]
  TIMEOUT
  Close
```

- Party deposits 12 ADA
- Contract closes, paying out 12 ADA + 3 ADA (pre-seeded min UTxO) = 15 ADA total to party

## Files

- `contract.json` — generated at runtime with a 2-hour timeout
- `state.json` — generated at runtime with 3 ADA pre-seeded for party
- `marlowe.json` — output of `run initialize`
- `marlowe-step1.json` — output of `run prepare` (deposit step)
- `create-tx.body` — creation transaction body
- `deposit-tx.body` — deposit transaction body

## Known Issues / Migration Notes

- **Execution cost is stubbed at 0** — `validatorInfo` returns `ExBudget 0 0` until the
  Plutus V3 evaluator is properly wired up with the V3 cost model and a real `ScriptContext`.
  The old V2 code was also buggy (passed `[]` args so the script was never evaluated).

- **`openRolesValidator` is a placeholder** — currently returns `rolePayoutValidatorBytes`
  since `marlowe-binaries` does not yet compile the open roles validator.

- **Template and Test commands are disabled** — `Language.Marlowe.CLI.Command.Template` and
  `Language.Marlowe.CLI.Command.Test` depend on `marlowe-contracts` and `marlowe-runtime:config`
  which are not yet in the workspace.

## Usage

```bash
# Set up environment
source /path/to/testnet/.source_cluster9
export CARDANO_TESTNET_MAGIC=42

# Provide a funded wallet
export PARTY_ADDR=addr_test1...
export PARTY_SKEY=/path/to/payment.skey

# Run the lifecycle
bash marlowe-cli/examples/lifecycle-test/run.sh
```
