# Marlowe Web Porting Guide

## Overview

This package is a self-contained port of `marlowe-runtime-web` from `marlowe-cardano`.
The goal is to remove dependencies on `marlowe-cardano` packages and make it
depend only on `marlowe-plutus`.

## Package Structure

```
marlowe-web/
├── marlowe-web.cabal
├── PORT.md                    # This document
└── src/
    └── Language/
         └── Marlowe/
              └── Runtime/
                   └── Web/
                        ├── Adapter/           # DTO conversions, HTTP adapters
                        ├── Core/              # Core types (Address, Asset, etc.)
                        ├── Contract/          # Contract API endpoints
                        ├── Payout/            # Payout API
                        ├── Role/              # Role API
                        ├── Tx/                # Transaction API
                        ├── Withdrawal/        # Withdrawal API
                        └── Server/            # Server setup
```

## Dependencies on marlowe-cardano packages

### marlowe-runtime and subpackages

The following modules depend on `marlowe-runtime` (and subpackages like
`marlowe-runtime:contract-api`, `marlowe-runtime:sync-api`, etc.):

| Module | Dependencies |
|--------|-------------|
| `Adapter/Server/ContractClient.hs` | `ChainSync.Api`, `Client`, `Client.Transfer`, `Contract.Api`, `Protocol.*` |
| `Adapter/Server/SyncClient.hs` | `Protocol.Client`, `Protocol.Query.Client`, `ChainSync.Api`, `Core.Api`, `Discovery.Api`, `Transaction.Api` |
| `Adapter/Server/TxClient.hs` | `Cardano.Api`, `ChainSync.Api`, `Core.Api`, `Transaction.Api`, `Protocol.Client`, `Core.V1.Semantics` |
| `Adapter/Server/DTO.hs` | `Cardano.Api`, `ChainSync.Api`, `Core.Api`, `Discovery.Api`, `Transaction.Api`, `Protocol.Query.Types`, `Core.V1.Semantics`, `Core.V1.Semantics.Types.Address` |
| `Adapter/Server/Monad.hs` | `ChainSync.Api`, `Core.Api` |
| `Adapter/Server/ApiError.hs` | `History.Api`, `Transaction.Api` |
| `Contract/Server.hs` | `Core.Api`, `ChainSync.Api`, `Transaction.Api`, `Discovery.Api`, `Protocol.Query.Types` |
| `Contract/Transaction/Server.hs` | `Cardano.Api`, `Core.Api`, `ChainSync.Api`, `Transaction.Api`, `Protocol.Query.Types` |
| `Contract/Source/Server.hs` | `ChainSync.Api`, `Contract.Api`, `Protocol.Transfer.Types` |
| `Withdrawal/Server.hs` | `Cardano.Api`, `Core.Api`, `ChainSync.Api`, `Transaction.Api`, `Protocol.Query.Types` |
| `Role/Server.hs` | `Cardano.Api`, `Core.Api`, `Transaction.Api`, `ChainSync.Api` |
| `Payout/Server.hs` | `Protocol.Query.Types` |

### marlowe-protocols

| Module | Dependencies |
|--------|-------------|
| `Adapter/Server/TxClient.hs` | `Protocol.Client` |
| `Adapter/Server/SyncClient.hs` | `Protocol.Client`, `Protocol.Query.Client`, `Protocol.Query.Types` |
| `Adapter/Server/DTO.hs` | `Protocol.Query.Types` |
| `Adapter/Server/ContractClient.hs` | `Protocol.Client`, `Protocol.Transfer.Types` |
| `Contract/Server.hs`, `Transaction/Server.hs`, etc. | `Protocol.Query.Types` |

### marlowe-object

| Module | Dependencies |
|--------|-------------|
| `Core/Semantics/Schema.hs` | `Object.Types` |
| `Core/Object/Schema.hs` | `Object.Types` |
| `Contract/API.hs` | `Object.Types` |
| `Contract/Source/Server.hs` | `Object.Types`, `Object.Link` |
| `Client.hs` | `Object.Types` |
| `Adapter/Server/ContractClient.hs` | `Object.Types`, `Object.Link` |

## Types to Port

### Language.Marlowe.Core.V1.Semantics → Marlowe.Plutus.Semantics

| Original | Ported |
|----------|--------|
| `Language.Marlowe.Core.V1.Semantics` | `Marlowe.Plutus.Semantics` |
| `Language.Marlowe.Core.V1.Semantics.Types` | `Marlowe.Plutus.Semantics.Types` |
| `Language.Marlowe.Core.V1.Semantics.Types.Address` | `Marlowe.Plutus.Semantics.Types.Address` |

### marlowe-object → Need Stubs/Mocks

The following types from `marlowe-object` are used:
- `Label`
- `LabelledObject`
- `ObjectBundle`
- `Object.Value`, `Object.Observation`, `Object.Contract`, `Object.Party`, `Object.Token`, `Object.Action`, etc.

**Decision**: These are complex AST types. For the initial port, we will stub them.

### marlowe-protocols → Need Stubs/Mocks

The following are used from `marlowe-protocols`:
- `Protocol.Client`
- `Protocol.Query.Client`
- `Protocol.Query.Types`
- `Protocol.Transfer.Types`

### marlowe-runtime types → Need Stubs/Mocks

Many runtime types are used:
- `ChainSync.Api` types (txouts, utxos, etc.)
- `Core.Api` types (MarloweVersion, etc.)
- `Transaction.Api` types
- `Discovery.Api` types
- `History.Api` types
- `Contract.Api` types

## Stubs to Create

### 1. Runtime Mock Types

Create stub modules that re-export from appropriate packages or define minimal types:

```
Language.Marlowe.Runtime.Web.Runtime/
├── Types.hs          # Stub for Core.Api, Transaction.Api, etc.
├── ChainSync.hs      # Stub for ChainSync.Api
├── Protocol.hs       # Stub for marlowe-protocols
└── Object.hs         # Stub for marlowe-object
```

### 2. Client/Server Stubs

For the initial port, these modules should be stubs:

- `Adapter/Server/ContractClient.hs` → Stub
- `Adapter/Server/SyncClient.hs` → Stub
- `Adapter/Server/TxClient.hs` → Stub
- `Adapter/Server/DTO.hs` → Partial port, needs stubs for runtime types
- `Adapter/Server/Monad.hs` → Stub
- `Adapter/Server/ApiError.hs` → Partial port

### 3. Server Stubs

These server modules should be stubs initially:

- `Contract/Server.hs` → Stub
- `Contract/Transaction/Server.hs` → Stub
- `Contract/Source/Server.hs` → Stub
- `Withdrawal/Server.hs` → Stub
- `Role/Server.hs` → Stub
- `Payout/Server.hs` → Stub

## Modules That Can Be Copied Directly (No/Minimal Changes)

These modules have minimal external dependencies and can be copied with minor changes:

### Core Types
- `Core/Address.hs`
- `Core/Script.hs`
- `Core/Metadata.hs`
- `Core/Tx.hs`
- `Core/Base16.hs`
- `Core/Asset.hs`
- `Core/NetworkId.hs`
- `Core/MarloweVersion.hs`
- `Core/Tip.hs`
- `Core/Party.hs`
- `Core/BlockHeader.hs`
- `Core/Roles.hs`

### Adapters (Internal)
- `Adapter/ByteString.hs`
- `Adapter/CommaList.hs`
- `Adapter/Servant.hs`
- `Adapter/Links.hs`
- `Adapter/Pagination.hs`
- `Adapter/URI.hs`

### API Types
- `Tx/API.hs` (needs V1 types changed)
- `Contract/API.hs` (needs V1 types changed)
- `Contract/Next/API.hs`
- `Contract/Next/Schema.hs`
- `Withdrawal/API.hs`
- `Payout/API.hs`
- `Role/API.hs`

### Clients
- `Role/Client.hs`
- `Contract/Transaction/Client.hs`
- `Contract/Next/Client.hs`
- `Client.hs`
- `Status.hs`

## Implementation Notes

### Address Types

The original uses `P.Address` from `PlutusLedgerApi.V2`. We need to use
`Marlowe.Plutus.Semantics.Types.Address` instead.

### Token/Asset Types

Original uses `V1.Token` from `Language.Marlowe.Core.V1.Semantics.Types`.
We use `Marlowe.Plutus.Semantics.Types.Token`.

### Contract/State Types

Original uses `V1.Contract`, `V1.State`, `V1.Input` from `V1.Semantics.Types`.
We use `Marlowe.Plutus.Semantics.Types.Contract`, etc.

### MarloweVersion

Original defines `MarloweVersion` in `Core.Api`. We should use the one from
`Language.Marlowe.Runtime.Core.Api` (from marlowe-transactions) or define
our own following the same pattern.

## Phases

### Phase 1: Initial Structure (DONE)
- [x] Create package structure
- [x] Create cabal file with dependencies
- [x] Create stub modules for complex dependencies

### Phase 2: Create Core Stubs (DONE)
- [x] Create Runtime.Types module with stub types
- [x] Create Core.NetworkId stub module

### Phase 3: Copy Self-Contained Modules (IN PROGRESS)
- [ ] Copy Core types (Address, Asset, BlockHeader, etc.)
- [ ] Copy Adapters (internal utilities)
- [ ] Copy API type modules

### Phase 4: Create More Stubs
- [ ] Create Protocol stubs for marlowe-protocols
- [ ] Create Server stubs for Contract/Role/Payout servers
- [ ] Create Client stubs

### Phase 5: Add Servant/OpenAPI Support
- [ ] Add servant dependency
- [ ] Add openapi3 dependency
- [ ] Copy API type classes and schemas

### Phase 6: Build and Fix
- [ ] Build package
- [ ] Fix compilation errors iteratively
- [ ] Refine stubs as needed

### Phase 7: Integration (Future)
- [ ] Integrate with marlowe-transactions for real transaction building
- [ ] Add actual ChainSync client implementation
- [ ] Add real protocol clients

## Current Status

### Packages Built Successfully
- marlowe-transactions (0.1.0.0) - Transaction builders with BuildConstraints
- marlowe-web (0.1.0.0) - Initial stub package

### marlowe-web Modules
- `Language.Marlowe.Runtime.Web.Runtime.Types` - Stub types for MarloweVersion, TransactionScriptOutput, Payout, Wallet, etc.
- `Language.Marlowe.Runtime.Web.Core.NetworkId` - Stub NetworkId type
- `Language.Marlowe.Runtime.Web.Core.Semantics.Schema` - Empty module placeholder
