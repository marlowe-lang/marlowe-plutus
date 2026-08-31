# Simple-1 Example

A minimal Marlowe contract demonstrating a simple two-step payment flow.

## Contract Description

The contract involves:
1. Party deposits 12 ADA into the contract
2. Party receives 5 ADA back after notification
3. Contract closes

## Files

- `simple-1.contract` - JSON representation of the Marlowe contract
- `simple-1.state` - Initial state with 3 ADA in an account

## Status

**Known Issue**: `Language.Marlowe.Scripts` contains stub implementations that need to be
replaced with actual Plutus script generation from `Marlowe.Plutus.Scripts`.

Until this is resolved, the marlowe-cli tool cannot execute contract operations.

## Original Documentation

See `../../../doc/simple-1.contract` and `../../../doc/simple-1.state` for the source files.
