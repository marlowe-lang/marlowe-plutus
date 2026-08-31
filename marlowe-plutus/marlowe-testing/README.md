# marlowe-testing

`marlowe-testing` is the shared testing support package for this repository.

It contains:

- reusable test helpers and generators used by `marlowe-plutus` and `marlowe-binaries`
- reference fixtures under `marlowe-testing/reference/data`
- the `marlowe-testing-reference` executable for regenerating `*.paths` files from `*.contract` fixtures

## Regenerating reference paths

With no arguments, the generator discovers `*.contract` fixtures from the Cabal data files:

```bash
cabal run exe:marlowe-testing-reference
```

To generate `*.paths` files for all reference contracts in the fixture directory, run:

```bash
cabal run exe:marlowe-testing-reference -- "marlowe-testing/reference/data"
```

To generate a single fixture pair, pass a single `*.contract` file:

```bash
cabal run exe:marlowe-testing-reference -- "marlowe-testing/reference/data/deposit.contract"
```

This creates or overwrites the matching `*.paths` file next to the input contract.

In practice:

- use the no-argument form to process the fixtures Cabal exposes in the package data directory
- use an explicit path when you want to update the checked-in files in your working tree

The generator uses `Marlowe.Testing.Reference.processContract`, which:

- reads the JSON contract fixture
- computes all valid input traces with `getAllInputs`
- replays them with `computeTransaction`
- writes the resulting `ReferencePath` values as JSON

## Layout

- `Marlowe/Testing/Reference.hs` - fixture loading and path generation logic
- `Marlowe/Testing/Scripts.hs` - reusable validator conformance checks
- `reference/data/` - `*.contract` / `*.paths` fixture pairs
