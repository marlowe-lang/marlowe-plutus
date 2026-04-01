# Marlowe Plutus Validators

This project implements the on-chain component of the Cardano implementation of Marlowe as a Plutus smart contract.
The main outputs are the marlowe semantics validator, which checks the spending
of Marlowe script outputs, and the marlowe role payout validator, which checks
the spending of role payouts.

For testing support, fixture generation, and reference data, see `marlowe-plutus/marlowe-testing/README.md`.


## Repo Structure

The Haskell/Plinth code is structured as follows:

```shell
.
├── libs                # Shared internal libraries, adaptors, and utilities
├── marlowe-binaries    # Separated plutus-tx compilation pipeline and scripts generation
└── marlowe-plutus      # Marlowe implementation in Plinth together with test suite
```

### marlowe-plutus

#### marlowe-testing

`marlowe-testing` contains shared testing utilities, reference fixtures, and the fixture-generation executable.
See `marlowe-plutus/marlowe-testing/README.md` for details.

### marlowe-binaries

`marlowe-binaries` provides the CLI for compiling scripts and working with benchmark fixtures. For example, `cabal run marlowe-binaries -- compile --message-format json | cabal run marlowe-binaries -- benchmark generate --output-dir benchmarks` compiles the default production scripts, prints a JSON `CompileResponse`, and pipes it into benchmark generation.
By default, `compile` writes scripts to `out/`, `benchmark generate` writes fixtures under `benchmarks/semantics` and `benchmarks/rolepayout`, and `benchmark run` reads from the packaged `benchmarks/` directory unless `--benchmark-dir` is provided.




## Dev Shell

This repository uses nix to provide the development and build environment.

For instructions on how to install and configure nix (including how to enable access to our binary caches), refer to [this document](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md). 

If you already have nix installed and configured, you may enter the development shell by running `nix develop`.

If you have direnv installed, you can have the shell automatically load and 
refresh for you by running these commands:

```bash
mkdir .direnv
direnv allow
```

Now, whenever you enter the repo the shell will be automatically loaded for you
and will be refreshed when the environment changes.

Once in the dev shell, type `info` to see the available commands and environment.

## Compiling the project

From the dev shell, you can compile the project with `cabal build all`.

Alternatively, you can compile with `nix` using `nix build .#marlowe-validators`

## Compiling the validators

You can compile the validators with the CLI:

```bash
cabal run marlowe-binaries -- compile
```

This writes the default production scripts to `out/`:

- `out/marlowe-rolepayout.plutus` - the role payout validator as a JSON-encoded CBOR text-envelope
- `out/marlowe-semantics.plutus` - the Marlowe validator as a JSON-encoded CBOR text-envelope

Use `--devel-scripts` to preserve tracing and `--message-format text|json|yaml` to control command output.
