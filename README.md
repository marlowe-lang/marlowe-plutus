# Marlowe Plutus Validators

This project implements the on-chain component of the Cardano implementation of Marlowe as a Plutus smart contract.
The main outputs are the marlowe semantics validator, which checks the spending
of Marlowe script outputs, and the marlowe role payout validator, which checks
the spending of role payouts.

We are using some CPP macros so in general this option `-fforce-recomp` should be used BUT
it slows down the devel cycle significantly.
We should consider either using it from command line or in cabal.project with local override.

```
cabal build marlowe-plutus-spec --ghc-option=-fforce-recomp
```


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

`marlowe-testing` is a library which does not expose tests but provides core utilities which can be used during binaries testing and pure semantic testing.
Prominent modules include:

* `Spec.Marlowe.Scripts` - this module implement a tests on the "pure transaction" level. This test suite is parametrised by `MarloweValidators` structure which expects
two validator functions - one for semantics and one for the role payout. It is used by both `marlowe-plutus` and `marlowe-binaries`.




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

You can compile the validators using the following command:

```bash
nix build .#marlowe-validators
```

This will build the project and run the `marlowe-validators` executable and
output the compiled plutus scripts into local directory called `result`. This
directory will contain two files:

- `marlowe-rolepayout.plutus` The compiled role payout validator as a JSON-encoded CBOR text-envelope.
- `marlowe-semantics.plutus` The compiled marlowe validator as a JSON-encoded CBOR text-envelope.
