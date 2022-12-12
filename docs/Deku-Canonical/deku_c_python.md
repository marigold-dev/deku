---
sidebar_position: 4
---

# Python Deku Client Library

A short introduction to the installation and usage of the Python client library for Deku.

## Installation

The Deku library requires a few librairies for cryptography, which are dependencies of
[PyTezos](https://pytezos.org/quick_start.html):

```bash
sudo apt install libsodium-dev libsecp256k1-dev libgmp-dev
```

The Deku library also currently requires Python 3.10. We recommend using
[pyenv](https://realpython.com/intro-to-pyenv/), which allows to manage several different version of
Python on your system, as well as various virtual environments. Once `pyenv` is installed, create a
virtual environment by using the following commands:

```bash
$ pyenv install 3.10  # If not already installed
$ eval "$(pyenv init -)"
$ pyenv local 3.10
$ pyenv virtualenv 3.10 deku
$ pyenv activate deku
```

Then you can install the library using:
```bash
$ pip3 install .
```

in the [`deku-c/python-client/`
directory](https://github.com/marigold-dev/deku/tree/main/deku-c/python-client).

## Examples

Two examples are given in the `examples/` directory:
* `examples/bridge.py` demonstrates a deposit from Tezos to Deku using a pre-deployed contract on
  Ghostnet, as well as the withdraw operation. This contract uses a pre-defined implicit account on
  Ghostnet, but you may have to change it and/or use the
  [faucet](https://faucet.ghostnet.teztnets.xyz/) to refill it.
* `examples/contracts.py` shows the origination of a contract on Deku and invocation of an
  entrypoint of this contract. You can then observe that the contract storage has changed using the
  [Deku TypeScript CLI](https://deku.marigold.dev/docs/Deku-Canonical/deku_c_cli).
