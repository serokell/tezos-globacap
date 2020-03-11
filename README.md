<!--
 - SPDX-FileCopyrightText: 2020 TBD
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -->

# Tezos Globacap

Tezos Globacap project provides two entities:
* Safelist contract which is used to restrict and regulate share transfers.
* Holdings contract which is used to distribute the token and optionally regulated by the
  safelist contract.

There is a [specification](/docs/specification.md) for the required smart contracts.

## Build Instructions

You can use `stack build` to build `globacap` executable.

## Usage

Run `stack exec -- globacap --help` to see available commands.

## Issue Tracker

We use [GitHub](https://github.com/serokell/tezos-globacap/issues).

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

© 2020 TBD, all rights reserved.
