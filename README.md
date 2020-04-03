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
This executable allows you to print Holdings contract, its documentation and
its initial storage value.

In order to originate any contract or submit transactions you need additional software, for example `tezos-client`.
For Linux you can get it from the [tezos-packaging](https://github.com/serokell/tezos-packaging) repository.

For example, to originate Holdings contract do the following:
1. Print it to a file by passing `print -n Holdings -o Holdings.tz` to `globacap`.
2. Generate its initial storage using `storage-Holdings` command.
3. Use `tezos-client originate` command to originate the contract.

## Issue Tracker

We use [GitHub](https://github.com/serokell/tezos-globacap/issues).

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## License

© 2020 TBD, all rights reserved.
