# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
# SPDX-License-Identifier: MPL-2.0

let
  sources = import ./sources.nix;
  haskellNixArgs = import sources."haskell.nix";
  nixpkgs = import sources.nixpkgs;
in nixpkgs haskellNixArgs
