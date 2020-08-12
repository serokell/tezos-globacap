# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
# SPDX-License-Identifier: MPL-2.0

let
  sources = import ./sources.nix;
  haskellNix = import sources."haskell.nix" {
    sourceOverrides = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  nixpkgs = import sources.nixpkgs;
in nixpkgs haskellNix.nixpkgsArgs
