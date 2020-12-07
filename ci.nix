# SPDX-FileCopyrightText: 2020 Globacap
#
# SPDX-License-Identifier: MPL-2.0
rec {
  sources = import ./nix/sources.nix;
  xrefcheck = import sources.xrefcheck;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  pkgs = import sources.nixpkgs haskell-nix.nixpkgsArgs;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;

  # all local packages and their subdirectories
  # we need to know subdirectories to make weeder stuff work
  local-packages = [{
    name = "globacap";
    subdirectory = ".";
  }];

  # names of all local packages
  local-packages-names = map (p: p.name) local-packages;

  # haskell.nix package set
  # parameters:
  # - release -- 'true' for production build, 'false' for development build
  # - commitSha, commitDate -- git revision info used during morley-ledgers compilation
  hs-pkgs = { release, commitSha ? null, commitDate ? null }:
    pkgs.haskell-nix.stackProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };

      modules = [
        # common options for all local packages:
        {
          packages = pkgs.lib.genAttrs local-packages-names (packageName: {
            package.ghcOptions = with pkgs.lib;
              concatStringsSep " " ([
                "-O0"
                "-Werror"
              ]
              # produce *.dump-hi files, required for weeder:
                ++ optionals (!release) [ "-ddump-to-file" "-ddump-hi" ]);
            dontStrip = !release; # strip in release mode, reduces closure size
            doHaddock = true; # enable haddock for local packages

            # in non-release mode collect all *.dump-hi files (required for weeder)
            postInstall =
              if release then null else weeder-hacks.collect-dump-hi-files;
          });
        }

        {
          # don't haddock dependencies
          doHaddock = false;

          # provide commit sha and date for globacap in release mode:
          packages.globacap = {
            preBuild = ''
              export MORLEY_DOC_GIT_COMMIT_SHA=${
                if release then
                  pkgs.lib.escapeShellArg commitSha
                else
                  "UNSPECIFIED"
              }
              export MORLEY_DOC_GIT_COMMIT_DATE=${
                if release then
                  pkgs.lib.escapeShellArg commitDate
                else
                  "UNSPECIFIED"
              }
            '';
          };
        }
      ];
    };

  hs-pkgs-development = hs-pkgs { release = false; };

  # component set for all local packages like this:
  # { morley = { library = ...; exes = {...}; tests = {...}; ... };
  #   morley-prelude = { ... };
  #   ...
  # }
  packages = pkgs.lib.genAttrs local-packages-names
    (packageName: hs-pkgs-development."${packageName}".components);

  # returns a list of all components (library + exes + tests + benchmarks) for a package
  get-package-components = pkg:
    with pkgs.lib;
    optionals (pkg ? library) [ pkg.library pkg.library.haddock ]
    ++ attrValues pkg.exes
    ++ attrValues pkg.tests
    ++ attrValues pkg.benchmarks;

  # per-package list of components
  components =
    pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

  # a list of all components from all packages in the project
  all-components = with pkgs.lib; flatten (attrValues components);

  # run globacap to produce contract documents
  contracts-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs:
    pkgs.runCommand "contracts-doc" {
      buildInputs = [ (hs-pkgs releaseArgs).globacap.components.exes.globacap ];
    } ''
      mkdir $out
      cd $out
      globacap document --name Holdings --output Holdings-contract.md
    '';

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
    hs-pkgs = hs-pkgs-development;
    local-packages = local-packages;
  };
}
