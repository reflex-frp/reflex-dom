# Builds the wasm target with a GHC bindist from the ghc-wasm-meta pin
# instead of the drivers' own compilers.
#
# Example:
#
#   project.override (import ./wasm-meta.nix { series = "9.12"; })
#   => the project with its wasm target built by the 9.12.4.20260731 bindist
{ series }:

let # The exact bindist version of each series. The drivers name package
    # sets and library directories after the exact version, so the series
    # in the bindist's name is not enough. The pin carries the version in
    # ghcup metadata. That file is yaml, which nix cannot read.
    versions = {
      "9.12" = "9.12.4.20260731";
    };

in { nix-haskell-compilers, nix-haskell-patches, ... }: {

  imports = [
    (import "${nix-haskell-compilers}/ghc-wasm-meta" {
      flavour = series;
      version = versions.${series} or null;
    })
    (import "${nix-haskell-patches}/wasm/jsaddle-wasm" {})
  ];

  # The `if !arch(wasm32)` stanza of `cabal.project`, restated for the
  # driver that cannot read it. reflex-dom's warp backend needs C libraries
  # that nixpkgs cannot cross-compile to wasi. Only the wasi target turns
  # the flag off. The ghcjs target builds with the backend on.
  platforms.wasi32.packages.reflex-dom.flags.use-warp = false;

}
