{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };
    project = nix-haskell (import ./project.nix);

    # Builds the wasm target with the GHC 9.12 bindist from the ghc-wasm-meta
    # pin instead of the drivers' own compilers.
    wasm-meta = import ./wasm-meta.nix { series = "9.12"; };

in {
  haskell-nix = project.haskell-nix.project;
  nixpkgs = project.nixpkgs.project;

  haskell-nix-wasm-meta = project.haskell-nix.project.override wasm-meta;
  nixpkgs-wasm-meta = project.nixpkgs.project.override wasm-meta;
}
