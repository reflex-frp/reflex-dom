{ system ? builtins.currentSystem }:
let nix-haskell = import ./deps/nix-haskell { inherit system; };
    project = import ./project.nix;
in (nix-haskell project).haskell-nix.project
