{ system ? builtins.currentSystem, inputs ? {} }:

let nix-haskell = import ./deps/nix-haskell { inherit system inputs; };
    project = import ./project.nix;

in (nix-haskell project).haskell-nix.project
