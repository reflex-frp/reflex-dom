{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
, pkgs ? inputs.pkgs
}:

let project = import ./default.nix { inherit system inputs; };
    style = import ./style.nix { inherit system inputs pkgs; };

    withStyle = shell: pkgs.mkShell {
      inputsFrom = [ shell ];
      packages = [
        style.fourmolu
        style.hlint
      ];
      shellHook = style.syncConfigs "$PWD";
    };

in {
  haskell-nix = withStyle project.haskell-nix.shell;
  nixpkgs = withStyle project.nixpkgs.shell;

  haskell-nix-wasm-meta = withStyle project.haskell-nix-wasm-meta.shell;
  nixpkgs-wasm-meta = withStyle project.nixpkgs-wasm-meta.shell;
}
