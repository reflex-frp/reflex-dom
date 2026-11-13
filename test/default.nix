{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix { inherit system; }
, pkgs ? inputs.pkgs
}:
{
  test-reflex-dom-reexports = pkgs.runCommand "test-reflex-dom-reexports" {} (
    let
      ghc = pkgs.haskellPackages.ghcWithPackages (p: [p.Cabal]);
    in ''
      ${ghc}/bin/runhaskell ${./reflex-dom-reexports.hs} ${../reflex-dom/reflex-dom.cabal} ${../reflex-dom-core/reflex-dom-core.cabal}
      touch "$out"
    '');
}
