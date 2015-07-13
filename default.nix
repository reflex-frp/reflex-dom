{ mkDerivation, pkgs, dependent-map, ghcjs-dom, lens
, mtl, ref-tf, reflex, safe, text, these
, transformers, data-default, semigroups, aeson
, ghc, webkitgtk3-javascriptcore, exception-transformers
, dependent-sum-template, bifunctors
}:

mkDerivation {
  pname = "reflex-dom";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    reflex
    dependent-map
    mtl
    transformers
    these
    lens
    ghcjs-dom
    safe
    text
    data-default
    semigroups
    ref-tf
    aeson
    exception-transformers
    dependent-sum-template
    bifunctors
  ] ++ (if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk3-javascriptcore ]);
  license = null;
}
