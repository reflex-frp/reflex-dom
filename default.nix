{ mkDerivation, pkgs, dependent-map, ghcjs-dom, lens
, mtl, ref-tf, reflex, text, these
, transformers, data-default, semigroups, aeson
, ghc, webkitgtk3-javascriptcore, exception-transformers
, dependent-sum-template, bifunctors, bimap, raw-strings-qq
}:

mkDerivation {
  pname = "reflex-dom";
  version = "0.3";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    reflex
    dependent-map
    mtl
    transformers
    these
    lens
    ghcjs-dom
    text
    data-default
    semigroups
    ref-tf
    aeson
    exception-transformers
    dependent-sum-template
    bifunctors
    bimap
  ] ++ (if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk3-javascriptcore raw-strings-qq ]);
  license = null;
}
