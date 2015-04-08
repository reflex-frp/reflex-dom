{ mkDerivation, dependent-map, ghcjs-dom, lens
, mtl, reflex, safe, text, these
, transformers, data-default, semigroups, aeson
, ghc, webkit-javascriptcore
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
    aeson
  ] ++ (if (ghc.pname or null) == "ghcjs" then [ ] else [ webkit-javascriptcore ]);
  license = null;
}
