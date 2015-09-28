{ mkDerivation, dependent-map, ghcjs-dom, lens
, mtl, ref-tf, reflex, safe, text, these
, transformers, data-default, semigroups, aeson
, ghc, webkitgtk3-javascriptcore, exception-transformers
, webkitgtk24x, dependent-sum-template, bifunctors, zenc
, random
}:

mkDerivation {
  pname = "reflex-dom";
  version = "0.2";
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
    zenc
    random
  ] ++ (if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk3-javascriptcore ]);
  pkgconfigDepends = if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk24x ];
  license = null;
}
