{ mkDerivation, pkgs, dependent-map, ghcjs-dom, lens
, mtl, ref-tf, reflex, text, these
, transformers, data-default, semigroups, blaze-builder, aeson
, ghc, webkitgtk3-javascriptcore, exception-transformers
, webkitgtk24x, dependent-sum-template, bifunctors, bimap
, raw-strings-qq, zenc, random, monad-control, keycode, hlint
, unbounded-delays
}:

mkDerivation {
  pname = "reflex-dom";
  version = "0.3";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    aeson
    bifunctors
    bimap
    blaze-builder
    data-default
    dependent-map
    dependent-sum-template
    exception-transformers
    ghcjs-dom
    keycode
    lens
    monad-control
    mtl
    random
    ref-tf
    reflex
    semigroups
    text
    these
    transformers
    zenc
    unbounded-delays
  ] ++ (if ghc.isGhcjs or false then [] else [
    raw-strings-qq
    webkitgtk3-javascriptcore
  ]);
  testDepends = if ghc.isGhcjs or false then [] else [
    hlint
  ];
  pkgconfigDepends = if ghc.isGhcjs or false then [] else [
    raw-strings-qq
    webkitgtk24x
    webkitgtk3-javascriptcore
  ];
  license = null;
}
