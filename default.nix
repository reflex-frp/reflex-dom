{ cabal, dependentMap, ghcjsDom, lens
, mtl, reflex, safe, text, these
, transformers, dataDefault
}:

cabal.mkDerivation (self: {
  pname = "reflex-dom";
  version = "0.1";
  src = ./.;
  buildDepends = [
    reflex
    dependentMap
    mtl
    transformers
    these
    lens
    ghcjsDom
    safe
    text
    dataDefault
  ];
  meta = {
    description = "Functional Reactive DOM widgets";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
