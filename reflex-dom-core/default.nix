{ mkDerivation, ghc, aeson, base, bifunctors, bimap, blaze-builder
, bytestring, containers, contravariant, data-default
, dependent-map, dependent-sum, dependent-sum-template, directory
, exception-transformers, ghcjs-dom, hlint, jsaddle, keycode, lens
, monad-control, mtl, primitive, random, ref-tf, reflex, semigroups
, stdenv, stm, template-haskell, temporary, text, these, time
, transformers, unbounded-delays, unix, zenc, hashable, xvfb_run
, chromium, process, jsaddle-warp
}:
let addGcTestDepends = drv: if stdenv.system != "x86_64-linux" then drv else drv // {
      testHaskellDepends = (drv.testHaskellDepends or []) ++ [ temporary jsaddle-warp process ];
      testSystemDepends = (drv.testSystemDepends or []) ++ [ xvfb_run chromium ];
    };
in mkDerivation (addGcTestDepends {
  pname = "reflex-dom-core";
  version = "0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors bimap blaze-builder bytestring containers
    contravariant data-default dependent-map dependent-sum
    dependent-sum-template directory exception-transformers ghcjs-dom
    jsaddle keycode lens monad-control mtl primitive random ref-tf
    reflex semigroups stm template-haskell text these time transformers
    unbounded-delays unix zenc
  ] ++ (if ghc.isGhcjs or false then [
    hashable
  ] else []);
  testHaskellDepends = [ base hlint ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
})
