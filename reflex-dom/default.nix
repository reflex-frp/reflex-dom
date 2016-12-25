{ mkDerivation, base, bytestring, jsaddle-webkit2gtk, reflex
, reflex-dom-core, stdenv, text, ghc
}:
mkDerivation {
  pname = "reflex-dom";
  version = "0.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring reflex reflex-dom-core text
  ] ++ (if ghc.isGhcjs or false then [] else [
    jsaddle-webkit2gtk
  ]);
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
