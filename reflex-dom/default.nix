{ mkDerivation, base, bytestring, jsaddle-webkit2gtk, jsaddle-wkwebview, reflex
, reflex-dom-core, stdenv, text, ghc
}:
mkDerivation {
  pname = "reflex-dom";
  version = "0.4";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    base bytestring reflex reflex-dom-core text
  ] ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.isDarwin then [
    jsaddle-wkwebview
  ] else [
    jsaddle-webkit2gtk
  ]);
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
