{ mkDerivation, base, bytestring, jsaddle-webkit2gtk, jsaddle-wkwebview, jsaddle-warp, reflex
, reflex-dom-core, stdenv, text, ghc, hostPlatform, jsaddle-clib, android-activity ? null
}:
let isAndroid = hostPlatform.libc == "bionic";
in mkDerivation {
  pname = "reflex-dom";
  version = "0.4";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    base bytestring reflex reflex-dom-core text
  ] ++ (if ghc.isGhcjs or false then [
  ] else if hostPlatform.isDarwin then [
    jsaddle-wkwebview
    jsaddle-warp
  ] else if isAndroid then [
    jsaddle-clib
    android-activity
  ] else [
    jsaddle-webkit2gtk
    jsaddle-warp
  ]);
  configureFlags = if isAndroid then [ "-fandroid" ] else [];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
