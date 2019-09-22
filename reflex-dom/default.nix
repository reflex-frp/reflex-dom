{ mkDerivation, base, bytestring, jsaddle-webkit2gtk, jsaddle-wkwebview, jsaddle-warp, reflex
, reflex-dom-core, stdenv, text, ghc, hostPlatform, jsaddle-clib, android-activity ? null
, ghcBackend ? if hostPlatform.isDarwin then "wkwebview" else "webkit2gtk"
}:
assert (builtins.elem ghcBackend [ "warp" "webkit2gtk" "wkwebview" ]);
let isAndroid = hostPlatform.libc == "bionic";
    ghcBackendPackage = {
      webkit2gtk = jsaddle-webkit2gtk;
      warp = jsaddle-warp;
      wkwebview = jsaddle-wkwebview;
    }.${ghcBackend};
in mkDerivation {
  pname = "reflex-dom";
  version = "0.5.2";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    base bytestring reflex reflex-dom-core text
  ] ++ (if ghc.isGhcjs or false then [
  ] else if hostPlatform.isiOS then [
    jsaddle-wkwebview
  ] else if isAndroid then [
    jsaddle-clib
    android-activity
  ] else if hostPlatform.isMacOS then [
    jsaddle-wkwebview
    ghcBackendPackage
  ] else [
    ghcBackendPackage
  ]);
  configureFlags = if ghcBackend == "warp" then [
    "-fuse-warp"
  ] else [
  ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
