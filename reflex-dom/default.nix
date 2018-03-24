{ mkDerivation, base, bytestring, jsaddle-webkit2gtk, jsaddle-wkwebview, jsaddle-warp, reflex
, reflex-dom-core, stdenv, text, ghc, hostPlatform, jsaddle-clib, android-activity ? null
, http-types , wai, wai-websockets, warp, websockets
, ghcBackend ? "webkit2gtk"
}:
assert (builtins.elem ghcBackend [ "warp" "webkit2gtk" ]);
let isAndroid = hostPlatform.libc == "bionic";
    ghcBackendPackage = {
      webkit2gtk = jsaddle-webkit2gtk;
      warp = jsaddle-warp;
    }.${ghcBackend};
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
    android-activity
    http-types
    jsaddle-clib
    wai
    wai-websockets
    warp
    websockets
  ] else [
    ghcBackendPackage
  ]);
  configureFlags = if isAndroid then [
    "-fandroid"
  ] else if ghcBackend == "warp" then [
    "-fuse-warp"
  ] else [
  ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
