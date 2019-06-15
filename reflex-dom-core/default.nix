{ mkDerivation, aeson, base, bifunctors, bimap, blaze-builder
, bytestring, chrome-test-utils, constraints, constraints-extras, containers
, contravariant, data-default, dependent-map, dependent-sum
, dependent-sum-template, directory, exception-transformers
, exceptions, filepath, ghcjs-dom, hlint, hspec, hspec-webdriver, http-types, HUnit
, jsaddle, jsaddle-warp, keycode, lens, lifted-base
, monad-control, mtl, network, network-uri, primitive, process
, random, ref-tf, reflex, semigroups, silently, stdenv, stm
, template-haskell, temporary, text, these, transformers, unix, wai
, wai-websockets, warp, webdriver, websockets, zenc
, ghc, iproute, chromium, hashable, selenium-server-standalone, which, fontconfig
}:
let addGcTestDepends = drv: if (stdenv.system != "x86_64-linux" || stdenv.hostPlatform != stdenv.buildPlatform || (ghc.isGhcjs or false)) then drv else drv // {
      # The headless browser run as part of gc tests would hang/crash without this
      preCheck = ''
        export FONTCONFIG_PATH=${fontconfig.out}/etc/fonts
      '';
      testHaskellDepends = (drv.testHaskellDepends or []) ++ [
        temporary
        jsaddle-warp
        process
        chrome-test-utils
      ];
      testSystemDepends = (drv.testSystemDepends or []) ++ [ chromium iproute ];
    };
in mkDerivation (addGcTestDepends {
  pname = "reflex-dom-core";
  version = "0.5.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors bimap blaze-builder bytestring constraints
    containers contravariant data-default dependent-map dependent-sum
    dependent-sum-template directory exception-transformers ghcjs-dom
    jsaddle keycode lens monad-control mtl network-uri primitive random
    ref-tf reflex semigroups stm template-haskell text these
    transformers unix zenc
  ] ++ (if ghc.isGhcjs or false then [
    hashable
  ] else []);

  #TODO: Get hlint working for cross-compilation
  doCheck = stdenv.hostPlatform == stdenv.buildPlatform && !(ghc.isGhcjs or false);

  # The headless browser run as part of the tests will exit without this
  preBuild = ''
    export HOME="$PWD"
  '';

  # Show some output while running tests, so we might notice what's wrong
  testTarget = "--show-details=streaming";

  testSystemDepends = [ selenium-server-standalone which ];
  testHaskellDepends = [
    aeson base bytestring constraints constraints-extras containers
    dependent-map dependent-sum dependent-sum-template directory
    exceptions filepath ghcjs-dom hlint hspec hspec-webdriver http-types HUnit jsaddle
    jsaddle-warp lens lifted-base network process random reflex
    silently temporary text unix wai wai-websockets warp webdriver
    websockets
  ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
})
