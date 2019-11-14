haskellPackages: let
  pkgs = haskellPackages.callPackage ({ pkgs }: pkgs) {};
  inherit (pkgs) stdenv;
  haskellLib = pkgs.haskell.lib;
in {
  reflex-dom-core = let
    inherit (haskellPackages) ghc;
    noGcTest = stdenv.hostPlatform.system != "x86_64-linux"
            || stdenv.hostPlatform != stdenv.buildPlatform
            || (ghc.isGhcjs or false);
  in haskellLib.overrideCabal
    (haskellPackages.callCabal2nix "reflex-dom-core" ./reflex-dom-core { })
    (drv: {
      #TODO: Get hlint working for cross-compilation
      doCheck = stdenv.hostPlatform == stdenv.buildPlatform && !(ghc.isGhcjs or false);

      # The headless browser run as part of the tests will exit without this
      preBuild = ''
        export HOME="$PWD"
      '';

      # Show some output while running tests, so we might notice what's wrong
      testTarget = "--show-details=streaming";

      testHaskellDepends = with haskellPackages; (drv.testHaskellDepends or []) ++ stdenv.lib.optionals (!noGcTest) [
        temporary
        jsaddle-warp
        process
        chrome-test-utils
      ];

      testSystemDepends = with pkgs; (drv.testSystemDepends or []) ++ [
        selenium-server-standalone which
      ] ++ stdenv.lib.optionals (!noGcTest) [
        chromium
        pkgs.iproute
      ];
    } // stdenv.lib.optionalAttrs (!noGcTest) {
      # The headless browser run as part of gc tests would hang/crash without this
      preCheck = ''
        export FONTCONFIG_PATH=${pkgs.fontconfig.out}/etc/fonts
      '';
    });
  reflex-dom = haskellLib.overrideCabal
    (haskellPackages.callCabal2nix "reflex-dom" ./reflex-dom { })
    (drv: {
      # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
      libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ stdenv.lib.optionals (with stdenv.hostPlatform; isAndroid && is32bit) [
        haskellPackages.android-activity
      ];
    });
  chrome-test-utils = haskellPackages.callCabal2nix "chrome-test-utils" ./chrome-test-utils {};
}
