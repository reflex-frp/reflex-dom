{ useReflexOptimizer ? false
, filterSource ? (x: x)
}:
self: super:
let
  pkgs = self.callPackage ({pkgs}: pkgs) {};
  inherit (pkgs) lib stdenv;

  reflexOptimizerFlag = lib.optional (useReflexOptimizer && (self.ghc.cross or null) == null) "-fuse-reflex-optimizer";

in pkgs.haskell.lib.overrideCabal
  (self.callCabal2nixWithOptions "reflex-dom" (filterSource ./.) (lib.concatStringsSep " " (lib.concatLists [
    reflexOptimizerFlag
  ])) {})
  (drv: {
    # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
    libraryHaskellDepends = (drv.libraryHaskellDepends or [])
      ++ stdenv.lib.optionals (with stdenv.hostPlatform; isAndroid && is32bit) [
      self.android-activity
    ] ++ stdenv.lib.optionals (with stdenv.hostPlatform; isWasm && is32bit) [
      self.jsaddle-wasm
    ];
  })
