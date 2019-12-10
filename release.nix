{ reflex-platform-fun ? import ./dep/reflex-platform
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;
  systems = ["x86_64-linux" "x86_64-darwin"];

  perPlatform = lib.genAttrs systems (system: let
    reflex-platform = reflex-platform-fun { inherit system; };
    compilers = [
      "ghc"
      "ghcjs"
    ] ++ lib.optionals (reflex-platform.androidSupport) [
      "ghcAndroidAarch64"
      "ghcAndroidAarch32"
    ] ++ lib.optionals (reflex-platform.iosSupport) [
      "ghcIosAarch64"
    ];
    hsPkgs = lib.genAttrs compilers (ghc: let
      ghc' = reflex-platform.${ghc}.override {
        overrides = self: super: import ./. self;
      };
    in {
      inherit (ghc') reflex-dom reflex-dom-core;
    });
  in hsPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-dom-${system}"
      (lib.concatLists (map builtins.attrValues (builtins.attrValues hsPkgs)));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-dom-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
