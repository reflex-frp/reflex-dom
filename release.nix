{ reflex-platform-fun ? import ./dep/reflex-platform
, supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;

  tests = system: import ./test { pkgs = (reflex-platform-fun { inherit system; }).nixpkgs; };

  perPlatform = lib.genAttrs supportedSystems (system: let
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
    compilerPkgs = lib.genAttrs compilers (ghc: let
      reflex-platform = reflex-platform-fun {
        inherit system;
        haskellOverlays = [
          (self: super: {
            commutative-semigroups = self.callHackageDirect {
              pkg = "commutative-semigroups";
              ver = "0.1.0.0";
              sha256 = "0xmv20n3iqjc64xi3c91bwqrg8x79sgipmflmk21zz4rj9jdkv8i";
            } {};
          })
          # Use this package's source for reflex
          (self: super: {
            _dep = super._dep // {
              reflex-dom = import ./src.nix;
            };
          })
        ];
      };
      all = tests system // {
        inherit (reflex-platform.${ghc})
          reflex-dom-core
          reflex-dom
          ;
      };
    in all // {
      cache = reflex-platform.pinBuildInputs "reflex-${system}-${ghc}"
        (builtins.attrValues all);
    });
  in compilerPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (map (a: a.cache) (builtins.attrValues compilerPkgs));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-dom-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
