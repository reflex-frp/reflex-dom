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
    variations = map (v: "reflex-dom" + v) [
      ""
    ];
    compilerPkgs = lib.genAttrs compilers (ghc: let
      variationPkgs = lib.genAttrs variations (variation: let
        reflex-platform = reflex-platform-fun {
          inherit system;
          haskellOverlays = [
            # Use this package's source for reflex
            (self: super: {
              _dep = super._dep // {
                reflex-dom = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
                  "release.nix"
                  ".git"
                  "dist"
                ])) ./.;
              };
            })
          ];
        };
        all = {
          inherit (reflex-platform.${ghc})
            reflex-dom-core
            reflex-dom
            ;
        };
      in all // {
        cache = reflex-platform.pinBuildInputs "reflex-${system}-${ghc}-${variation}"
          (builtins.attrValues all);
      });
    in variationPkgs // {
      cache = reflex-platform.pinBuildInputs "reflex-${system}-${ghc}"
        (map (a: a.cache) (builtins.attrValues variationPkgs));
    });
  in compilerPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (map (a: a.cache) (builtins.attrValues compilerPkgs));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-dom-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
