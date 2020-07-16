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
          # Use this package's source for reflex
          (self: super: {
            _dep = super._dep // {
              reflex-dom = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
                ".git"
                "dist"
                "dist-newstyle"
              ]) && !(builtins.elem path [
                ./CONTRIBUTING.md
                ./FAQ.md
                ./Quickref.md
                ./README.md
                ./release.nix
                ./test
              ])) ./.;
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
