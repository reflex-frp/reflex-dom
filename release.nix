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
            reflex = self.callHackageDirect {
              pkg = "reflex";
              ver = "0.8.2.1";
              sha256 = "10d1qkqwaqp9zaswmziaqgz60ifg5383d2i2ml2cqccn8943h26b";
            } {};
            patch = self.callHackageDirect {
              pkg = "patch";
              ver = "0.0.7.0";
              sha256 = "0yr2hk3fpwjxi1z0n384k3aq9b3z00c02bbwqybcj3n20l4k17l6";
            } {};
          })
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
