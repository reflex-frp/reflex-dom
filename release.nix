{ rp ? import ./reflex-platform.nix {}
}:
let
  inherit (rp.nixpkgs) lib;
  systems = ["x86_64-linux" "x86_64-darwin"];
  compilers = ["ghc8_4" "ghc8_0" "ghcjs8_4" "ghcjs8_0"];
in lib.genAttrs systems (system:
  lib.genAttrs compilers (ghc: let
    reflex-platform = import ./reflex-platform.nix { inherit system; };
    hsPkgs = reflex-platform.${ghc};
  in { inherit (import ./. hsPkgs reflex-platform.nixpkgs) reflex-dom reflex-dom-core; })
)
