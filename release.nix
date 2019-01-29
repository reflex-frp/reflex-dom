{ rp ? import ./reflex-platform.nix {}
}:
let
  inherit (rp.nixpkgs) lib;
  compilers = ["ghc8_4" "ghc8_0" "ghcjs8_4" "ghcjs8_0"];
in lib.genAttrs compilers (ghc: {
  inherit (import ./. rp.${ghc} rp.nixpkgs) reflex-dom reflex-dom-core;
})
