haskellPackages: let
  pkgs = haskellPackages.callPackage ({ pkgs }: pkgs) {};
  inherit (pkgs) stdenv;
  haskellLib = pkgs.haskell.lib;
in {
  reflex-dom = haskellPackages.callPackage ./reflex-dom {};
  reflex-dom-core = haskellPackages.callPackage ./reflex-dom-core {
    inherit (pkgs) iproute chromium;
  };
  chrome-test-utils = haskellPackages.callCabal2nix "chrome-test-utils" ./chrome-test-utils {};
}
