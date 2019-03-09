haskellPackages: nixpkgs: {
  reflex-dom = haskellPackages.callPackage ./reflex-dom {};
  reflex-dom-core = haskellPackages.callPackage ./reflex-dom-core {
    inherit (nixpkgs) iproute chromium;
  };
  chrome-test-utils = haskellPackages.callCabal2nix "chrome-test-utils" ./chrome-test-utils {};
}
