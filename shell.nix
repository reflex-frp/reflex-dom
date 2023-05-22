{ compiler ? "reflex-platform" # or "ghc943", "ghc924"
}:
let
  rp = import ./dep/reflex-platform { __useNewerCompiler = true; };
  pkgs = rp.nixpkgs;
  system = builtins.currentSystem;
  nixGhc = ((import ./dep/nixpkgs {}).haskell.packages.${compiler}).override {
    overrides = self: super: {
      chrome-test-utils = self.callCabal2nix "chrome-test-utils" ./chrome-test-utils {};
      webdriver = self.callHackageDirect {
        pkg = "webdriver";
        ver = "0.10.0.0";
        sha256 = "0v7k03p3msaaq4l7a3hfbx9ank61lzmxliyamxd9q9r006s62nv4";
      } {};
      th-extras = pkgs.haskell.lib.doJailbreak super.th-extras;
      hspec-contrib = self.callHackageDirect {
        pkg = "hspec-contrib";
        ver = "0.5.1.1";
        sha256 = "0vr9jhlrdal3w4a1vzpm0w5nbisw5qgaqxchsgzpnh8n8q0v85z5";
      } {};
      hlint = self.callHackageDirect {
        pkg = "hlint";
        ver = "3.5";
        sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
      } {};
      patch = self.callHackageDirect {
        pkg = "patch";
        ver = "0.0.8.1";
        sha256 = "0q5rxnyilhbnfph48fnxbclggsbbhs0pkn0kfiadm0hmfr440cgk";
      } {};
      jsaddle = self.callCabal2nix "jsaddle" (rp.hackGet ./dep/jsaddle + "/jsaddle") {};
    };
  };
  pkgenv = if compiler == "reflex-platform"
    then (import ./release.nix {}).${system}.ghc.reflex-dom-core.env
    else (nixGhc.callCabal2nix "reflex-dom-core" (import ./src.nix) {}).env;
in
  pkgs.mkShell {
    name = "shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      pkgenv
    ];
  }
