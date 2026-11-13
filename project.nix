{ pkgs, nix-haskell-patches, ... }:

{
  imports = [
    (import "${nix-haskell-patches}/wasm/jsaddle-wasm" { drivers = [ "haskell-nix" ]; })
  ];

  name = "reflex-dom";
  src = ./.;

  haskell-nix = {
    options.shell.withHaddock = false;
  };

  nixpkgs = {
    # The packages sit in subdirectories, which this driver finds only
    # through a cabal plan.
    options.use-plan = true;

    packages = {
      # The plan carries the project's structure, not the flag assignments of an
      # arch-conditional stanza, and configuring exactly gives a flag the default
      # its cabal file declares. Both differ from what the `!arch(wasm32)` stanza
      # of cabal.project asks for, so assign them for this driver directly. The
      # generated assignments go first and Cabal takes the last one given, so
      # these still decide.
      reflex-dom.flags = {
        use-warp = true;
        webkit2gtk = false;
      };

      # nixpkgs builds a Haskell library with profiling on, which reruns
      # every Template Haskell splice in a second pass. GHCi cannot link
      # `deriveGEq` from dependent-sum-template that way:
      #
      #   GHC.ByteCode.Linker.lookupCE
      #   couldn't find closure:$fDeriveGEQName_$cderiveGEq
      #
      # Profiling is a property of a whole dependency chain rather than of
      # one package, so the dependents go with it. A profiling build of
      # reflex-dom looks for interface files reflex-dom-core no longer has.
      reflex-dom-core.enableLibraryProfiling = false;
      reflex-dom.enableLibraryProfiling = false;
    };

    options.overrides = [
      # splitmix's testu01 test-suite names the testu01 C library, which a
      # Haskell package set cannot resolve when splitmix is built from source.
      # The suite is behind a manual flag and never built.
      (_: _: { testu01 = null; })

      # The browser suites need chromium and selenium, and a HOME chromium can write to.
      (_: super: {
        reflex-dom-core = import ./reflex-dom-test-selenium/add-overrides.nix { nixpkgs = pkgs; } super.reflex-dom-core;
      })
    ];

    # This driver has no wasm compiler of its own worth building against,
    # so the matrix gives it a wasm target only through the ghc-wasm-meta
    # pin. Its shell follows. The release matrix adds the wasi32 shell
    # target when it applies that pin.
    shell.crossPlatforms = ps: with ps; [ ghcjs ];
  };

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs wasi32 ];
    packages = ps: with ps; [ reflex-dom reflex-dom-core ];
    withHoogle = false;
  };

}
