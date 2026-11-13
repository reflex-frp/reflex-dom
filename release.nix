# Every combination of driver, compiler and target reflex-dom is meant to
# work for. Each is built two ways: as the drivers build it, and as a person
# would inside the project's shell. `checks` is the style checks and the
# reexport test.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A build
#   nix-build release.nix -A build.haskell-nix.ghc914.wasi32
#   nix-build release.nix -A shell-build.nixpkgs.ghc912.ghcjs
#   nix-build release.nix -A checks
#   nix-instantiate --eval --strict --json release.nix -A attr-paths
#
# `wasm-meta` is the same matrix with the wasm target's compiler taken from
# the ghc-wasm-meta pin instead of the driver's own. The nixpkgs driver has
# no 9.14 package set worth building against and no wasm compiler of its
# own, so outside `wasm-meta` it builds only 9.12, for native and ghcjs.
{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };

    variant = modules: nix-haskell ([ (import ./project.nix) ] ++ modules);

    stock = variant [];

    lib = stock.pkgs.lib;

in with (import (inputs.nix-haskell + "/libs/prelude") { inherit lib; });

let
    # The one matrix of driver, compiler series and wasm-meta. A cell is a
    # project, the driver that builds it, the platforms it targets, and the
    # path it takes in every result set.
    cells = [

      { path = [ "haskell-nix" "ghc912" ]; project = seriesProject "9.12" []; driver = "haskell-nix"; platforms = platforms; }
      { path = [ "haskell-nix" "ghc914" ]; project = seriesProject "9.14" []; driver = "haskell-nix"; platforms = platforms; }

      # This driver has no wasm compiler of its own worth building against.
      { path = [ "nixpkgs" "ghc912" ]; project = seriesProject "9.12" []; driver = "nixpkgs"; platforms = [ "native" "ghcjs" ]; }

      { path = [ "wasm-meta" "haskell-nix" "ghc912" ]; project = seriesProject "9.12" [ (wasmMeta "9.12") ]; driver = "haskell-nix"; platforms = platforms; }
      { path = [ "wasm-meta" "haskell-nix" "ghc914" ]; project = seriesProject "9.14" [ (wasmMeta "9.14") ]; driver = "haskell-nix"; platforms = platforms; }
      { path = [ "wasm-meta" "nixpkgs" "ghc912" ]; project = seriesProject "9.12" [ (wasmMeta "9.12") ]; driver = "nixpkgs"; platforms = platforms; }
      { path = [ "wasm-meta" "nixpkgs" "ghc914" ]; project = seriesProject "9.14" [ (wasmMeta "9.14") ]; driver = "nixpkgs"; platforms = platforms; }

    ];

    seriesProject = series: modules:
      variant ([ { compiler.name = ghcName series; } strict ] ++ modules);

    ghcName = series: "ghc${lib.replaceStrings [ "." ] [ "" ] series}";

    # The bindist is a wasm compiler the nixpkgs driver can carry cross
    # tools for, and the driver's own is not. The shell therefore gains
    # the target here rather than in the project. `mkForce`, since the
    # option is typed `unspecified` and two selectors would otherwise be
    # merged by applying both and joining what they return.
    wasmMeta = series: {
      imports = [ (import ./wasm-meta.nix { inherit series; }) ];
      nixpkgs.shell.crossPlatforms = lib.mkForce (ps: with ps; [ ghcjs wasi32 ]);
    };

    # The tree of one result set: each cell's derivations at the cell's
    # path, every level marked for `nix-build`.
    matrix = set:
      let subtrees = map (cell: lib.setAttrByPath cell.path (set.leaf cell)) cells;
      in recurse-for-derivations (lib.foldl lib.recursiveUpdate {} subtrees);

    # Each result set: what a cell builds, and the names of a cell's entries
    # relative to the set. The names come from the platform lists alone, so a
    # CI matrix reads them without evaluating a derivation.
    resultSets = {

      build = {
        leaf = cell: lib.genAttrs cell.platforms (built cell);
        names = cell: targetEntries cell cell.platforms;
      };

      shell-build = {
        leaf = cell: lib.genAttrs cell.platforms (shellBuild cell);
        names = cell: targetEntries cell cell.platforms;
      };

    };

    # reflex-dom's libraries, as one driver builds them for one platform.
    built = cell: platform:
      lib.genAttrs libraries (driverLibrary.${cell.driver} (driverProject cell platform));

    driverProject = cell: platform:
      let native = cell.project.${cell.driver}.project;
      in if platform == "native"
         then native
         else native.projectCross.${platform};

    # haskell.nix gives a package a tree of components, the nixpkgs driver one
    # derivation.
    driverLibrary = {
      haskell-nix = proj: name: proj.hsPkgs.${name}.components.library;
      nixpkgs = proj: name: proj.packages.${name};
    };

    # The build a person would run, in the shell they would run it in. cabal
    # needs no package index: the shell's package database already carries
    # everything the project depends on. A configuration naming no
    # repository keeps cabal from fetching one it cannot reach.
    # One `buildCommand` rather than phases: the nixpkgs driver's shell states
    # its own, which stands in for the whole phase list, so a phase written
    # here would never run in that shell.
    shellBuild = cell: platform:
      let cross = dispatcher cell platform;

          toolchain = if cross == "" then "native" else cross;

      in cell.project.${cell.driver}.project.shell.overrideAttrs (old: {
        # The toolchain, not the target: it names the target and tells the
        # bindist cells apart from the ones a driver builds itself.
        name = "reflex-dom-${cell.driver}-${toolchain}-shell-build";

        buildCommand = ''
          # What entering the shell runs. It keeps the native and the cross
          # library paths apart, which every native link here needs.
          eval "$shellHook"

          cp -r ${cell.project.config.src-cleaned} source
          chmod -R u+w source
          cd source

          # The sandbox has no network to fetch a source-repository-package
          # with, and the shell's package database already carries each one.
          awk '/^source-repository-package/ { skip = 1; next }
               skip && /^[[:space:]]/ { next }
               { skip = 0; print }' cabal.project > cabal.project.pinless
          mv cabal.project.pinless cabal.project

          export HOME=$TMPDIR
          export CABAL_CONFIG=$TMPDIR/cabal.config
          echo 'jobs: $ncpus' > $CABAL_CONFIG
          echo 'active-repositories: :none' >> cabal.project.local
          echo 'tests: False' >> cabal.project.local
          echo 'benchmarks: False' >> cabal.project.local

          ${cross} cabal build --offline lib:reflex-dom lib:reflex-dom-core
          touch $out
        '';
      });

    # The wrapper that puts a cross target's tools ahead of the ones the
    # shell carries for the build platform. It is named after the target's
    # own prefix, so the compiler a target is built with decides it.
    dispatcher = cell: platform:
      if platform == "native"
      then ""
      else lib.removeSuffix "-"
        (cell.project.config.${cell.driver}.cross-compiler platform).targetPrefix;

    # The name `nix-build -A` takes for a path through the tree.
    dotted = path: lib.concatStringsSep "." path;

    # The names of the chosen targets under a cell.
    targetEntries = cell: chosen:
      map (target: dotted (cell.path ++ [ target ])) chosen;

    # The names of every entry of one result set, under the set's name.
    setEntries = name: set:
      let cellEntries = cell: map (relative: "${name}.${relative}") (set.names cell);
      in lib.concatMap cellEntries cells;

    released = lib.mapAttrs (_: matrix) resultSets // {
      checks = recurse-for-derivations checks;
    };

    checks = {
      fourmolu = style.fourmolu-check;
      hlint = style.hlint-check;
      reexports = (import ./test { inherit system inputs; pkgs = stock.pkgs; }).test-reflex-dom-reexports;
    };

    style = import ./style.nix { inherit system inputs; pkgs = stock.pkgs; };

    strict = {
      packages = lib.genAttrs libraries (_: { flags.strict = true; });

      # A driver's own flags for a package replace the common ones, and
      # project.nix gives the nixpkgs driver flags of its own for reflex-dom.
      nixpkgs.packages = lib.genAttrs libraries (_: { flags.strict = true; });
    };

    platforms = [ "native" "ghcjs" "wasi32" ];

    libraries = [ "reflex-dom" "reflex-dom-core" ];

in released // {

  all = stock.pkgs.linkFarm "reflex-dom-release"
    (link-farm-entries "" released);

  # Every derivation here, named the way `nix-build -A` takes it, for a CI
  # matrix that builds each on its own runner.
  #
  # Example:
  #
  #   attr-paths
  #   => [ "build.haskell-nix.ghc912.native"
  #        ...
  #        "shell-build.wasm-meta.nixpkgs.ghc914.wasi32"
  #        "checks.fourmolu"
  #        "checks.hlint"
  #        "checks.reexports" ]
  attr-paths = lib.concatLists (lib.mapAttrsToList setEntries resultSets)
    ++ map (name: "checks.${name}") (lib.attrNames checks);

}
