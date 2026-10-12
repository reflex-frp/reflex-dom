{ pkgs }:
self: super:
let
  # https://github.com/ghcjs/jsaddle/pull/172
  source = builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/jsaddle/archive/2152422657602dc29c89140cebd3d7fae080acf1.tar.gz";
    sha256 = "0rms310psq04y3kflh3skn5d7nn7052y513hs3cp1p77rmqvbl62";
  };
  packageSource = source + "/jsaddle-warp";
  lock = builtins.fromJSON (builtins.readFile (packageSource + "/node-client/package-lock.json"));
  nodeTarballs = map (name:
    let dependency = lock.packages."node_modules/${name}";
    in pkgs.buildPackages.fetchurl {
      url = dependency.resolved;
      hash = dependency.integrity;
    }
  ) [ "ws" "xmlhttprequest" ];
in {
  # Native Warp needs a newer core than the pinned platform provides.
  jsaddle = if super.jsaddle-warp == null
    || (self.ghc.isGhcjs or false)
    || pkgs.lib.versionAtLeast super.jsaddle.version "0.9.9.0"
    then super.jsaddle
    else self.callHackageDirect {
      pkg = "jsaddle";
      ver = "0.9.9.4";
      sha256 = "sha256-6agO7Hj3wPFHHXV3hDWXt9LWW2h70OqvsM9v8n5dR5k=";
    } {};

  jsaddle-warp = if super.jsaddle-warp == null then null else
    pkgs.haskell.lib.overrideCabal
      (self.callCabal2nix "jsaddle-warp" packageSource {})
      (old: {
        doCheck = super.jsaddle-warp.doCheck or false;
        libraryToolDepends = (old.libraryToolDepends or []) ++ [ pkgs.buildPackages.nodejs ];
        # Cabal installs these Node client assets as package data.
        preConfigure = (old.preConfigure or "") + ''
          export npm_config_cache="$TMPDIR/npm-cache"
          npm cache add --offline --ignore-scripts ${pkgs.lib.escapeShellArgs nodeTarballs}
          npm ci --offline --ignore-scripts --prefix node-client
        '';
      });
}
