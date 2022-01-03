{ system ? "x86_64-linux" }:
let
  rp = import ./dep/reflex-platform {};
  nixpkgsSets = rp.thunkSet ./.ci;
  nixos2003 = import nixpkgsSets."nixos-20.03" { inherit system; };
  nixos2009 = import nixpkgsSets."nixos-20.09" { inherit system; };
  unstable = import nixpkgsSets.nixpkgs-unstable { inherit system; };
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  nixpkgs = nixos2003;

  unpack = tarball: nixpkgs.runCommand "tar-unpack" {} ''
    mkdir "$out"
    "${nixpkgs.gnutar}/bin/tar" xf ${tarball} --strip-components 1 -C "$out"
  '';

  getSource = pkg: ver: sha256:
    let pkgver = "${pkg}-${ver}";
    in  unpack (builtins.fetchurl {
          url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
          inherit sha256;
        });

  buildFromHackage = self: pkg: ver: sha256: 
    self.callCabal2nix pkg (getSource pkg ver sha256) {};

  sedCabal = sedCmd: path: nixpkgs.runCommand "sed_cabal" {} ''
    mkdir $out
    cp -rL ${path}/* $out
    ${nixpkgs.gnused}/bin/sed -i '${sedCmd}' $out/*.cabal
  '';
  common-overrides = self: super: {
    reflex-dom-test-selenium = self.callCabal2nix "reflex-dom-test-selenium" ./reflex-dom-test-selenium {};
    chrome-test-utils = self.callCabal2nix "chrome-test-utils" ./chrome-test-utils {};
    reflex-dom-core = self.callCabal2nix "reflex-dom-core" ./reflex-dom-core {};
    reflex-dom = self.callCabal2nix "reflex-dom" ./reflex-dom {};
    hspec-webdriver = self.callCabal2nix "hspec-webdriver" (nixpkgs.fetchFromGitHub {
      owner = "dfordivam";
      repo = "hspec-webdriver-clone";
      rev = "0d748b7bb7cd74dce0a55a1ec86b01dbb8a71cd8";
      sha256 = "1criynifhvmnqwhrshmzylikqkvlgq98xf72w9cdd2zpjw539qf0";
    }) {};
    patch = self.callHackageDirect {
      pkg = "patch";
      ver = "0.0.5.0";
      sha256 = "1x1rbi51r5gvbkg96884c2py7in4n0ijh5ins8ya3b5kga32siq4";
    } {};
    reflex = self.callHackageDirect {
      pkg = "reflex";
      ver = "0.8.2.0";
      sha256 = "09cb0yjmwc1ycyvvc2p265w7wivp3xgwfmlmpv3gi8b4zmvwn32i";
    } {};

  };
  old-ghc-overrides = self: super: {
    semialign = buildFromHackage self "semialign" "1" "004x0a80sqqdgvsyk4z0nasxpi6z3g1d8kgwj804bj9ka8dlc75m";
  };
  ghcs = rec {
    ghc844 = nixos2003.haskell.packages.ghc844.override {
      overrides = self: super: common-overrides self super // old-ghc-overrides self super // {
        # This somewhat gross override is to get around the fact that
        # witherable declares a license and cabal version not supported by this
        # (older) version of nixpkgs.  However, the cabal file is actually
        # cabal-1.8 compatible.
        witherable = doJailbreak (self.callCabal2nix "witherable"
          (sedCabal "s/BSD-3-Clause/BSD3/g"
            (sedCabal "s/cabal-version:       2.4/cabal-version: 1.8/g"
              (rp.hackGet ./dep/witherable + "/witherable"))) {});
        base-compat = buildFromHackage self "base-compat" "0.10.5" "0hgvlqcr852hfp52jp99snhbj550mvxxpi8qn15d8ml9aqhyl2lr";
        tagged = buildFromHackage self "tagged" "0.8.6.1" "00kcc6lmj7v3xm2r3wzw5jja27m4alcw1wi8yiismd0bbzwzrq7m";
        indexed-traversable = self.callHackageDirect {
          pkg = "indexed-traversable";
          ver = "0.1.1";
          sha256 = "1r5hvz6c90qcjc6r79r1vdv38l898saiv0027xzknlp48hcx8292";
        } {};
        indexed-traversable-instances = dontCheck (doJailbreak (self.callHackageDirect {
          pkg = "indexed-traversable-instances";
          ver = "0.1.1";
          sha256 = "0bcsxhaic3m2sj64r7dfgp0ns2gyijp8xxga4yigbhx3j6vy5894";
        } {}));
        hspec-webdriver = self.callCabal2nix "hspec-webdriver" (nixpkgs.fetchFromGitHub {
          owner = "dfordivam";
          repo = "hspec-webdriver-clone";
          rev = "0d748b7bb7cd74dce0a55a1ec86b01dbb8a71cd8";
          sha256 = "1criynifhvmnqwhrshmzylikqkvlgq98xf72w9cdd2zpjw539qf0";
        }) {};
        chell = self.callHackageDirect {
          pkg = "chell";
          ver = "0.5";
          sha256 = "18yfxm337w9zx614wbw93ar7zkq0zz59rmcq8cl332crdpnylsv8";
        } {};
        system-fileio = doJailbreak super.system-fileio;
        jsaddle-warp = dontCheck (self.callHackageDirect {
          pkg = "jsaddle-warp";
          ver = "0.9.7.0";
          sha256 = "1jd2l1mkh0y6797c10q5dmxv04blkal5bbjrgb806j6d8ssi6pjc";
        } {});
        patience = self.callHackageDirect {
          pkg = "patience";
          ver = "0.2.0.0";
          sha256 = "1ldhx9nzv0v64hdvzyqlh4xpir37sdvfib6mvkzhwi7dfyr25431";
        } {};
        warp = dontCheck (self.callHackageDirect {
          pkg = "warp";
          ver = "3.2.28";
          sha256 = "1v3iipryla6966yimp75cyckjs54gnh6h1xxx0m1yw72aa2bpkiv";
        } {});
        http2 = self.callHackageDirect {
          pkg = "http2";
          ver = "1.6.5";
          sha256 = "0q0n11hxmiay45mr6bjd98dwg2d42x2xl2f7inzwaq0rgpcnlrvv";
        } {};
        OneTuple = doJailbreak (self.callHackageDirect {
          pkg = "OneTuple";
          ver = "0.3";
          sha256 = "1xs5zmg1dq815gbb35khbj6jp64f7zgk1hfy8pyf7srm22cjd2dz";
        } {});
        base-orphans = self.callHackageDirect {
          pkg = "base-orphans";
          ver = "0.8.6";
          sha256 = "17hplm1mgw65jbszg5z4vqk4i24ilxv8mbszr3s8lhpll5naik26";
        } {};
      };
    };
    ghc865 = nixos2009.haskell.packages.ghc865.override {
      overrides = self: super: common-overrides self super // {
        reflex = buildFromHackage self "reflex" "0.8.2.0" "1hvagxcs413bqairxf77vp19484mxnbfckhd44wv22ncwfh5mq6d";
      };
    };
    ghc884 = nixos2009.haskell.packages.ghc884.override {
      overrides = common-overrides;
    };
    ghc8102 = nixos2009.haskell.packages.ghc8102.override {
      overrides = common-overrides;
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.reflex-dom) ghcs
