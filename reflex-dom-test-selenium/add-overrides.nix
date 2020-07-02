{ nixpkgs }: testPackage:
  nixpkgs.haskell.lib.overrideCabal testPackage  
    (drv: {

      # The headless browser run as part of the tests will exit without this
      preBuild = ''
        export HOME="$PWD"
      '';

      testSystemDepends = with nixpkgs; (drv.testSystemDepends or []) ++ [
        selenium-server-standalone which
        chromium
        nixpkgs.iproute
      ];

      # The headless browser run as part of gc tests would hang/crash without this
      preCheck = ''
        export FONTCONFIG_PATH=${nixpkgs.fontconfig.out}/etc/fonts
      '';
    })