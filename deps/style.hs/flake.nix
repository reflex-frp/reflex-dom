# DO NOT HAND-EDIT THIS FILE
{
  inputs = {
    "upstream" = {
      owner = "obsidiansystems";
      repo = "style.hs";
      rev = "417c71ee50898b700221c0ef10ccc6c1befb4500";
      type = "github";
      inputs = {
        "flake-compat" = { follows = "flake-compat"; };
        "nixpkgs" = { follows = "nixpkgs"; };
      };
    };
    "flake-compat" = {
      narHash = "sha256-vNpUSpF5Nuw8xvDLj2KCwwksIbjua2LZCqhV1LNRDns=";
      owner = "NixOS";
      repo = "flake-compat";
      rev = "5edf11c44bc78a0d334f6334cdaf7d60d732daab";
      type = "github";
    };
    "nixpkgs" = {
      narHash = "sha256-9mUW6gNwoN2SWc/l0fW4svPNOulXLl8ijqKyeSOGgJE=";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "a0374025a863d007d98e3297f6aa46cc3141c2f0";
      type = "github";
    };
  };
  outputs = inputs: inputs."upstream".outputs // { src = inputs."upstream"; };
}
