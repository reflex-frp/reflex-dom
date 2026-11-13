{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
, pkgs ? inputs.pkgs
}:

let style = inputs."style.hs";

    fourmolu = import (style + "/fourmolu.nix") { inherit system pkgs; };
    hlint = import (style + "/hlint.nix") { inherit system pkgs; };

    styled = [
      ./chrome-test-utils
      ./reflex-dom
      ./reflex-dom-core
      ./reflex-dom-test-selenium
      ./test
    ];
    sources = map (path: "${path}") styled;

    check = name: tool: command:
      pkgs.runCommand name { nativeBuildInputs = [ tool ]; } ''
        ${command} ${toString sources}
        touch $out
      '';

    fourmolu-check = check "fourmolu-check" fourmolu "fourmolu --mode check --config ${fourmoluConfig}";
    hlint-check = check "hlint-check" hlint "hlint --with-group=generalise";

    fourmoluAdditions = ''
      fixities:
        - infixl 1 &
        - infixr 4 .~, %~
        - infixl 8 ^., ^.., ^?
    '';

    fourmoluConfig = pkgs.writeText "fourmolu.yaml" (builtins.readFile "${style}/fourmolu.yaml" + "\n" + fourmoluAdditions);

    syncConfig = source: target: ''
      cmp -s ${source} "${target}" \
        || install -m 544 ${source} "${target}"
    '';

    # Keeps a copy of each shared style config next to the code, where the
    # tools and editors look for it. The thunk's configs, with this repo's
    # additions, are the source of truth: a copy that differs is overwritten.
    syncConfigs = dir: ''
      ${syncConfig fourmoluConfig "${dir}/fourmolu.yaml"}
      ${syncConfig "${style}/hlint.yaml" "${dir}/.hlint.yaml"}
      ${syncConfig "${style}/STYLE.md" "${dir}/STYLE.md"}
    '';
in {
  inherit fourmolu hlint fourmolu-check hlint-check syncConfigs;

  checks = pkgs.linkFarmFromDrvs "style-checks" [ fourmolu-check hlint-check ];

  shell = pkgs.mkShell { packages = [ fourmolu hlint ]; };
}
