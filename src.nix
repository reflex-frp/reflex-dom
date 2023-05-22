builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
  ".git"
  "dist"
  "dist-newstyle"
]) && !(builtins.elem path [
  ./CONTRIBUTING.md
  ./FAQ.md
  ./Quickref.md
  ./README.md
  ./release.nix
  ./test
])) ./reflex-dom-core
