let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib;{
      diagrams-core = doJailbreak super.diagrams-core;
      diagrams-lib = doJailbreak super.diagrams-lib;
      diagrams-svg = doJailbreak super.diagrams-svg;
      diagrams-graphviz = doJailbreak super.diagrams-graphviz;
      diagrams-contrib = doJailbreak super.diagrams-contrib;
      diagrams-rasterific = doJailbreak super.diagrams-rasterific;
  };};
  ghcWithPackages =
    haskellPackages.ghcWithHoogle (g: with g;
    [classy-prelude
     hakyll
     hakyll-favicon
     hakyll-series
     base
     pandoc pandoc-types
     diagrams
     diagrams-contrib
     diagrams-graphviz
     diagrams-svg
     diagrams-rasterific
    ]);
in with pkgs;
  runCommand "hakyll-env"
    (with haskellPackages;
      rec
        { ghc = ghcWithPackages;
          shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
          buildInputs =
            [ ghcWithPackages zsh hindent hoogle hlint align graphviz-nox];})
  "echo success > $out"
