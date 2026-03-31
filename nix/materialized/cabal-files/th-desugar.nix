{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "th-desugar"; version = "1.17"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Richard Eisenberg <rae@cs.brynmawr.edu>";
      homepage = "https://github.com/goldfirere/th-desugar";
      url = "";
      synopsis = "Functions to desugar Template Haskell";
      description = "This package provides the Language.Haskell.TH.Desugar module, which desugars\nTemplate Haskell's rich encoding of Haskell syntax into a simpler encoding.\nThis desugaring discards surface syntax information (such as the use of infix\noperators) but retains the original meaning of the TH code. The intended use\nof this package is as a preprocessor for more advanced code manipulation\ntools. Note that the input to any of the ds... functions should be produced\nfrom a TH quote, using the syntax [| ... |]. If the input to these functions\nis a hand-coded TH syntax tree, the results may be unpredictable. In\nparticular, it is likely that promoted datatypes will not work as expected.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."ordered-containers" or (errorHandler.buildDepError "ordered-containers"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
        ];
        buildable = true;
      };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
            (hsPkgs."th-desugar" or (errorHandler.buildDepError "th-desugar"))
            (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-desugar-1.17.tar.gz";
      sha256 = "9f879fca6e952c75d53d4c923cf0162aaacca466cde71f1718147a7c72a20028";
    });
  }) // {
    package-description-override = "name:           th-desugar\nversion:        1.17\ncabal-version:  >= 1.10\nsynopsis:       Functions to desugar Template Haskell\nhomepage:       https://github.com/goldfirere/th-desugar\ncategory:       Template Haskell\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\nbug-reports:    https://github.com/goldfirere/th-desugar/issues\nstability:      experimental\nextra-source-files: README.md, CHANGES.md\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ntested-with:    GHC == 8.0.2\n              , GHC == 8.2.2\n              , GHC == 8.4.4\n              , GHC == 8.6.5\n              , GHC == 8.8.4\n              , GHC == 8.10.7\n              , GHC == 9.0.2\n              , GHC == 9.2.8\n              , GHC == 9.4.8\n              , GHC == 9.6.4\n              , GHC == 9.8.2\n              , GHC == 9.10.1\ndescription:\n    This package provides the Language.Haskell.TH.Desugar module, which desugars\n    Template Haskell's rich encoding of Haskell syntax into a simpler encoding.\n    This desugaring discards surface syntax information (such as the use of infix\n    operators) but retains the original meaning of the TH code. The intended use\n    of this package is as a preprocessor for more advanced code manipulation\n    tools. Note that the input to any of the ds... functions should be produced\n    from a TH quote, using the syntax [| ... |]. If the input to these functions\n    is a hand-coded TH syntax tree, the results may be unpredictable. In\n    particular, it is likely that promoted datatypes will not work as expected.\n\nsource-repository this\n  type:     git\n  location: https://github.com/goldfirere/th-desugar.git\n  tag:      v1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/goldfirere/th-desugar.git\n  branch:   master\n\nlibrary\n  build-depends:\n      base >= 4.9 && < 5,\n      ghc-prim,\n      template-haskell >= 2.11 && < 2.23,\n      containers >= 0.5,\n      mtl >= 2.1 && < 2.4,\n      ordered-containers >= 0.2.2,\n      syb >= 0.4,\n      th-abstraction >= 0.6 && < 0.8,\n      th-orphans >= 0.13.7,\n      transformers-compat >= 0.6.3\n  default-extensions: TemplateHaskell\n  exposed-modules:    Language.Haskell.TH.Desugar\n                      Language.Haskell.TH.Desugar.Expand\n                      Language.Haskell.TH.Desugar.Lift\n                      Language.Haskell.TH.Desugar.OMap\n                      Language.Haskell.TH.Desugar.OMap.Strict\n                      Language.Haskell.TH.Desugar.OSet\n                      Language.Haskell.TH.Desugar.Subst\n                      Language.Haskell.TH.Desugar.Sweeten\n  other-modules:      Language.Haskell.TH.Desugar.AST\n                      Language.Haskell.TH.Desugar.Core\n                      Language.Haskell.TH.Desugar.FV\n                      Language.Haskell.TH.Desugar.Match\n                      Language.Haskell.TH.Desugar.Reify\n                      Language.Haskell.TH.Desugar.Util\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\n\ntest-suite spec\n  type:               exitcode-stdio-1.0\n  ghc-options:        -Wall\n  default-language:   Haskell2010\n  default-extensions: TemplateHaskell\n  hs-source-dirs:     Test\n  main-is:            Run.hs\n  other-modules:      Dec\n                      DsDec\n                      FakeSums\n                      FakeTuples\n                      ReifyTypeCUSKs\n                      ReifyTypeSigs\n                      Splices\n                      T158Exp\n                      T159Decs\n                      T183\n\n  build-depends:\n      base >= 4 && < 5,\n      ghc-prim,\n      template-haskell,\n      containers >= 0.5,\n      mtl >= 2.1,\n      syb >= 0.4,\n      HUnit >= 1.2,\n      hspec >= 1.3,\n      th-abstraction,\n      th-desugar,\n      th-orphans >= 0.13.9\n";
  }