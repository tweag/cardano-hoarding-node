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
      identifier = { name = "th-desugar"; version = "1.15"; };
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
      url = "http://hackage.haskell.org/package/th-desugar-1.15.tar.gz";
      sha256 = "179e2cddabb1af642d08e304fa24f4910ad42aae10095961a47d0c916e5ffe98";
    });
  }) // {
    package-description-override = "name:           th-desugar\r\nversion:        1.15\r\nx-revision: 1\r\ncabal-version:  >= 1.10\r\nsynopsis:       Functions to desugar Template Haskell\r\nhomepage:       https://github.com/goldfirere/th-desugar\r\ncategory:       Template Haskell\r\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>\r\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\r\nbug-reports:    https://github.com/goldfirere/th-desugar/issues\r\nstability:      experimental\r\nextra-source-files: README.md, CHANGES.md\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\ntested-with:    GHC == 8.0.2\r\n              , GHC == 8.2.2\r\n              , GHC == 8.4.4\r\n              , GHC == 8.6.5\r\n              , GHC == 8.8.4\r\n              , GHC == 8.10.7\r\n              , GHC == 9.0.2\r\n              , GHC == 9.2.6\r\n              , GHC == 9.4.4\r\n              , GHC == 9.6.1\r\ndescription:\r\n    This package provides the Language.Haskell.TH.Desugar module, which desugars\r\n    Template Haskell's rich encoding of Haskell syntax into a simpler encoding.\r\n    This desugaring discards surface syntax information (such as the use of infix\r\n    operators) but retains the original meaning of the TH code. The intended use\r\n    of this package is as a preprocessor for more advanced code manipulation\r\n    tools. Note that the input to any of the ds... functions should be produced\r\n    from a TH quote, using the syntax [| ... |]. If the input to these functions\r\n    is a hand-coded TH syntax tree, the results may be unpredictable. In\r\n    particular, it is likely that promoted datatypes will not work as expected.\r\n\r\nsource-repository this\r\n  type:     git\r\n  location: https://github.com/goldfirere/th-desugar.git\r\n  tag:      v1.10\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/goldfirere/th-desugar.git\r\n  branch:   master\r\n\r\nlibrary\r\n  build-depends:\r\n      base >= 4.9 && < 5,\r\n      ghc-prim,\r\n      template-haskell >= 2.11 && < 2.21,\r\n      containers >= 0.5,\r\n      mtl >= 2.1 && < 2.4,\r\n      ordered-containers >= 0.2.2,\r\n      syb >= 0.4,\r\n      th-abstraction >= 0.5 && < 0.7,\r\n      th-orphans >= 0.13.7,\r\n      transformers-compat >= 0.6.3\r\n  default-extensions: TemplateHaskell\r\n  exposed-modules:    Language.Haskell.TH.Desugar\r\n                      Language.Haskell.TH.Desugar.Expand\r\n                      Language.Haskell.TH.Desugar.Lift\r\n                      Language.Haskell.TH.Desugar.OMap\r\n                      Language.Haskell.TH.Desugar.OMap.Strict\r\n                      Language.Haskell.TH.Desugar.OSet\r\n                      Language.Haskell.TH.Desugar.Subst\r\n                      Language.Haskell.TH.Desugar.Sweeten\r\n  other-modules:      Language.Haskell.TH.Desugar.AST\r\n                      Language.Haskell.TH.Desugar.Core\r\n                      Language.Haskell.TH.Desugar.FV\r\n                      Language.Haskell.TH.Desugar.Match\r\n                      Language.Haskell.TH.Desugar.Reify\r\n                      Language.Haskell.TH.Desugar.Util\r\n  default-language:   Haskell2010\r\n  ghc-options:        -Wall\r\n\r\n\r\ntest-suite spec\r\n  type:               exitcode-stdio-1.0\r\n  ghc-options:        -Wall\r\n  default-language:   Haskell2010\r\n  default-extensions: TemplateHaskell\r\n  hs-source-dirs:     Test\r\n  main-is:            Run.hs\r\n  other-modules:      Dec\r\n                      DsDec\r\n                      ReifyTypeCUSKs\r\n                      ReifyTypeSigs\r\n                      Splices\r\n                      T158Exp\r\n                      T159Decs\r\n\r\n  build-depends:\r\n      base >= 4 && < 5,\r\n      template-haskell,\r\n      containers >= 0.5,\r\n      mtl >= 2.1,\r\n      syb >= 0.4,\r\n      HUnit >= 1.2,\r\n      hspec >= 1.3,\r\n      th-abstraction,\r\n      th-desugar,\r\n      th-orphans >= 0.13.9\r\n";
  }