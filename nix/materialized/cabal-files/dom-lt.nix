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
      identifier = { name = "dom-lt"; version = "0.2.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) Matt Morrow, 2009";
      maintainer = "Andreas Klebinger <klebinger.andreas@gmx.at>";
      author = "Matt Morrow";
      homepage = "";
      url = "";
      synopsis = "The Lengauer-Tarjan graph dominators algorithm.";
      description = "The Lengauer-Tarjan graph dominators algorithm.\nIncluded are ways to compute domination and post-domination relationships.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
        ];
        buildable = true;
      };
      tests = {
        "dom-lt-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "dom-lt-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dom-lt-0.2.4.tar.gz";
      sha256 = "216ac48590d64058e5da5b83346af2595e2060987230c2b8dae578b2c19c2e2e";
    });
  }) // {
    package-description-override = "name:               dom-lt\nversion:            0.2.4\ncabal-version:      >= 1.10\nbuild-type:         Simple\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Algorithms, Graphs\nauthor:             Matt Morrow\ncopyright:          (c) Matt Morrow, 2009\nmaintainer:         Andreas Klebinger <klebinger.andreas@gmx.at>\nbug-reports:        https://github.com/AndreasPK/dom-lt/issues\nstability:          stable\nsynopsis:           The Lengauer-Tarjan graph dominators algorithm.\ndescription:\n    The Lengauer-Tarjan graph dominators algorithm.\n\n    Included are ways to compute domination and post-domination relationships.\n\ntested-with:\n  --\n  GHC == 9.12.2 || == 9.10.1 || == 9.8.4 || == 9.6.7 || == 9.4.8 || == 8.10.3 || == 8.0.2\n\nExtra-Source-Files:\n  Changelog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/AndreasPK/dom-lt\n\nlibrary\n  Default-Language: Haskell2010\n  includes:\n  build-tools:\n  extra-libraries:\n  hs-source-dirs:   .\n\n  ghc-options:\n      -O2 -funbox-strict-fields\n\n  if impl(ghc >= 9.4)\n    -- Improves perf by 5-9% on my box\n    ghc-options: -fworker-wrapper-cbv\n\n  default-extensions: RankNTypes\n  build-depends:\n      -- base version for ghc-7.10.3\n      base >= 4.8.2 && < 5\n    , array\n    , containers >= 0.5 && < 0.9\n  exposed-modules:\n    Data.Graph.Dom,\n    Data.Graph.Dom.Internal\n\ntest-suite dom-lt-tests\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n\n  Main-Is:  Main.hs\n  hs-source-dirs: tests\n\n  build-depends:\n      base                        >=4.8.2   && <5\n    , dom-lt\n    , containers\n    , HUnit                       >=1.3   && <1.7\n\n  default-extensions:\n  Ghc-Options: -Wall\n\nbenchmark dom-lt-bench\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n\n  Main-Is:  Main.hs\n  hs-source-dirs: benchmarks\n\n  Build-Depends:\n      base,\n      dom-lt,\n      containers,\n      -- Technically older still works but we want the µ bugfix\n      criterion >= 1.5.3.0,\n      deepseq\n  default-extensions:\n\n  Ghc-Options: -O2 -fno-full-laziness\n";
  }