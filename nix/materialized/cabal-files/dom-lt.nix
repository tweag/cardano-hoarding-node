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
      identifier = { name = "dom-lt"; version = "0.2.3"; };
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
      url = "http://hackage.haskell.org/package/dom-lt-0.2.3.tar.gz";
      sha256 = "3d198be111a1a6b6d19356c7737ee486607735b6405b35cde6c105035309e3c0";
    });
  }) // {
    package-description-override = "name:               dom-lt\r\nversion:            0.2.3\r\nx-revision: 1\r\ncabal-version:      >= 1.10\r\nbuild-type:         Simple\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\ncategory:           Algorithms, Graphs\r\nauthor:             Matt Morrow\r\ncopyright:          (c) Matt Morrow, 2009\r\nmaintainer:         Andreas Klebinger <klebinger.andreas@gmx.at>\r\nbug-reports:        https://github.com/AndreasPK/dom-lt/issues\r\nstability:          stable\r\nsynopsis:           The Lengauer-Tarjan graph dominators algorithm.\r\ndescription:\r\n    The Lengauer-Tarjan graph dominators algorithm.\r\n\r\n    Included are ways to compute domination and post-domination relationships.\r\n\r\ntested-with:\r\n  -- Every two major versions between 7 and 9 should be enough\r\n  GHC == 9.0.1 || == 8.10.3 || == 8.8.4 || == 8.4.4 || == 8.0.2 || == 7.8.4 || == 7.4.2\r\n\r\nExtra-Source-Files:\r\n  Changelog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/AndreasPK/dom-lt\r\n\r\nlibrary\r\n  Default-Language: Haskell2010\r\n  includes:\r\n  build-tools:\r\n  extra-libraries:\r\n  hs-source-dirs:   .\r\n  ghc-options:      -O2 -funbox-strict-fields\r\n  default-extensions: RankNTypes\r\n  build-depends:\r\n      base >= 4.3 && < 5\r\n    , array\r\n    , containers >= 0.4.2.0 && < 0.8\r\n  exposed-modules:\r\n    Data.Graph.Dom,\r\n    Data.Graph.Dom.Internal\r\n\r\ntest-suite dom-lt-tests\r\n  Default-Language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n\r\n  Main-Is:  Main.hs\r\n  hs-source-dirs: tests\r\n\r\n  build-depends:\r\n      base                        >=4.3   && <5\r\n    , dom-lt\r\n    , containers\r\n    , HUnit                       >=1.3   && <1.7\r\n\r\n  default-extensions:\r\n  Ghc-Options: -Wall\r\n\r\nbenchmark dom-lt-bench\r\n  Default-Language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n\r\n  Main-Is:  Main.hs\r\n  hs-source-dirs: benchmarks\r\n\r\n  Build-Depends: base, dom-lt, containers, criterion, deepseq\r\n  default-extensions:\r\n\r\n  Ghc-Options: -O2 -fno-full-laziness\r\n\r\n";
  }