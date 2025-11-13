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
      identifier = { name = "measures"; version = "0.1.0.2"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "An abstraction for (tuples of) measured quantities";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/measures-0.1.0.2.tar.gz";
      sha256 = "329fbe75c3367885ec317bc09b1f4cdecd64d58a33a31c11b60a9f33c2617e87";
    });
  }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                measures\nversion:             0.1.0.2\nsynopsis:            An abstraction for (tuples of) measured quantities\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n\n  exposed-modules:\n                        Data.Measure\n                        Data.Measure.Class\n\n  build-depends:        base\n                      , base-deriving-via\n\ntest-suite test\n  hs-source-dirs:       test\n  main-is:              Main.hs\n  type:                 exitcode-stdio-1.0\n\n  other-modules:\n                        Test.Data.Measure\n\n  build-depends:        base\n                      , QuickCheck\n                      , tasty\n                      , tasty-quickcheck\n\n                      , measures\n";
  }