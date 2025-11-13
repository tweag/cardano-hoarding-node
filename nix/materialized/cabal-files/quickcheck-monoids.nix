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
      specVersion = "3.0";
      identifier = { name = "quickcheck-monoids"; version = "0.1.0.3"; };
      license = "Apache-2.0";
      copyright = "2024 Input Output Global Inc (IOG)";
      maintainer = "coot@coot.me";
      author = "Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "QuickCheck monoids";
      description = "All and Any monoids for `Testable` instances based on `.&&.` and `.||.`.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
        ];
        buildable = true;
      };
      tests = {
        "quickcheck-monoids-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."quickcheck-monoids" or (errorHandler.buildDepError "quickcheck-monoids"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/quickcheck-monoids-0.1.0.3.tar.gz";
      sha256 = "7e2111b7a660dde55c6ea152ed97a973a9655db55e878d02cfdf1863d32ab76a";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: quickcheck-monoids\nversion: 0.1.0.3\nsynopsis: QuickCheck monoids\ndescription: All and Any monoids for `Testable` instances based on `.&&.` and `.||.`.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: Marcin Szamotulski\nmaintainer: coot@coot.me\ncategory: Testing\ncopyright: 2024 Input Output Global Inc (IOG)\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\nextra-source-files: README.md\n\ncommon warnings\n  ghc-options: -Wall\n\nlibrary\n  import: warnings\n  exposed-modules: Test.QuickCheck.Monoids\n  build-depends:\n    QuickCheck,\n    base <4.22,\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\ntest-suite quickcheck-monoids-test\n  import: warnings\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  build-depends:\n    QuickCheck,\n    base,\n    quickcheck-monoids,\n    tasty,\n    tasty-quickcheck,\n\n  ghc-options:\n    -Wall\n    -rtsopts\n";
  }