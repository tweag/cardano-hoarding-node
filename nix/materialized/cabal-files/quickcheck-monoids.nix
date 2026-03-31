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
      identifier = { name = "quickcheck-monoids"; version = "0.1.1.0"; };
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
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/quickcheck-monoids-0.1.1.0.tar.gz";
      sha256 = "202e16a16dae830adabdd9a5d9a08a416a74036745ea79f9bce0961548d042e3";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: quickcheck-monoids\nversion: 0.1.1.0\nsynopsis: QuickCheck monoids\ndescription: All and Any monoids for `Testable` instances based on `.&&.` and `.||.`.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: Marcin Szamotulski\nmaintainer: coot@coot.me\ncategory: Testing\ncopyright: 2024 Input Output Global Inc (IOG)\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\ncommon warnings\n  ghc-options: -Wall\n\nlibrary\n  import: warnings\n  exposed-modules: Test.QuickCheck.Monoids\n  build-depends:\n    QuickCheck,\n    base <4.23,\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n";
  }