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
      identifier = { name = "non-integral"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "non-integral-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/non-integral-1.0.0.0.tar.gz";
      sha256 = "6eed92a22abfc241dee1942d42320f6ac98cabe44a3b172eded214b5ef7d0e82";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\nname:               non-integral\nversion:            1.0.0.0\nlicense:            Apache-2.0\nmaintainer:         operations@iohk.io\nauthor:             IOHK\ndescription:        Implementation decision for non-integer calculations\nbuild-type:         Simple\nextra-source-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/input-output-hk/cardano-ledger.git\n    subdir:   libs/non-integral\n\nlibrary\n    exposed-modules:  Cardano.Ledger.NonIntegral\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wredundant-constraints -Wunused-packages\n\n    build-depends:    base >=4.14 && <5\n\ntest-suite non-integral-test\n    type:             exitcode-stdio-1.0\n    main-is:          Tests.hs\n    hs-source-dirs:   test\n    other-modules:    Tests.Cardano.Ledger.NonIntegral\n    default-language: Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wredundant-constraints -Wunused-packages\n        -O2 -threaded -rtsopts -with-rtsopts=-N\n\n    build-depends:\n        base,\n        non-integral,\n        QuickCheck\n";
  }