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
      identifier = {
        name = "cardano-ledger-shelley-ma-test";
        version = "1.4.0.0";
      };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger with multiasset and time lock support.";
      description = "This package extends the Shelley ledger with support for\nnative tokens and timelocks.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "cardano-ledger-shelley-ma-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-shelley-ma-test-1.4.0.0.tar.gz";
      sha256 = "c8d1d6911f62fac8d3109e3d516d5d978c2b78803622fe6dc54c97e84c0fdabf";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-shelley-ma-test\nversion: 1.4.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Shelley ledger with multiasset and time lock support.\ndescription:\n  This package extends the Shelley ledger with support for\n  native tokens and timelocks.\n\ncategory: Network\nbuild-type: Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/shelley-ma/test-suite\n\nlibrary\n  exposed-modules:\n    Test.Cardano.Ledger.AllegraEraGen\n    Test.Cardano.Ledger.EraBuffet\n    Test.Cardano.Ledger.Mary.Golden\n    Test.Cardano.Ledger.MaryEraGen\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip\n    Test.Cardano.Ledger.ShelleyMA.TxBody\n    Test.Cardano.Ledger.TranslationTools\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-ledger-allegra:{cardano-ledger-allegra, testlib} ^>=1.8,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib} ^>=1.7,\n    cardano-ledger-core >=1.17,\n    cardano-ledger-mary:{cardano-ledger-mary, testlib} ^>=1.9,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib} >=1.12,\n    cardano-ledger-shelley-test >=1.6,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    hashable,\n    microlens,\n    mtl,\n    small-steps >=1.1,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    text,\n\ntest-suite cardano-ledger-shelley-ma-test\n  type: exitcode-stdio-1.0\n  main-is: Tests.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.Allegra.ScriptTranslation\n    Test.Cardano.Ledger.Allegra.Translation\n    Test.Cardano.Ledger.Mary.Examples\n    Test.Cardano.Ledger.Mary.Examples.Cast\n    Test.Cardano.Ledger.Mary.Examples.MultiAssets\n    Test.Cardano.Ledger.Mary.Translation\n    Test.Cardano.Ledger.Mary.Value\n    Test.Cardano.Ledger.ShelleyMA.Serialisation\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    \"-with-rtsopts=-K4m -M650m\"\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-allegra,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos >=1.4,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    data-default,\n    deepseq,\n    groups,\n    microlens,\n    mtl,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n";
  }