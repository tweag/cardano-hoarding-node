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
        name = "cardano-ledger-alonzo-test";
        version = "1.4.0.0";
      };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Tests for Cardano ledger introducing Plutus Core";
      description = "This package builds upon the Mary ledger with support for extended UTxO\nvia Plutus Core.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      tests = {
        "cardano-ledger-alonzo-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-protocol-tpraos".components.sublibs.testlib or (errorHandler.buildDepError "cardano-protocol-tpraos:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-alonzo-test-1.4.0.0.tar.gz";
      sha256 = "d71385d298c830c1b86e5e85de685ebad8911af53f01cfb4dbb40d34984717e2";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-alonzo-test\nversion: 1.4.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Tests for Cardano ledger introducing Plutus Core\ndescription:\n  This package builds upon the Mary ledger with support for extended UTxO\n  via Plutus Core.\n\ncategory: Network\nbuild-type: Simple\ndata-files:\n  golden/*.cbor\n  golden/*.json\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/alonzo/test-suite\n\nlibrary\n  exposed-modules:\n    Test.Cardano.Ledger.Alonzo.AlonzoEraGen\n    Test.Cardano.Ledger.Alonzo.EraMapping\n    Test.Cardano.Ledger.Alonzo.Scripts\n    Test.Cardano.Ledger.Alonzo.Trace\n\n  hs-source-dirs: src\n  other-modules: Paths_cardano_ledger_alonzo_test\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base >=4.18 && <5,\n    cardano-ledger-allegra >=1.2,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib} >=1.14,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib} >=1.0,\n    cardano-ledger-core:{cardano-ledger-core, testlib} >=1.18,\n    cardano-ledger-mary >=1.4,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib} >=1.14,\n    cardano-ledger-shelley-ma-test >=1.2,\n    cardano-ledger-shelley-test >=1.6,\n    cardano-protocol-tpraos >=1.0,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    microlens,\n    plutus-ledger-api >=1.33,\n    random,\n    small-steps:{small-steps, testlib} >=1.1,\n    transformers,\n\ntest-suite cardano-ledger-alonzo-test\n  type: exitcode-stdio-1.0\n  main-is: Tests.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_alonzo_test\n    Test.Cardano.Ledger.Alonzo.ChainTrace\n    Test.Cardano.Ledger.Alonzo.Golden\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n    -rtsopts\n\n  build-depends:\n    QuickCheck,\n    aeson >=2,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-alonzo-test,\n    cardano-ledger-binary,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos:{cardano-protocol-tpraos, testlib},\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    microlens,\n    plutus-ledger-api,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    time,\n";
  }