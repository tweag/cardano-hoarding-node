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
      identifier = { name = "byron-spec-ledger"; version = "1.1.0.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-legder";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
        ];
        buildable = true;
      };
      tests = {
        "byron-spec-ledger-test" = {
          depends = [
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/byron-spec-ledger-1.1.0.1.tar.gz";
      sha256 = "0bb9787ebef1b550f7c6e8289058a189a95e0327437ac62922e5b21c1e08e639";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: byron-spec-ledger\nversion: 1.1.0.1\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/input-output-hk/cardano-legder\nsynopsis: Executable specification of Cardano ledger\ncategory: Testing\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nlibrary\n  exposed-modules:\n    Byron.Spec.Ledger.Core\n    Byron.Spec.Ledger.Core.Generators\n    Byron.Spec.Ledger.Core.Omniscient\n    Byron.Spec.Ledger.Delegation\n    Byron.Spec.Ledger.Delegation.Test\n    Byron.Spec.Ledger.GlobalParams\n    Byron.Spec.Ledger.STS.UTXO\n    Byron.Spec.Ledger.STS.UTXOW\n    Byron.Spec.Ledger.STS.UTXOWS\n    Byron.Spec.Ledger.UTxO\n    Byron.Spec.Ledger.UTxO.Generators\n    Byron.Spec.Ledger.Update\n    Byron.Spec.Ledger.Update.Generators\n    Byron.Spec.Ledger.Update.Test\n    Data.AbstractSize\n    Hedgehog.Gen.Double\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    Unique >=0.4.7.6,\n    base >=4.18 && <5,\n    bimap >=0.4 && <0.6,\n    bytestring,\n    cardano-crypto-class,\n    cardano-ledger-binary >=1.0,\n    containers,\n    crypton,\n    hashable,\n    hedgehog >=1.0.4,\n    microlens,\n    microlens-th,\n    nothunks,\n    small-steps:{small-steps, testlib} >=1.1,\n\ntest-suite byron-spec-ledger-test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Byron.Spec.Ledger.AbstractSize.Properties\n    Test.Byron.Spec.Ledger.Core.Generators.Properties\n    Test.Byron.Spec.Ledger.Delegation.Examples\n    Test.Byron.Spec.Ledger.Delegation.Properties\n    Test.Byron.Spec.Ledger.Relation.Properties\n    Test.Byron.Spec.Ledger.UTxO.Properties\n    Test.Byron.Spec.Ledger.Update.Examples\n    Test.Byron.Spec.Ledger.Update.Properties\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -rtsopts\n    \"-with-rtsopts=-K4m -M150m\"\n\n  build-depends:\n    Unique,\n    base,\n    bimap,\n    byron-spec-ledger,\n    containers,\n    hedgehog,\n    microlens,\n    microlens-th,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hedgehog,\n    tasty-hunit,\n";
  }