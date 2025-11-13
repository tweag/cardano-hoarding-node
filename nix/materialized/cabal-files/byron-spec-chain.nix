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
      identifier = { name = "byron-spec-chain"; version = "1.0.1.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-legder";
      url = "";
      synopsis = "Executable specification of the Cardano blockchain";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
        ];
        buildable = true;
      };
      tests = {
        "chain-rules-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
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
      url = "https://chap.intersectmbo.org/package/byron-spec-chain-1.0.1.1.tar.gz";
      sha256 = "82aab157a2343f7b7b77d23078fbbda23f1a987296b19c07178ec28c6b123964";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: byron-spec-chain\nversion: 1.0.1.1\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/input-output-hk/cardano-legder\nsynopsis: Executable specification of the Cardano blockchain\ncategory: Testing\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nlibrary\n  exposed-modules:\n    Byron.Spec.Chain.STS.Block\n    Byron.Spec.Chain.STS.Rule.BBody\n    Byron.Spec.Chain.STS.Rule.Bupi\n    Byron.Spec.Chain.STS.Rule.Chain\n    Byron.Spec.Chain.STS.Rule.Epoch\n    Byron.Spec.Chain.STS.Rule.Pbft\n    Byron.Spec.Chain.STS.Rule.SigCnt\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base >=4.18 && <5,\n    bimap >=0.4 && <0.6,\n    byron-spec-ledger >=1.0,\n    bytestring,\n    containers,\n    hashable,\n    hedgehog >=1.0.4,\n    microlens,\n    microlens-th,\n    small-steps:{small-steps, testlib} >=1.1,\n\ntest-suite chain-rules-test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Byron.AbstractSize.Properties\n    Test.Byron.Spec.Chain.STS.Properties\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -rtsopts\n    \"-with-rtsopts=-K4m -M300m\"\n\n  build-depends:\n    base,\n    byron-spec-chain,\n    byron-spec-ledger >=1.0,\n    containers,\n    data-ordlist,\n    hedgehog,\n    microlens,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hedgehog,\n    tasty-hunit,\n";
  }