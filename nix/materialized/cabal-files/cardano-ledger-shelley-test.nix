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
        name = "cardano-ledger-shelley-test";
        version = "1.7.0.0";
      };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-ledger-shelley exposed to other packages";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-protocol-tpraos".components.sublibs.testlib or (errorHandler.buildDepError "cardano-protocol-tpraos:testlib"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
        ];
        buildable = true;
      };
      tests = {
        "cardano-ledger-shelley-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-protocol-tpraos".components.sublibs.testlib or (errorHandler.buildDepError "cardano-protocol-tpraos:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
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
      benchmarks = {
        "mainbench" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-shelley-test-1.7.0.0.tar.gz";
      sha256 = "a028fdcd0dca6495ba4e194b214dd47385baa9340f921b3f2527586cb753b12f";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-shelley-test\nversion: 1.7.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nsynopsis:\n  Test helpers from cardano-ledger-shelley exposed to other packages\n\nbuild-type: Simple\ndata-files: test/Golden/ShelleyGenesis\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger.git\n  subdir: eras/shelley/test-suite\n\nlibrary\n  exposed-modules:\n    Test.Cardano.Ledger.Shelley.Address.Bootstrap\n    Test.Cardano.Ledger.Shelley.BenchmarkFunctions\n    Test.Cardano.Ledger.Shelley.ByronTranslation\n    Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes\n    Test.Cardano.Ledger.Shelley.Examples.Cast\n    Test.Cardano.Ledger.Shelley.Examples.Federation\n    Test.Cardano.Ledger.Shelley.Generator.Block\n    Test.Cardano.Ledger.Shelley.Generator.Constants\n    Test.Cardano.Ledger.Shelley.Generator.Core\n    Test.Cardano.Ledger.Shelley.Generator.Delegation\n    Test.Cardano.Ledger.Shelley.Generator.EraGen\n    Test.Cardano.Ledger.Shelley.Generator.Presets\n    Test.Cardano.Ledger.Shelley.Generator.ScriptClass\n    Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen\n    Test.Cardano.Ledger.Shelley.Generator.Trace.Chain\n    Test.Cardano.Ledger.Shelley.Generator.Trace.DCert\n    Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger\n    Test.Cardano.Ledger.Shelley.Generator.Trace.TxCert\n    Test.Cardano.Ledger.Shelley.Generator.TxAuxData\n    Test.Cardano.Ledger.Shelley.Generator.TxCert\n    Test.Cardano.Ledger.Shelley.Generator.Update\n    Test.Cardano.Ledger.Shelley.Generator.Utxo\n    Test.Cardano.Ledger.Shelley.PropertyTests\n    Test.Cardano.Ledger.Shelley.Rewards\n    Test.Cardano.Ledger.Shelley.Rules.AdaPreservation\n    Test.Cardano.Ledger.Shelley.Rules.Chain\n    Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces\n    Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness\n    Test.Cardano.Ledger.Shelley.Rules.Deleg\n    Test.Cardano.Ledger.Shelley.Rules.Deposits\n    Test.Cardano.Ledger.Shelley.Rules.IncrementalStake\n    Test.Cardano.Ledger.Shelley.Rules.Pool\n    Test.Cardano.Ledger.Shelley.Rules.PoolReap\n    Test.Cardano.Ledger.Shelley.Rules.TestChain\n    Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators\n    Test.Cardano.Ledger.Shelley.Serialisation.Generators\n    Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils\n    Test.Cardano.Ledger.Shelley.ShelleyTranslation\n    Test.Cardano.Ledger.Shelley.Utils\n    Test.Cardano.Ledger.Shelley.WitVKeys\n    Test.Cardano.Ledger.TerseTools\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck >=2.13.2,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-data >=1.2,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib} ^>=1.7,\n    cardano-ledger-byron,\n    cardano-ledger-core:{cardano-ledger-core, testlib} >=1.18,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib} >=1.17,\n    cardano-protocol-tpraos:{cardano-protocol-tpraos, testlib} >=1.4,\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    cborg,\n    containers,\n    data-default,\n    deepseq,\n    microlens,\n    mtl,\n    nothunks,\n    plutus-ledger-api,\n    prettyprinter,\n    serialise,\n    set-algebra,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    text,\n    transformers,\n    tree-diff,\n    vector,\n    vector-map >=1.1,\n\ntest-suite cardano-ledger-shelley-test\n  type: exitcode-stdio-1.0\n  main-is: Tests.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_shelley_test\n    Test.Cardano.Ledger.Shelley.Examples.Chain\n    Test.Cardano.Ledger.Shelley.Examples.Combinators\n    Test.Cardano.Ledger.Shelley.Examples.EmptyBlock\n    Test.Cardano.Ledger.Shelley.Examples.GenesisDelegation\n    Test.Cardano.Ledger.Shelley.Examples.Init\n    Test.Cardano.Ledger.Shelley.Examples.Mir\n    Test.Cardano.Ledger.Shelley.Examples.MirTransfer\n    Test.Cardano.Ledger.Shelley.Examples.NetworkID\n    Test.Cardano.Ledger.Shelley.Examples.PoolLifetime\n    Test.Cardano.Ledger.Shelley.Examples.PoolReReg\n    Test.Cardano.Ledger.Shelley.Examples.TwoPools\n    Test.Cardano.Ledger.Shelley.Examples.Updates\n    Test.Cardano.Ledger.Shelley.Fees\n    Test.Cardano.Ledger.Shelley.MultiSigExamples\n    Test.Cardano.Ledger.Shelley.RulesTests\n    Test.Cardano.Ledger.Shelley.SafeHash\n    Test.Cardano.Ledger.Shelley.Serialisation\n    Test.Cardano.Ledger.Shelley.Serialisation.Golden.Address\n    Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding\n    Test.Cardano.Ledger.Shelley.Serialisation.Golden.Genesis\n    Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR\n    Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON\n    Test.Cardano.Ledger.Shelley.UnitTests\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    \"-with-rtsopts=-K4m -M300m\"\n\n  build-depends:\n    QuickCheck,\n    aeson >=2,\n    base,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-byron,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos:{cardano-protocol-tpraos, testlib},\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    data-default,\n    deepseq,\n    groups,\n    iproute,\n    microlens,\n    prettyprinter,\n    scientific,\n    small-steps:{small-steps, testlib} >=1.1,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    time,\n\nbenchmark mainbench\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: bench\n  other-modules:\n    BenchUTxOAggregate\n    BenchValidation\n    Cardano.Ledger.Shelley.Bench.Gen\n    Cardano.Ledger.Shelley.Bench.Rewards\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -O2\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-data,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    criterion,\n    deepseq,\n    mtl,\n    set-algebra,\n    small-steps:{small-steps, testlib} >=1.1,\n";
  }