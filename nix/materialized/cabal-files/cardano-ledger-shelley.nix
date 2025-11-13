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
    flags = { asserts = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-ledger-shelley"; version = "1.17.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Shelley Ledger Executable Model";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."ImpSpec" or (errorHandler.buildDepError "ImpSpec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-shelley-1.17.0.0.tar.gz";
      sha256 = "5e8f7e17f13eb59670eff3583e7d68321a98faa3bda5803ecd2b270d62d19599";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-shelley\nversion: 1.17.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nsynopsis: Shelley Ledger Executable Model\nbuild-type: Simple\ndata-files:\n  cddl-files/shelley.cddl\n  golden/*.json\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger.git\n  subdir: eras/shelley/impl\n\nflag asserts\n  description: Enable assertions\n  default: False\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Chain\n    Cardano.Ledger.Shelley\n    Cardano.Ledger.Shelley.API\n    Cardano.Ledger.Shelley.API.ByronTranslation\n    Cardano.Ledger.Shelley.API.Mempool\n    Cardano.Ledger.Shelley.API.Types\n    Cardano.Ledger.Shelley.API.Validation\n    Cardano.Ledger.Shelley.API.Wallet\n    Cardano.Ledger.Shelley.AdaPots\n    Cardano.Ledger.Shelley.BlockBody\n    Cardano.Ledger.Shelley.BlockBody.Internal\n    Cardano.Ledger.Shelley.BlockChain\n    Cardano.Ledger.Shelley.Core\n    Cardano.Ledger.Shelley.Genesis\n    Cardano.Ledger.Shelley.Governance\n    Cardano.Ledger.Shelley.Internal\n    Cardano.Ledger.Shelley.LedgerState\n    Cardano.Ledger.Shelley.PParams\n    Cardano.Ledger.Shelley.PoolRank\n    Cardano.Ledger.Shelley.RewardProvenance\n    Cardano.Ledger.Shelley.RewardUpdate\n    Cardano.Ledger.Shelley.Rewards\n    Cardano.Ledger.Shelley.Rules\n    Cardano.Ledger.Shelley.Rules.Reports\n    Cardano.Ledger.Shelley.Scripts\n    Cardano.Ledger.Shelley.SoftForks\n    Cardano.Ledger.Shelley.StabilityWindow\n    Cardano.Ledger.Shelley.State\n    Cardano.Ledger.Shelley.Transition\n    Cardano.Ledger.Shelley.Translation\n    Cardano.Ledger.Shelley.Tx\n    Cardano.Ledger.Shelley.TxAuxData\n    Cardano.Ledger.Shelley.TxBody\n    Cardano.Ledger.Shelley.TxCert\n    Cardano.Ledger.Shelley.TxOut\n    Cardano.Ledger.Shelley.TxWits\n    Cardano.Ledger.Shelley.UTxO\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Shelley.Era\n    Cardano.Ledger.Shelley.LedgerState.IncrementalStake\n    Cardano.Ledger.Shelley.LedgerState.NewEpochState\n    Cardano.Ledger.Shelley.LedgerState.PulsingReward\n    Cardano.Ledger.Shelley.LedgerState.Types\n    Cardano.Ledger.Shelley.Rules.Bbody\n    Cardano.Ledger.Shelley.Rules.Deleg\n    Cardano.Ledger.Shelley.Rules.Delegs\n    Cardano.Ledger.Shelley.Rules.Delpl\n    Cardano.Ledger.Shelley.Rules.Epoch\n    Cardano.Ledger.Shelley.Rules.Ledger\n    Cardano.Ledger.Shelley.Rules.Ledgers\n    Cardano.Ledger.Shelley.Rules.Mir\n    Cardano.Ledger.Shelley.Rules.NewEpoch\n    Cardano.Ledger.Shelley.Rules.Newpp\n    Cardano.Ledger.Shelley.Rules.Pool\n    Cardano.Ledger.Shelley.Rules.PoolReap\n    Cardano.Ledger.Shelley.Rules.Ppup\n    Cardano.Ledger.Shelley.Rules.Rupd\n    Cardano.Ledger.Shelley.Rules.Snap\n    Cardano.Ledger.Shelley.Rules.Tick\n    Cardano.Ledger.Shelley.Rules.Upec\n    Cardano.Ledger.Shelley.Rules.Utxo\n    Cardano.Ledger.Shelley.Rules.Utxow\n    Cardano.Ledger.Shelley.State.Account\n    Cardano.Ledger.Shelley.State.CertState\n    Cardano.Ledger.Shelley.State.Stake\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    aeson >=2,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-data ^>=1.2.2,\n    cardano-ledger-binary ^>=1.7,\n    cardano-ledger-byron,\n    cardano-ledger-core:{cardano-ledger-core, internal} ^>=1.18,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    deepseq,\n    groups,\n    mempack,\n    microlens,\n    mtl,\n    nothunks,\n    quiet,\n    set-algebra >=1.0,\n    small-steps >=1.1.1,\n    text,\n    time,\n    transformers,\n    validation-selective,\n    vector-map ^>=1.1,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Shelley.Arbitrary\n    Test.Cardano.Ledger.Shelley.Binary.Annotator\n    Test.Cardano.Ledger.Shelley.Binary.Cddl\n    Test.Cardano.Ledger.Shelley.Binary.Golden\n    Test.Cardano.Ledger.Shelley.Binary.RoundTrip\n    Test.Cardano.Ledger.Shelley.CDDL\n    Test.Cardano.Ledger.Shelley.Constants\n    Test.Cardano.Ledger.Shelley.Era\n    Test.Cardano.Ledger.Shelley.Examples\n    Test.Cardano.Ledger.Shelley.Imp\n    Test.Cardano.Ledger.Shelley.Imp.EpochSpec\n    Test.Cardano.Ledger.Shelley.Imp.LedgerSpec\n    Test.Cardano.Ledger.Shelley.Imp.PoolSpec\n    Test.Cardano.Ledger.Shelley.Imp.UtxoSpec\n    Test.Cardano.Ledger.Shelley.Imp.UtxowSpec\n    Test.Cardano.Ledger.Shelley.ImpTest\n    Test.Cardano.Ledger.Shelley.JSON\n    Test.Cardano.Ledger.Shelley.TreeDiff\n    Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_shelley\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    ImpSpec,\n    base,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-data,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-byron:{cardano-ledger-byron, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley,\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    data-default,\n    generic-random,\n    hedgehog-quickcheck,\n    heredoc,\n    microlens,\n    microlens-mtl,\n    mtl,\n    prettyprinter,\n    prettyprinter-ansi-terminal,\n    random,\n    small-steps >=1.1,\n    text,\n    time,\n    transformers,\n    tree-diff,\n    unliftio,\n    vector-map,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_shelley\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.4,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_shelley\n    Test.Cardano.Ledger.Shelley.Binary.CddlSpec\n    Test.Cardano.Ledger.Shelley.Binary.GoldenSpec\n    Test.Cardano.Ledger.Shelley.Binary.RoundTripSpec\n    Test.Cardano.Ledger.Shelley.BinarySpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley,\n    testlib,\n";
  }