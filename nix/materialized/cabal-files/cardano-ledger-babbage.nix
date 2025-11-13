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
      identifier = { name = "cardano-ledger-babbage"; version = "1.12.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger introducing refrence scripts and inline datums";
      description = "This package builds upon the Alonzo ledger with support for reference scripts,\nreference inputs and inline datums.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-ghc" or (errorHandler.buildDepError "microlens-ghc"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
          ];
          buildable = true;
        };
        "gen-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-babbage-1.12.0.0.tar.gz";
      sha256 = "fe1bc267aa72a7a589163dc779990fe67e02720362a4d6482516d37bdc68215b";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-babbage\nversion: 1.12.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis:\n  Cardano ledger introducing refrence scripts and inline datums\n\ndescription:\n  This package builds upon the Alonzo ledger with support for reference scripts,\n  reference inputs and inline datums.\n\ncategory: Network\nbuild-type: Simple\ndata-files:\n  cddl-files/babbage.cddl\n  golden/*.cbor\n  golden/*.json\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/babbage/impl\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Babbage\n    Cardano.Ledger.Babbage.BlockBody\n    Cardano.Ledger.Babbage.Collateral\n    Cardano.Ledger.Babbage.Core\n    Cardano.Ledger.Babbage.PParams\n    Cardano.Ledger.Babbage.Rules\n    Cardano.Ledger.Babbage.Scripts\n    Cardano.Ledger.Babbage.State\n    Cardano.Ledger.Babbage.Transition\n    Cardano.Ledger.Babbage.Tx\n    Cardano.Ledger.Babbage.TxBody\n    Cardano.Ledger.Babbage.TxInfo\n    Cardano.Ledger.Babbage.TxOut\n    Cardano.Ledger.Babbage.TxWits\n    Cardano.Ledger.Babbage.UTxO\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Babbage.Era\n    Cardano.Ledger.Babbage.Rules.Bbody\n    Cardano.Ledger.Babbage.Rules.Deleg\n    Cardano.Ledger.Babbage.Rules.Delegs\n    Cardano.Ledger.Babbage.Rules.Delpl\n    Cardano.Ledger.Babbage.Rules.Ledger\n    Cardano.Ledger.Babbage.Rules.Ledgers\n    Cardano.Ledger.Babbage.Rules.Pool\n    Cardano.Ledger.Babbage.Rules.Ppup\n    Cardano.Ledger.Babbage.Rules.Utxo\n    Cardano.Ledger.Babbage.Rules.Utxos\n    Cardano.Ledger.Babbage.Rules.Utxow\n    Cardano.Ledger.Babbage.State.Account\n    Cardano.Ledger.Babbage.State.CertState\n    Cardano.Ledger.Babbage.State.Stake\n    Cardano.Ledger.Babbage.Translation\n    Cardano.Ledger.Babbage.TxAuxData\n    Cardano.Ledger.Babbage.TxCert\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson >=2.2,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-data >=1.2,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-alonzo ^>=1.14,\n    cardano-ledger-binary >=1.6,\n    cardano-ledger-core:{cardano-ledger-core, internal} >=1.18,\n    cardano-ledger-mary ^>=1.9,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-strict-containers,\n    containers,\n    deepseq,\n    mempack,\n    microlens,\n    nothunks,\n    plutus-ledger-api >=1.33,\n    set-algebra,\n    small-steps >=1.1,\n    text,\n    transformers,\n    validation-selective,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Babbage.Arbitrary\n    Test.Cardano.Ledger.Babbage.Binary.Annotator\n    Test.Cardano.Ledger.Babbage.Binary.Cddl\n    Test.Cardano.Ledger.Babbage.Binary.Twiddle\n    Test.Cardano.Ledger.Babbage.CDDL\n    Test.Cardano.Ledger.Babbage.Era\n    Test.Cardano.Ledger.Babbage.Examples\n    Test.Cardano.Ledger.Babbage.Imp\n    Test.Cardano.Ledger.Babbage.Imp.UtxoSpec\n    Test.Cardano.Ledger.Babbage.Imp.UtxosSpec\n    Test.Cardano.Ledger.Babbage.Imp.UtxowSpec\n    Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid\n    Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid\n    Test.Cardano.Ledger.Babbage.ImpTest\n    Test.Cardano.Ledger.Babbage.Translation.TranslatableGen\n    Test.Cardano.Ledger.Babbage.TreeDiff\n    Test.Cardano.Ledger.Babbage.TxInfoSpec\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_babbage\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-babbage,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib} >=1.13.2,\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib} >=1.17,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    generic-random,\n    heredoc,\n    microlens,\n    microlens-ghc,\n    plutus-ledger-api,\n    small-steps >=1.1,\n    time,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_babbage\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.5,\n    testlib,\n\nexecutable gen-golden\n  main-is: GenerateGoldenFileMain.hs\n  hs-source-dirs: test\n  other-modules: Paths_cardano_ledger_babbage\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base,\n    cardano-ledger-alonzo:testlib,\n    cardano-ledger-babbage,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_babbage\n    Test.Cardano.Ledger.Babbage.Binary.CddlSpec\n    Test.Cardano.Ledger.Babbage.BinarySpec\n    Test.Cardano.Ledger.Babbage.GoldenSpec\n    Test.Cardano.Ledger.Babbage.GoldenTranslation\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    HUnit,\n    base,\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo:testlib,\n    cardano-ledger-babbage,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core,\n    cardano-ledger-core:testlib,\n    cardano-ledger-shelley:testlib,\n    testlib,\n";
  }