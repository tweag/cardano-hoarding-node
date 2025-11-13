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
      identifier = { name = "cardano-ledger-alonzo"; version = "1.14.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger introducing Plutus Core";
      description = "This package builds upon the Mary ledger with support for extended UTxO\nvia Plutus Core.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
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
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-data".components.sublibs.testlib or (errorHandler.buildDepError "cardano-data:testlib"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
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
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
          ];
          buildable = true;
        };
        "gen-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-alonzo-1.14.0.0.tar.gz";
      sha256 = "a35385934dbf6f37ddfdc059a9c1bc236e66f7d024a4f200e25f39e80ed7476d";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-alonzo\nversion: 1.14.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Cardano ledger introducing Plutus Core\ndescription:\n  This package builds upon the Mary ledger with support for extended UTxO\n  via Plutus Core.\n\ncategory: Network\nbuild-type: Simple\ndata-files:\n  cddl-files/alonzo.cddl\n  golden/*.cbor\n  golden/*.json\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/alonzo/impl\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Alonzo\n    Cardano.Ledger.Alonzo.BlockBody\n    Cardano.Ledger.Alonzo.BlockBody.Internal\n    Cardano.Ledger.Alonzo.Core\n    Cardano.Ledger.Alonzo.Genesis\n    Cardano.Ledger.Alonzo.PParams\n    Cardano.Ledger.Alonzo.Plutus.Context\n    Cardano.Ledger.Alonzo.Plutus.Evaluate\n    Cardano.Ledger.Alonzo.Plutus.TxInfo\n    Cardano.Ledger.Alonzo.Rules\n    Cardano.Ledger.Alonzo.Scripts\n    Cardano.Ledger.Alonzo.State\n    Cardano.Ledger.Alonzo.Transition\n    Cardano.Ledger.Alonzo.Translation\n    Cardano.Ledger.Alonzo.Tx\n    Cardano.Ledger.Alonzo.TxAuxData\n    Cardano.Ledger.Alonzo.TxBody\n    Cardano.Ledger.Alonzo.TxOut\n    Cardano.Ledger.Alonzo.TxSeq\n    Cardano.Ledger.Alonzo.TxWits\n    Cardano.Ledger.Alonzo.UTxO\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Alonzo.Era\n    Cardano.Ledger.Alonzo.Rules.Bbody\n    Cardano.Ledger.Alonzo.Rules.Deleg\n    Cardano.Ledger.Alonzo.Rules.Delegs\n    Cardano.Ledger.Alonzo.Rules.Delpl\n    Cardano.Ledger.Alonzo.Rules.Ledger\n    Cardano.Ledger.Alonzo.Rules.Ledgers\n    Cardano.Ledger.Alonzo.Rules.Pool\n    Cardano.Ledger.Alonzo.Rules.Ppup\n    Cardano.Ledger.Alonzo.Rules.Utxo\n    Cardano.Ledger.Alonzo.Rules.Utxos\n    Cardano.Ledger.Alonzo.Rules.Utxow\n    Cardano.Ledger.Alonzo.State.Account\n    Cardano.Ledger.Alonzo.State.CertState\n    Cardano.Ledger.Alonzo.State.Stake\n    Cardano.Ledger.Alonzo.TxCert\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    aeson >=2.2,\n    base >=4.18 && <5,\n    base64-bytestring,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data ^>=1.2.1,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-binary ^>=1.7,\n    cardano-ledger-core:{cardano-ledger-core, internal} ^>=1.18,\n    cardano-ledger-mary ^>=1.9,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    deepseq,\n    mempack,\n    microlens,\n    nothunks,\n    plutus-ledger-api >=1.37,\n    set-algebra >=1.0,\n    small-steps >=1.1,\n    text,\n    transformers,\n    validation-selective,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Alonzo.Arbitrary\n    Test.Cardano.Ledger.Alonzo.Binary.Annotator\n    Test.Cardano.Ledger.Alonzo.Binary.Cddl\n    Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec\n    Test.Cardano.Ledger.Alonzo.Binary.RoundTrip\n    Test.Cardano.Ledger.Alonzo.Binary.Twiddle\n    Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec\n    Test.Cardano.Ledger.Alonzo.CDDL\n    Test.Cardano.Ledger.Alonzo.Era\n    Test.Cardano.Ledger.Alonzo.Examples\n    Test.Cardano.Ledger.Alonzo.Imp\n    Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec\n    Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec\n    Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec\n    Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid\n    Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid\n    Test.Cardano.Ledger.Alonzo.ImpTest\n    Test.Cardano.Ledger.Alonzo.Translation.Golden\n    Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen\n    Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance\n    Test.Cardano.Ledger.Alonzo.TreeDiff\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_alonzo\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    HUnit,\n    base,\n    bytestring,\n    cardano-data:{cardano-data, testlib},\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    cuddle >=0.4,\n    data-default,\n    generic-random,\n    heredoc,\n    microlens,\n    microlens-mtl,\n    mtl,\n    plutus-ledger-api,\n    serialise,\n    text,\n    time,\n    tree-diff,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_alonzo\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.5,\n    testlib,\n\nexecutable gen-golden\n  main-is: GenerateGoldenFileMain.hs\n  hs-source-dirs: test\n  other-modules: Paths_cardano_ledger_alonzo\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base,\n    cardano-ledger-alonzo,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_alonzo\n    Test.Cardano.Ledger.Alonzo.Binary.CanonicalSpec\n    Test.Cardano.Ledger.Alonzo.Binary.CddlSpec\n    Test.Cardano.Ledger.Alonzo.BinarySpec\n    Test.Cardano.Ledger.Alonzo.GoldenSpec\n    Test.Cardano.Ledger.Alonzo.GoldenTranslation\n    Test.Cardano.Ledger.Alonzo.TxInfoSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    HUnit,\n    QuickCheck,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:testlib,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    testlib,\n    time,\n";
  }