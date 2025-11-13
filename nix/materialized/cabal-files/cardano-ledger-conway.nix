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
      identifier = { name = "cardano-ledger-conway"; version = "1.20.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger with an updated on-chain governance system.";
      description = "This package builds upon the Babbage ledger with an updated on-chain governance system.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
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
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
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
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-data".components.sublibs.testlib or (errorHandler.buildDepError "cardano-data:testlib"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
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
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
          ];
          buildable = true;
        };
        "gen-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
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
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-conway-1.20.0.0.tar.gz";
      sha256 = "331706443c53c3992f5685d6ec8a91972fd8af4bd6de6a15c626edbb4dac0f2d";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-conway\nversion: 1.20.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Cardano ledger with an updated on-chain governance system.\ndescription:\n  This package builds upon the Babbage ledger with an updated on-chain governance system.\n\ncategory: Network\nbuild-type: Simple\ndata-files:\n  cddl-files/conway.cddl\n  golden/*.cbor\n  golden/*.json\n  test/data/*.json\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/conway/impl\n\nflag asserts\n  description: Enable assertions\n  default: False\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Conway\n    Cardano.Ledger.Conway.BlockBody\n    Cardano.Ledger.Conway.Core\n    Cardano.Ledger.Conway.Genesis\n    Cardano.Ledger.Conway.Governance\n    Cardano.Ledger.Conway.Governance.DRepPulser\n    Cardano.Ledger.Conway.PParams\n    Cardano.Ledger.Conway.Rules\n    Cardano.Ledger.Conway.Scripts\n    Cardano.Ledger.Conway.State\n    Cardano.Ledger.Conway.Transition\n    Cardano.Ledger.Conway.Tx\n    Cardano.Ledger.Conway.TxBody\n    Cardano.Ledger.Conway.TxCert\n    Cardano.Ledger.Conway.TxInfo\n    Cardano.Ledger.Conway.TxWits\n    Cardano.Ledger.Conway.UTxO\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Conway.Era\n    Cardano.Ledger.Conway.Governance.Internal\n    Cardano.Ledger.Conway.Governance.Procedures\n    Cardano.Ledger.Conway.Governance.Proposals\n    Cardano.Ledger.Conway.Rules.Bbody\n    Cardano.Ledger.Conway.Rules.Cert\n    Cardano.Ledger.Conway.Rules.Certs\n    Cardano.Ledger.Conway.Rules.Deleg\n    Cardano.Ledger.Conway.Rules.Enact\n    Cardano.Ledger.Conway.Rules.Epoch\n    Cardano.Ledger.Conway.Rules.Gov\n    Cardano.Ledger.Conway.Rules.GovCert\n    Cardano.Ledger.Conway.Rules.HardFork\n    Cardano.Ledger.Conway.Rules.Ledger\n    Cardano.Ledger.Conway.Rules.Ledgers\n    Cardano.Ledger.Conway.Rules.Mempool\n    Cardano.Ledger.Conway.Rules.NewEpoch\n    Cardano.Ledger.Conway.Rules.Pool\n    Cardano.Ledger.Conway.Rules.Ratify\n    Cardano.Ledger.Conway.Rules.Tickf\n    Cardano.Ledger.Conway.Rules.Utxo\n    Cardano.Ledger.Conway.Rules.Utxos\n    Cardano.Ledger.Conway.Rules.Utxow\n    Cardano.Ledger.Conway.State.Account\n    Cardano.Ledger.Conway.State.CertState\n    Cardano.Ledger.Conway.State.Stake\n    Cardano.Ledger.Conway.State.VState\n    Cardano.Ledger.Conway.Translation\n    Cardano.Ledger.Conway.TxAuxData\n    Cardano.Ledger.Conway.TxOut\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson >=2.2,\n    base >=4.18 && <5,\n    cardano-crypto-class,\n    cardano-data >=1.2.3,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-alonzo ^>=1.14,\n    cardano-ledger-babbage ^>=1.12,\n    cardano-ledger-binary ^>=1.7,\n    cardano-ledger-core:{cardano-ledger-core, internal} ^>=1.18,\n    cardano-ledger-mary ^>=1.9,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    deepseq,\n    mempack,\n    microlens,\n    mtl,\n    nothunks,\n    plutus-ledger-api >=1.37,\n    small-steps >=1.1.2,\n    text,\n    transformers,\n    validation-selective,\n    vector-map,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Conway.Arbitrary\n    Test.Cardano.Ledger.Conway.Binary.Annotator\n    Test.Cardano.Ledger.Conway.Binary.Cddl\n    Test.Cardano.Ledger.Conway.Binary.Regression\n    Test.Cardano.Ledger.Conway.Binary.RoundTrip\n    Test.Cardano.Ledger.Conway.BinarySpec\n    Test.Cardano.Ledger.Conway.CDDL\n    Test.Cardano.Ledger.Conway.CommitteeRatifySpec\n    Test.Cardano.Ledger.Conway.DRepRatifySpec\n    Test.Cardano.Ledger.Conway.Era\n    Test.Cardano.Ledger.Conway.Examples\n    Test.Cardano.Ledger.Conway.Genesis\n    Test.Cardano.Ledger.Conway.GenesisSpec\n    Test.Cardano.Ledger.Conway.GovActionReorderSpec\n    Test.Cardano.Ledger.Conway.Imp\n    Test.Cardano.Ledger.Conway.Imp.BbodySpec\n    Test.Cardano.Ledger.Conway.Imp.CertsSpec\n    Test.Cardano.Ledger.Conway.Imp.DelegSpec\n    Test.Cardano.Ledger.Conway.Imp.EnactSpec\n    Test.Cardano.Ledger.Conway.Imp.EpochSpec\n    Test.Cardano.Ledger.Conway.Imp.GovCertSpec\n    Test.Cardano.Ledger.Conway.Imp.GovSpec\n    Test.Cardano.Ledger.Conway.Imp.HardForkSpec\n    Test.Cardano.Ledger.Conway.Imp.LedgerSpec\n    Test.Cardano.Ledger.Conway.Imp.RatifySpec\n    Test.Cardano.Ledger.Conway.Imp.UtxoSpec\n    Test.Cardano.Ledger.Conway.Imp.UtxosSpec\n    Test.Cardano.Ledger.Conway.Imp.UtxowSpec\n    Test.Cardano.Ledger.Conway.ImpTest\n    Test.Cardano.Ledger.Conway.Plutus.PlutusSpec\n    Test.Cardano.Ledger.Conway.Proposals\n    Test.Cardano.Ledger.Conway.SPORatifySpec\n    Test.Cardano.Ledger.Conway.Spec\n    Test.Cardano.Ledger.Conway.Translation.TranslatableGen\n    Test.Cardano.Ledger.Conway.TreeDiff\n    Test.Cardano.Ledger.Conway.TxInfoSpec\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_conway\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    ImpSpec,\n    aeson,\n    base,\n    bytestring,\n    cardano-data:{cardano-data, testlib},\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-babbage:{cardano-ledger-babbage, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-conway,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    data-default,\n    deepseq,\n    generic-random,\n    heredoc,\n    microlens,\n    microlens-mtl,\n    mtl,\n    plutus-ledger-api,\n    prettyprinter,\n    small-steps >=1.1,\n    text,\n    time,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_conway\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.5,\n    testlib,\n\nexecutable gen-golden\n  main-is: GenerateGoldenFileMain.hs\n  hs-source-dirs: test\n  other-modules: Paths_cardano_ledger_conway\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base,\n    cardano-ledger-alonzo:testlib,\n    cardano-ledger-conway,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_conway\n    Test.Cardano.Ledger.Conway.Binary.CddlSpec\n    Test.Cardano.Ledger.Conway.GoldenSpec\n    Test.Cardano.Ledger.Conway.GoldenTranslation\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    HUnit,\n    base,\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-binary:testlib,\n    cardano-ledger-conway,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:testlib,\n    testlib,\n";
  }