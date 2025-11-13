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
      identifier = { name = "cardano-ledger-dijkstra"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger with nested transactions";
      description = "This package builds upon the Conway ledger with a nested transactions system.";
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
          (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-dijkstra-0.1.0.0.tar.gz";
      sha256 = "020d1b6a5f84ab4d5f2debced8ca4d3e662527260c5b26f6d8989a283da3f337";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-dijkstra\nversion: 0.1.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Cardano ledger with nested transactions\ndescription:\n  This package builds upon the Conway ledger with a nested transactions system.\n\ncategory: Network\nbuild-type: Simple\ndata-files:\n  cddl-files/dijkstra.cddl\n  golden/*.json\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/dijkstra\n\nflag asserts\n  description: Enable assertions\n  default: False\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Dijkstra\n    Cardano.Ledger.Dijkstra.BlockBody\n    Cardano.Ledger.Dijkstra.Core\n    Cardano.Ledger.Dijkstra.Era\n    Cardano.Ledger.Dijkstra.Genesis\n    Cardano.Ledger.Dijkstra.Governance\n    Cardano.Ledger.Dijkstra.PParams\n    Cardano.Ledger.Dijkstra.Rules\n    Cardano.Ledger.Dijkstra.Scripts\n    Cardano.Ledger.Dijkstra.State\n    Cardano.Ledger.Dijkstra.Transition\n    Cardano.Ledger.Dijkstra.Tx\n    Cardano.Ledger.Dijkstra.TxAuxData\n    Cardano.Ledger.Dijkstra.TxBody\n    Cardano.Ledger.Dijkstra.TxCert\n    Cardano.Ledger.Dijkstra.TxInfo\n    Cardano.Ledger.Dijkstra.TxOut\n    Cardano.Ledger.Dijkstra.TxWits\n    Cardano.Ledger.Dijkstra.UTxO\n\n  other-modules:\n    Cardano.Ledger.Dijkstra.Rules.Bbody\n    Cardano.Ledger.Dijkstra.Rules.Cert\n    Cardano.Ledger.Dijkstra.Rules.Certs\n    Cardano.Ledger.Dijkstra.Rules.Deleg\n    Cardano.Ledger.Dijkstra.Rules.Gov\n    Cardano.Ledger.Dijkstra.Rules.GovCert\n    Cardano.Ledger.Dijkstra.Rules.Ledger\n    Cardano.Ledger.Dijkstra.Rules.Ledgers\n    Cardano.Ledger.Dijkstra.Rules.Pool\n    Cardano.Ledger.Dijkstra.Rules.Utxo\n    Cardano.Ledger.Dijkstra.Rules.Utxos\n    Cardano.Ledger.Dijkstra.Rules.Utxow\n    Cardano.Ledger.Dijkstra.State.Account\n    Cardano.Ledger.Dijkstra.State.CertState\n    Cardano.Ledger.Dijkstra.State.Stake\n    Cardano.Ledger.Dijkstra.Translation\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.14 && <5,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo,\n    cardano-ledger-babbage,\n    cardano-ledger-binary,\n    cardano-ledger-conway,\n    cardano-ledger-core:{cardano-ledger-core, internal} >=1.18,\n    cardano-ledger-mary,\n    cardano-ledger-shelley,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    deepseq,\n    mempack,\n    microlens,\n    nothunks,\n    plutus-ledger-api,\n    small-steps >=1.1.2,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary testlib\n  visibility: public\n  hs-source-dirs: testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Dijkstra.Arbitrary\n    Test.Cardano.Ledger.Dijkstra.Binary.Annotator\n    Test.Cardano.Ledger.Dijkstra.Binary.Cddl\n    Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip\n    Test.Cardano.Ledger.Dijkstra.CDDL\n    Test.Cardano.Ledger.Dijkstra.Era\n    Test.Cardano.Ledger.Dijkstra.Examples\n    Test.Cardano.Ledger.Dijkstra.Imp\n    Test.Cardano.Ledger.Dijkstra.ImpTest\n    Test.Cardano.Ledger.Dijkstra.TreeDiff\n\n  other-modules: Paths_cardano_ledger_dijkstra\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-data,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-babbage:{cardano-ledger-babbage, testlib},\n    cardano-ledger-binary,\n    cardano-ledger-conway:{cardano-ledger-conway, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra,\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    generic-random,\n    heredoc,\n    microlens,\n    small-steps,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_dijkstra\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.5,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_dijkstra\n    Test.Cardano.Ledger.Dijkstra.GoldenSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-core:testlib,\n    cardano-ledger-dijkstra:{cardano-ledger-dijkstra, testlib},\n    cardano-ledger-shelley:testlib,\n";
  }