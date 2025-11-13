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
      identifier = { name = "cardano-ledger-allegra"; version = "1.8.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Allegra ledger era that introduces time lock support.";
      description = "This package builds upon Shelley era with support for timelocks.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
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
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-allegra-1.8.0.0.tar.gz";
      sha256 = "6aa2f556eff6427e8a68a675ae4f7eeac9a6c5e5b0cbd14ef22d7ac9cb1b86bf";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-allegra\nversion: 1.8.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Allegra ledger era that introduces time lock support.\ndescription:\n  This package builds upon Shelley era with support for timelocks.\n\ncategory: Network\nbuild-type: Simple\ndata-files: cddl-files/allegra.cddl\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/allegra/impl\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Allegra\n    Cardano.Ledger.Allegra.Core\n    Cardano.Ledger.Allegra.Rules\n    Cardano.Ledger.Allegra.Scripts\n    Cardano.Ledger.Allegra.State\n    Cardano.Ledger.Allegra.Transition\n    Cardano.Ledger.Allegra.Translation\n    Cardano.Ledger.Allegra.Tx\n    Cardano.Ledger.Allegra.TxAuxData\n    Cardano.Ledger.Allegra.TxBody\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Allegra.BlockBody\n    Cardano.Ledger.Allegra.Era\n    Cardano.Ledger.Allegra.PParams\n    Cardano.Ledger.Allegra.Rules.Bbody\n    Cardano.Ledger.Allegra.Rules.Deleg\n    Cardano.Ledger.Allegra.Rules.Delegs\n    Cardano.Ledger.Allegra.Rules.Delpl\n    Cardano.Ledger.Allegra.Rules.Ledger\n    Cardano.Ledger.Allegra.Rules.Ledgers\n    Cardano.Ledger.Allegra.Rules.Pool\n    Cardano.Ledger.Allegra.Rules.Ppup\n    Cardano.Ledger.Allegra.Rules.Utxo\n    Cardano.Ledger.Allegra.Rules.Utxow\n    Cardano.Ledger.Allegra.State.Account\n    Cardano.Ledger.Allegra.State.CertState\n    Cardano.Ledger.Allegra.State.Stake\n    Cardano.Ledger.Allegra.TxCert\n    Cardano.Ledger.Allegra.TxOut\n    Cardano.Ledger.Allegra.TxWits\n    Cardano.Ledger.Allegra.UTxO\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-ledger-binary >=1.4,\n    cardano-ledger-core:{cardano-ledger-core, internal} >=1.18,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    deepseq,\n    mempack,\n    microlens,\n    nothunks,\n    small-steps >=1.1,\n    transformers,\n    validation-selective,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Allegra.Arbitrary\n    Test.Cardano.Ledger.Allegra.Binary.Annotator\n    Test.Cardano.Ledger.Allegra.Binary.Cddl\n    Test.Cardano.Ledger.Allegra.CDDL\n    Test.Cardano.Ledger.Allegra.Era\n    Test.Cardano.Ledger.Allegra.Examples\n    Test.Cardano.Ledger.Allegra.Imp\n    Test.Cardano.Ledger.Allegra.Imp.UtxowSpec\n    Test.Cardano.Ledger.Allegra.ImpTest\n    Test.Cardano.Ledger.Allegra.TreeDiff\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_allegra\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-allegra,\n    cardano-ledger-binary,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    generic-random,\n    heredoc,\n    microlens,\n    mtl,\n    small-steps,\n    text,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_allegra\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.3.4.0,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.Allegra.Binary.CddlSpec\n    Test.Cardano.Ledger.Allegra.BinarySpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-allegra,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-shelley:testlib,\n    testlib,\n";
  }