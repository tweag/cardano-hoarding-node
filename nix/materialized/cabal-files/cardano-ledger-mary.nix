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
      identifier = { name = "cardano-ledger-mary"; version = "1.9.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano ledger with multiasset support.";
      description = "This package extends the Allegra ledger with support for\nnative tokens.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data".components.sublibs.testlib or (errorHandler.buildDepError "cardano-data:testlib"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          ];
          buildable = true;
        };
      };
      exes = {
        "huddle-cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-data".components.sublibs.testlib or (errorHandler.buildDepError "cardano-data:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-mary-1.9.0.0.tar.gz";
      sha256 = "8bf2b530e9eeb88b7dc8c0aec34b5e84ef13b64f6425c658c57d53c38443b9dd";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-mary\nversion: 1.9.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Cardano ledger with multiasset support.\ndescription:\n  This package extends the Allegra ledger with support for\n  native tokens.\n\ncategory: Network\nbuild-type: Simple\ndata-files: cddl-files/mary.cddl\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: eras/mary/impl\n\nflag asserts\n  description: Enable assertions\n  default: False\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Mary\n    Cardano.Ledger.Mary.Core\n    Cardano.Ledger.Mary.State\n    Cardano.Ledger.Mary.Transition\n    Cardano.Ledger.Mary.TxBody\n    Cardano.Ledger.Mary.TxOut\n    Cardano.Ledger.Mary.UTxO\n    Cardano.Ledger.Mary.Value\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Mary.BlockBody\n    Cardano.Ledger.Mary.Era\n    Cardano.Ledger.Mary.PParams\n    Cardano.Ledger.Mary.Rules\n    Cardano.Ledger.Mary.Rules.Bbody\n    Cardano.Ledger.Mary.Rules.Deleg\n    Cardano.Ledger.Mary.Rules.Delegs\n    Cardano.Ledger.Mary.Rules.Delpl\n    Cardano.Ledger.Mary.Rules.Ledger\n    Cardano.Ledger.Mary.Rules.Ledgers\n    Cardano.Ledger.Mary.Rules.Pool\n    Cardano.Ledger.Mary.Rules.Ppup\n    Cardano.Ledger.Mary.Rules.Utxo\n    Cardano.Ledger.Mary.Rules.Utxow\n    Cardano.Ledger.Mary.Scripts\n    Cardano.Ledger.Mary.State.Account\n    Cardano.Ledger.Mary.State.CertState\n    Cardano.Ledger.Mary.State.Stake\n    Cardano.Ledger.Mary.Translation\n    Cardano.Ledger.Mary.Tx\n    Cardano.Ledger.Mary.TxAuxData\n    Cardano.Ledger.Mary.TxCert\n    Cardano.Ledger.Mary.TxWits\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    aeson >=2.2,\n    base >=4.18 && <5,\n    base16-bytestring,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data ^>=1.2,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-binary >=1.4,\n    cardano-ledger-core:{cardano-ledger-core, internal} >=1.18,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-strict-containers,\n    containers,\n    deepseq,\n    groups,\n    mempack,\n    microlens,\n    nothunks,\n    primitive,\n    text,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Mary.Arbitrary\n    Test.Cardano.Ledger.Mary.Binary.Annotator\n    Test.Cardano.Ledger.Mary.Binary.Cddl\n    Test.Cardano.Ledger.Mary.CDDL\n    Test.Cardano.Ledger.Mary.Era\n    Test.Cardano.Ledger.Mary.Examples\n    Test.Cardano.Ledger.Mary.Imp\n    Test.Cardano.Ledger.Mary.Imp.UtxoSpec\n    Test.Cardano.Ledger.Mary.ImpTest\n    Test.Cardano.Ledger.Mary.TreeDiff\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules: Paths_cardano_ledger_mary\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data:testlib,\n    cardano-ledger-allegra:{cardano-ledger-allegra, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-strict-containers,\n    containers,\n    cuddle >=0.4,\n    microlens,\n\nexecutable huddle-cddl\n  main-is: Main.hs\n  hs-source-dirs: huddle-cddl\n  other-modules: Paths_cardano_ledger_mary\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    cardano-ledger-binary:testlib >=1.3.4.0,\n    testlib,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.Mary.Binary.CddlSpec\n    Test.Cardano.Ledger.Mary.BinarySpec\n    Test.Cardano.Ledger.Mary.ValueSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-data:{cardano-data, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary,\n    cardano-ledger-shelley:testlib,\n    containers,\n    testlib,\n";
  }