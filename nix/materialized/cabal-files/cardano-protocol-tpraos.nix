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
      identifier = { name = "cardano-protocol-tpraos"; version = "1.4.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano Protocol: Transitional Praos";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-protocol-tpraos".components.sublibs.testlib or (errorHandler.buildDepError "cardano-protocol-tpraos:testlib"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-protocol-tpraos-1.4.1.0.tar.gz";
      sha256 = "0ba610b15e73245736a58607b612ff77142fb7f5e004afe42f58f330598b3ea5";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-protocol-tpraos\nversion: 1.4.1.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Cardano Protocol: Transitional Praos\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/cardano-protocol-tpraos\n\nlibrary\n  exposed-modules:\n    Cardano.Protocol.Crypto\n    Cardano.Protocol.TPraos.API\n    Cardano.Protocol.TPraos.BHeader\n    Cardano.Protocol.TPraos.OCert\n    Cardano.Protocol.TPraos.Rules.OCert\n    Cardano.Protocol.TPraos.Rules.Overlay\n    Cardano.Protocol.TPraos.Rules.Prtcl\n    Cardano.Protocol.TPraos.Rules.Tickn\n    Cardano.Protocol.TPraos.Rules.Updn\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    base >=4.18 && <5,\n    bytestring,\n    cardano-crypto-class ^>=2.2,\n    cardano-ledger-allegra >=1.1,\n    cardano-ledger-alonzo >=1.2,\n    cardano-ledger-babbage >=1.1,\n    cardano-ledger-binary >=1.6,\n    cardano-ledger-conway >=1.1,\n    cardano-ledger-core >=1.18,\n    cardano-ledger-dijkstra >=0.1,\n    cardano-ledger-mary >=1.1,\n    cardano-ledger-shelley >=1.16,\n    cardano-slotting,\n    containers,\n    deepseq,\n    microlens,\n    mtl,\n    non-integral,\n    nothunks,\n    quiet,\n    set-algebra,\n    small-steps >=1.1,\n    transformers,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Protocol.Binary.Annotator\n    Test.Cardano.Protocol.Binary.Cddl\n    Test.Cardano.Protocol.Binary.RoundTrip\n    Test.Cardano.Protocol.Crypto.KES\n    Test.Cardano.Protocol.Crypto.VRF\n    Test.Cardano.Protocol.Crypto.VRF.Fake\n    Test.Cardano.Protocol.TPraos.Arbitrary\n    Test.Cardano.Protocol.TPraos.Create\n    Test.Cardano.Protocol.TPraos.Examples\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto-class >=2.1.1,\n    cardano-ledger-allegra:{cardano-ledger-allegra, testlib},\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-conway:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-protocol-tpraos,\n    cardano-strict-containers,\n    containers,\n    generic-random,\n    microlens,\n    nothunks,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Protocol.Binary.BinarySpec\n    Test.Cardano.Protocol.Binary.CddlSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-ledger-allegra:{cardano-ledger-allegra, testlib},\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-protocol-tpraos:{cardano-protocol-tpraos, testlib},\n    cuddle,\n";
  }