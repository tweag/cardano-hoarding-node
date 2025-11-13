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
      identifier = { name = "cardano-crypto-test"; version = "1.6.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-crypto exposed to other packages";
      description = "Test helpers from cardano-crypto exposed to other packages";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-crypto-test-1.6.0.0.tar.gz";
      sha256 = "62d7d94f73eef151eb7418e0976d17056b0d16aad6159aba35fbfe8028020698";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-crypto-test\nversion: 1.6.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nsynopsis: Test helpers from cardano-crypto exposed to other packages\ndescription: Test helpers from cardano-crypto exposed to other packages\ncategory: Currency\nbuild-type: Simple\ndata-files:\n  golden/AbstractHash\n  golden/DecShare\n  golden/EncShare\n  golden/PassPhrase\n  golden/RedeemSignature\n  golden/RedeemSigningKey\n  golden/RedeemVerificationKey\n  golden/Secret\n  golden/SecretProof\n  golden/Signature\n  golden/SigningKey\n  golden/VerificationKey\n  golden/VssPublicKey\n  golden/json/ProtocolMagic0_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic1_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic2_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic_Legacy_NMMustBeJust\n  golden/json/ProtocolMagic_Legacy_NMMustBeNothing\n\nlibrary\n  exposed-modules:\n    Test.Cardano.Crypto.CBOR\n    Test.Cardano.Crypto.Dummy\n    Test.Cardano.Crypto.Example\n    Test.Cardano.Crypto.Gen\n    Test.Cardano.Crypto.Json\n    Test.Cardano.Crypto.Orphans\n\n  cpp-options: -DCARDANO_CRYPTO_TEST\n  other-modules:\n    GetDataFileName\n    Paths_cardano_crypto_test\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Weverything\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n\n  build-depends:\n    base >=4.14 && <5,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-wrapper ^>=1.6,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib} >=1.3.1,\n    cardano-prelude,\n    cardano-prelude-test,\n    crypton,\n    hedgehog >=1.0.4,\n    memory,\n";
  }