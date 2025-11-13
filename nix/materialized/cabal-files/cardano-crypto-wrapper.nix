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
      identifier = { name = "cardano-crypto-wrapper"; version = "1.6.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cryptographic primitives used in Byron era of the Cardano project";
      description = "Cryptographic primitives used in Byron era of the Cardano project";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."heapwords" or (errorHandler.buildDepError "heapwords"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
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
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."cardano-crypto-wrapper".components.sublibs.testlib or (errorHandler.buildDepError "cardano-crypto-wrapper:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-crypto-wrapper-1.6.1.0.tar.gz";
      sha256 = "5e9ece4bf68e2e7b298fbcea7411dc53d2b46cce144af46190de882b8dee75c3";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-crypto-wrapper\nversion: 1.6.1.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nsynopsis:\n  Cryptographic primitives used in Byron era of the Cardano project\n\ndescription:\n  Cryptographic primitives used in Byron era of the Cardano project\n\ncategory: Currency\nbuild-type: Simple\ndata-files:\n  golden/AbstractHash\n  golden/DecShare\n  golden/EncShare\n  golden/PassPhrase\n  golden/RedeemSignature\n  golden/RedeemSigningKey\n  golden/RedeemVerificationKey\n  golden/Secret\n  golden/SecretProof\n  golden/Signature\n  golden/SigningKey\n  golden/VerificationKey\n  golden/VssPublicKey\n  golden/json/ProtocolMagic0_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic1_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic2_Legacy_HasNetworkMagic\n  golden/json/ProtocolMagic_Legacy_NMMustBeJust\n  golden/json/ProtocolMagic_Legacy_NMMustBeNothing\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nlibrary\n  exposed-modules:\n    Cardano.Crypto\n    Cardano.Crypto.Hashing\n    Cardano.Crypto.Orphans\n    Cardano.Crypto.ProtocolMagic\n    Cardano.Crypto.Random\n    Cardano.Crypto.Raw\n    Cardano.Crypto.Signing\n    Cardano.Crypto.Signing.Redeem\n    Cardano.Crypto.Signing.Safe\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Crypto.Signing.KeyGen\n    Cardano.Crypto.Signing.Redeem.Compact\n    Cardano.Crypto.Signing.Redeem.KeyGen\n    Cardano.Crypto.Signing.Redeem.Signature\n    Cardano.Crypto.Signing.Redeem.SigningKey\n    Cardano.Crypto.Signing.Redeem.VerificationKey\n    Cardano.Crypto.Signing.Safe.KeyGen\n    Cardano.Crypto.Signing.Safe.PassPhrase\n    Cardano.Crypto.Signing.Safe.SafeSigner\n    Cardano.Crypto.Signing.Signature\n    Cardano.Crypto.Signing.SigningKey\n    Cardano.Crypto.Signing.Tag\n    Cardano.Crypto.Signing.VerificationKey\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.18 && <5,\n    base16-bytestring >=1,\n    base64-bytestring,\n    base64-bytestring-type,\n    binary,\n    bytestring,\n    canonical-json,\n    cardano-crypto,\n    cardano-ledger-binary >=1.3.1,\n    cardano-prelude >=0.2.0.0,\n    crypton,\n    data-default,\n    deepseq,\n    formatting,\n    heapwords,\n    memory,\n    nothunks,\n    text,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Crypto.CBOR\n    Test.Cardano.Crypto.Dummy\n    Test.Cardano.Crypto.Example\n    Test.Cardano.Crypto.Gen\n    Test.Cardano.Crypto.Json\n    Test.Cardano.Crypto.Orphans\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules:\n    Paths_cardano_crypto_wrapper\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Weverything\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-wrapper,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-prelude,\n    cardano-prelude-test,\n    crypton,\n    hedgehog >=1.0.4,\n    memory,\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: test.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_crypto_wrapper\n    Test.Cardano.Crypto.Hashing\n    Test.Cardano.Crypto.Keys\n    Test.Cardano.Crypto.Limits\n    Test.Cardano.Crypto.Random\n    Test.Cardano.Crypto.Signing.Redeem\n    Test.Cardano.Crypto.Signing.Redeem.Compact\n    Test.Cardano.Crypto.Signing.Safe\n    Test.Cardano.Crypto.Signing.Signing\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n    -threaded\n    -rtsopts\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-wrapper,\n    cardano-ledger-binary,\n    cardano-prelude,\n    cardano-prelude-test,\n    crypton,\n    formatting,\n    hedgehog >=1.0.4,\n    testlib,\n";
  }