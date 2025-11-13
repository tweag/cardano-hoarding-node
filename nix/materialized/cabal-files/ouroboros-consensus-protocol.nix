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
      identifier = {
        name = "ouroboros-consensus-protocol";
        version = "0.13.0.0";
      };
      license = "Apache-2.0";
      copyright = "2021-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Cardano consensus protocols";
      description = "Cardano consensus protocols.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
          (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "unstable-protocol-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      tests = {
        "protocol-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-consensus-protocol-0.13.0.0.tar.gz";
      sha256 = "cfc9f40730a1520d59a535b5c48b5df8e8cde8232e4a0864ea8fb4a4175a4f13";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-consensus-protocol\nversion: 0.13.0.0\nsynopsis: Cardano consensus protocols\ndescription: Cardano consensus protocols.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:\n  2021-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\n\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Network\nextra-doc-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/ouroboros-consensus\n  subdir: ouroboros-consensus-protocol\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\ncommon common-lib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\ncommon common-test\n  import: common-lib\n  ghc-options:\n    -threaded\n    -rtsopts\n\nlibrary\n  import: common-lib\n  hs-source-dirs: src/ouroboros-consensus-protocol\n  exposed-modules:\n    Ouroboros.Consensus.Protocol.Ledger.HotKey\n    Ouroboros.Consensus.Protocol.Ledger.Util\n    Ouroboros.Consensus.Protocol.Praos\n    Ouroboros.Consensus.Protocol.Praos.AgentClient\n    Ouroboros.Consensus.Protocol.Praos.Common\n    Ouroboros.Consensus.Protocol.Praos.Header\n    Ouroboros.Consensus.Protocol.Praos.VRF\n    Ouroboros.Consensus.Protocol.Praos.Views\n    Ouroboros.Consensus.Protocol.TPraos\n\n  build-depends:\n    Win32-network ^>=0.2,\n    base >=4.14 && <4.22,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class ^>=2.2,\n    cardano-ledger-binary,\n    cardano-ledger-core,\n    cardano-ledger-shelley,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    containers,\n    contra-tracer ^>=0.1.0,\n    io-classes ^>=1.8.0,\n    io-sim,\n    kes-agent <1,\n    kes-agent-crypto <1,\n    mtl,\n    network ^>=3.2.7,\n    nothunks,\n    ouroboros-consensus >=0.23 && <0.29,\n    ouroboros-network-framework ^>=0.19,\n    ouroboros-network-testing ^>=0.8,\n    serialise,\n    text,\n\nlibrary unstable-protocol-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-protocol-testlib\n  exposed-modules:\n    Test.Consensus.Protocol.Serialisation.Generators\n    Test.Ouroboros.Consensus.Protocol.Praos.Header\n\n  build-depends:\n    QuickCheck,\n    aeson,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-praos ^>=2.2,\n    cardano-crypto-tests ^>=2.2,\n    cardano-ledger-binary,\n    cardano-ledger-core,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    containers,\n    ouroboros-consensus-protocol,\n    text,\n\ntest-suite protocol-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/protocol-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.Protocol.Praos.SelectView\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-crypto-class ^>=2.2,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core >=1.17 && <1.19,\n    cardano-protocol-tpraos ^>=1.4.1,\n    containers,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-protocol,\n    serialise,\n    tasty,\n    tasty-quickcheck,\n";
  }