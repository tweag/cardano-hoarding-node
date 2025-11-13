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
      identifier = { name = "ouroboros-network-api"; version = "0.16.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knutsson";
      homepage = "";
      url = "";
      synopsis = "A networking api shared with ouroboros-consensus";
      description = "A networking api shared with ouroboros-consensus.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-api-0.16.0.0.tar.gz";
      sha256 = "476434114d7dcb64ad67e93a0564673c8e7af0fe20f84cccc0f4e91c556dbeb3";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-network-api\nversion: 0.16.0.0\nsynopsis: A networking api shared with ouroboros-consensus\ndescription: A networking api shared with ouroboros-consensus.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knutsson\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules:\n    Cardano.Network.ConsensusMode\n    Cardano.Network.PeerSelection.Bootstrap\n    Cardano.Network.PeerSelection.LocalRootPeers\n    Cardano.Network.PeerSelection.PeerTrustable\n    Cardano.Network.Types\n    Ouroboros.Network.AnchoredFragment\n    Ouroboros.Network.AnchoredSeq\n    Ouroboros.Network.Block\n    Ouroboros.Network.BlockFetch.ConsensusInterface\n    Ouroboros.Network.CodecCBORTerm\n    Ouroboros.Network.ControlMessage\n    Ouroboros.Network.Handshake\n    Ouroboros.Network.Handshake.Acceptable\n    Ouroboros.Network.Handshake.Queryable\n    Ouroboros.Network.Magic\n    Ouroboros.Network.NodeToClient.Version\n    Ouroboros.Network.NodeToNode.Version\n    Ouroboros.Network.PeerSelection.LedgerPeers.Type\n    Ouroboros.Network.PeerSelection.LedgerPeers.Utils\n    Ouroboros.Network.PeerSelection.PeerAdvertise\n    Ouroboros.Network.PeerSelection.PeerMetric.Type\n    Ouroboros.Network.PeerSelection.PeerSharing\n    Ouroboros.Network.PeerSelection.PeerSharing.Codec\n    Ouroboros.Network.PeerSelection.RelayAccessPoint\n    Ouroboros.Network.Point\n    Ouroboros.Network.Protocol.Limits\n    Ouroboros.Network.SizeInBytes\n    Ouroboros.Network.Util.ShowProxy\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    aeson,\n    base >=4.14 && <4.22,\n    base16-bytestring,\n    bytestring >=0.10 && <0.13,\n    cardano-binary,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg >=0.2.1 && <0.3,\n    containers,\n    contra-tracer,\n    deepseq,\n    dns,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8.0.1,\n    iproute ^>=1.7.15,\n    measures,\n    network ^>=3.2.7,\n    network-mux ^>=0.9,\n    nothunks,\n    quiet,\n    random,\n    serialise >=0.2 && <0.3,\n    text >=1.2 && <2.2,\n    typed-protocols ^>=1.0,\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-modules:\n    Test.Ouroboros.Network.PeerSelection.RelayAccessPoint\n\n  build-depends:\n    QuickCheck,\n    aeson,\n    base >=4.14 && <4.22,\n    bytestring,\n    cardano-binary,\n    cborg,\n    iproute,\n    ouroboros-network-api,\n    tasty,\n    tasty-quickcheck,\n    with-utf8,\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n    -rtsopts\n";
  }