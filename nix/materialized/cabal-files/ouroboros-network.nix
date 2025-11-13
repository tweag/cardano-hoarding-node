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
    flags = { asserts = false; txsubmission-delay = true; };
    package = {
      specVersion = "3.4";
      identifier = { name = "ouroboros-network"; version = "0.22.3.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Ouroboros blockchain protocol";
      description = "A networking layer for the Ouroboros blockchain protocol.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.mtl or (errorHandler.buildDepError "io-classes:mtl"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."strict-checked-vars" or (errorHandler.buildDepError "strict-checked-vars"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."directory" or (errorHandler.buildDepError "directory"));
        buildable = true;
      };
      sublibs = {
        "orphan-instances" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network".components.sublibs.cardano-diffusion or (errorHandler.buildDepError "ouroboros-network:cardano-diffusion"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
        "cardano-diffusion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
        };
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network".components.sublibs.cardano-diffusion or (errorHandler.buildDepError "ouroboros-network:cardano-diffusion"))
            (hsPkgs."ouroboros-network".components.sublibs.orphan-instances or (errorHandler.buildDepError "ouroboros-network:orphan-instances"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-framework".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-framework:testlib"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."quickcheck-monoids" or (errorHandler.buildDepError "quickcheck-monoids"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
          ];
          buildable = true;
        };
      };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."infinite-list" or (errorHandler.buildDepError "infinite-list"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
          buildable = true;
        };
      };
      tests = {
        "sim-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-network".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network:testlib"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "io-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ] ++ (if system.isWindows
            then [
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            ]
            else [
              (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ]);
          buildable = true;
        };
      };
      benchmarks = {
        "sim-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-network".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network:testlib"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-0.22.3.0.tar.gz";
      sha256 = "3269a49f7bf498b4b92d3f5895ba2a8aa028a93acae0b9dfeefb374514f4a29c";
    });
  }) // {
    package-description-override = "cabal-version: 3.4\nname: ouroboros-network\nversion: 0.22.3.0\nsynopsis: A networking layer for the Ouroboros blockchain protocol\ndescription: A networking layer for the Ouroboros blockchain protocol.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\nflag txsubmission-delay\n  description: Delay initial request for transactions from outbound/client peer\n  manual: True\n  default: True\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/ouroboros-network\n\ncommon ghc-options\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\n-- in tests librararies redundant constraints are sometimes useful (e.g.\n-- by default truned off debug tracing might require extra constraints like\n-- `Show` or `MonadSay`).\ncommon ghc-options-tests\n  import: ghc-options\n  ghc-options: -Wno-redundant-constraints\n\nlibrary\n  import: ghc-options\n  hs-source-dirs: src\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:\n    Control.Concurrent.Class.MonadSTM.Strict.TMergeVar\n    Ouroboros.Network.BlockFetch\n    Ouroboros.Network.BlockFetch.Client\n    Ouroboros.Network.BlockFetch.ClientRegistry\n    Ouroboros.Network.BlockFetch.ClientState\n    Ouroboros.Network.BlockFetch.Decision\n    Ouroboros.Network.BlockFetch.Decision.Genesis\n    Ouroboros.Network.BlockFetch.Decision.Trace\n    Ouroboros.Network.BlockFetch.DeltaQ\n    Ouroboros.Network.BlockFetch.State\n    Ouroboros.Network.DeltaQ\n    Ouroboros.Network.Diffusion\n    Ouroboros.Network.Diffusion.Configuration\n    Ouroboros.Network.Diffusion.Policies\n    Ouroboros.Network.Diffusion.Topology\n    Ouroboros.Network.Diffusion.Types\n    Ouroboros.Network.Diffusion.Utils\n    Ouroboros.Network.ExitPolicy\n    Ouroboros.Network.KeepAlive\n    Ouroboros.Network.NodeToClient\n    Ouroboros.Network.NodeToNode\n    Ouroboros.Network.PeerSelection\n    Ouroboros.Network.PeerSelection.Churn\n    Ouroboros.Network.PeerSelection.Governor\n    Ouroboros.Network.PeerSelection.Governor.ActivePeers\n    Ouroboros.Network.PeerSelection.Governor.Monitor\n    Ouroboros.Network.PeerSelection.Governor.Types\n    Ouroboros.Network.PeerSelection.LedgerPeers\n    Ouroboros.Network.PeerSelection.PeerMetric\n    Ouroboros.Network.PeerSelection.PeerSelectionActions\n    Ouroboros.Network.PeerSelection.PeerStateActions\n    Ouroboros.Network.PeerSelection.PublicRootPeers\n    Ouroboros.Network.PeerSelection.RootPeersDNS\n    Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions\n    Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore\n    Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers\n    Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers\n    Ouroboros.Network.PeerSelection.State.EstablishedPeers\n    Ouroboros.Network.PeerSelection.State.KnownPeers\n    Ouroboros.Network.PeerSelection.State.LocalRootPeers\n    Ouroboros.Network.PeerSelection.Types\n    Ouroboros.Network.PeerSharing\n    Ouroboros.Network.TxSubmission.Inbound\n    Ouroboros.Network.TxSubmission.Mempool.Reader\n    Ouroboros.Network.TxSubmission.Outbound\n\n  other-modules:\n    Ouroboros.Network.PeerSelection.Governor.BigLedgerPeers\n    Ouroboros.Network.PeerSelection.Governor.EstablishedPeers\n    Ouroboros.Network.PeerSelection.Governor.KnownPeers\n    Ouroboros.Network.PeerSelection.Governor.RootPeers\n    Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers\n\n  reexported-modules:\n    Ouroboros.Network.AnchoredFragment,\n    Ouroboros.Network.AnchoredSeq,\n    Ouroboros.Network.Magic,\n    Ouroboros.Network.NodeToClient.Version,\n    Ouroboros.Network.NodeToNode.Version,\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-extensions:\n    BangPatterns\n    DataKinds\n    EmptyCase\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTSyntax\n    GADTs\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    NamedFieldPuns\n    OverloadedStrings\n    PolyKinds\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    TemplateHaskell\n    TupleSections\n    TypeApplications\n    TypeFamilies\n    TypeInType\n\n  build-depends:\n    async >=2.2 && <2.3,\n    base >=4.14 && <4.22,\n    bytestring >=0.10 && <0.13,\n    cardano-prelude,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg >=0.2.1 && <0.3,\n    containers,\n    contra-tracer,\n    deepseq,\n    dlist,\n    dns,\n    hashable,\n    io-classes:{io-classes, mtl, si-timers, strict-stm} ^>=1.8.0.1,\n    iproute,\n    monoidal-synchronisation,\n    mtl,\n    network ^>=3.2.7,\n    network-mux,\n    nothunks,\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-framework ^>=0.19,\n    ouroboros-network-protocols ^>=0.15,\n    psqueues >=0.2.3 && <0.3,\n    random,\n    strict-checked-vars ^>=0.2,\n    transformers,\n    typed-protocols:{typed-protocols, stateful} ^>=1.0,\n\n  if !os(windows)\n    build-depends:\n      directory\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\n  if flag(txsubmission-delay)\n    cpp-options: -DTXSUBMISSION_DELAY\n\nlibrary orphan-instances\n  import: ghc-options\n  visibility: public\n  hs-source-dirs: orphan-instances\n  exposed-modules:\n    Cardano.Network.OrphanInstances\n    Ouroboros.Network.OrphanInstances\n\n  other-modules:\n  reexported-modules:\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-extensions:\n    BangPatterns\n    DataKinds\n    EmptyCase\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTSyntax\n    GADTs\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    NamedFieldPuns\n    OverloadedStrings\n    PolyKinds\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    TemplateHaskell\n    TupleSections\n    TypeApplications\n    TypeFamilies\n    TypeInType\n\n  build-depends:\n    aeson,\n    base >=4.14 && <4.22,\n    containers,\n    iproute,\n    network,\n    network-mux,\n    ouroboros-network:{ouroboros-network, cardano-diffusion},\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-framework ^>=0.19,\n    text,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary cardano-diffusion\n  import: ghc-options\n  visibility: public\n  hs-source-dirs: cardano-diffusion\n  exposed-modules:\n    Cardano.Network.Diffusion\n    Cardano.Network.Diffusion.Configuration\n    Cardano.Network.Diffusion.Handlers\n    Cardano.Network.Diffusion.Policies\n    Cardano.Network.Diffusion.Topology\n    Cardano.Network.Diffusion.Types\n    Cardano.Network.LedgerPeerConsensusInterface\n    Cardano.Network.PeerSelection.Churn\n    Cardano.Network.PeerSelection.ExtraRootPeers\n    Cardano.Network.PeerSelection.Governor.Monitor\n    Cardano.Network.PeerSelection.Governor.PeerSelectionActions\n    Cardano.Network.PeerSelection.Governor.PeerSelectionState\n    Cardano.Network.PeerSelection.Governor.Types\n    Cardano.Network.PeerSelection.PeerSelectionActions\n    Cardano.Network.PeerSelection.PublicRootPeers\n\n  other-modules:\n  reexported-modules:\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-extensions:\n    BangPatterns\n    DataKinds\n    EmptyCase\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTSyntax\n    GADTs\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    NamedFieldPuns\n    OverloadedStrings\n    PolyKinds\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    TemplateHaskell\n    TupleSections\n    TypeApplications\n    TypeFamilies\n    TypeInType\n\n  build-depends:\n    base >=4.14 && <4.22,\n    containers,\n    contra-tracer,\n    dns,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8,\n    monoidal-synchronisation,\n    network ^>=3.2.7,\n    ouroboros-network,\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-framework ^>=0.19,\n    ouroboros-network-protocols ^>=0.15,\n    random,\n\n  if !os(windows)\n    build-depends:\n      unix\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\n-- Simulation Test Library\nlibrary testlib\n  mixins:\n    QuickCheck hiding (Test.QuickCheck.Monoids)\n\n  import: ghc-options-tests\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  visibility: public\n  hs-source-dirs: testlib\n  build-depends:\n    QuickCheck,\n    aeson,\n    array,\n    base >=4.14 && <4.22,\n    bytestring,\n    cardano-slotting,\n    cborg,\n    containers,\n    contra-tracer,\n    deepseq,\n    dns,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    iproute,\n    monoidal-synchronisation,\n    mtl,\n    network,\n    network-mux,\n    nothunks,\n    ouroboros-network:{ouroboros-network, cardano-diffusion, orphan-instances},\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-framework:testlib,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    ouroboros-network-protocols:testlib,\n    ouroboros-network-testing ^>=0.8.1,\n    pipes,\n    pretty-simple,\n    psqueues,\n    quickcheck-monoids,\n    random,\n    serialise,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    text,\n    time >=1.9.1 && <1.14,\n    typed-protocols:{typed-protocols, examples},\n\n  exposed-modules:\n    Ouroboros.Network.BlockFetch.Examples\n    Ouroboros.Network.MockNode\n    Test.Ouroboros.Network.BlockFetch\n    Test.Ouroboros.Network.Diffusion.Node\n    Test.Ouroboros.Network.Diffusion.Node.ChainDB\n    Test.Ouroboros.Network.Diffusion.Node.Kernel\n    Test.Ouroboros.Network.Diffusion.Node.MiniProtocols\n    Test.Ouroboros.Network.Diffusion.Policies\n    Test.Ouroboros.Network.Diffusion.Testnet.Cardano\n    Test.Ouroboros.Network.Diffusion.Testnet.Cardano.Simulation\n    Test.Ouroboros.Network.KeepAlive\n    Test.Ouroboros.Network.LedgerPeers\n    Test.Ouroboros.Network.MockNode\n    Test.Ouroboros.Network.Mux\n    Test.Ouroboros.Network.NodeToClient.Version\n    Test.Ouroboros.Network.NodeToNode.Version\n    Test.Ouroboros.Network.OrphanInstances.Tests\n    Test.Ouroboros.Network.Orphans\n    Test.Ouroboros.Network.PeerSelection\n    Test.Ouroboros.Network.PeerSelection.Cardano.Instances\n    Test.Ouroboros.Network.PeerSelection.Cardano.LocalRootPeers\n    Test.Ouroboros.Network.PeerSelection.Cardano.MockEnvironment\n    Test.Ouroboros.Network.PeerSelection.Cardano.PublicRootPeers\n    Test.Ouroboros.Network.PeerSelection.Gource\n    Test.Ouroboros.Network.PeerSelection.Instances\n    Test.Ouroboros.Network.PeerSelection.KnownPeers\n    Test.Ouroboros.Network.PeerSelection.LocalRootPeers\n    Test.Ouroboros.Network.PeerSelection.PeerGraph\n    Test.Ouroboros.Network.PeerSelection.PeerMetric\n    Test.Ouroboros.Network.PeerSelection.RootPeersDNS\n    Test.Ouroboros.Network.PeerSelection.Utils\n    Test.Ouroboros.Network.TxSubmission\n    Test.Ouroboros.Network.Version\n\n  ghc-options:\n    -Wno-unused-packages\n\n-- Simulation tests, and IO tests which don't require native system calls.\n-- (i.e. they don't require system call API provided by `Win32-network` or\n-- `network` dependency).  test-suite sim-tests\ntest-suite sim-tests\n  import: ghc-options-tests\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  type: exitcode-stdio-1.0\n  hs-source-dirs: sim-tests\n  main-is: Main.hs\n  build-depends:\n    base >=4.14 && <4.22,\n    ouroboros-network:testlib,\n    ouroboros-network-protocols:testlib,\n    tasty,\n    with-utf8,\n\n  ghc-options:\n    -fno-ignore-asserts\n    -threaded\n    -rtsopts\n    +RTS\n    -T\n    -RTS\n\n-- Tests which require system calls provided by `Win32-network` or `network`\n-- library.  These tests are compiled natively & run on all supported\n-- platforms: x86_64-w64-mingw32 (Windows), x86_64-linux, x86-64-darwin and\n-- aarch64-darwin.\ntest-suite io-tests\n  import: ghc-options-tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: io-tests\n  main-is: Main.hs\n  other-modules:\n    Test.Ouroboros.Network.Pipe\n    Test.Ouroboros.Network.Socket\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    contra-tracer,\n    io-classes:{io-classes, si-timers, strict-stm},\n    monoidal-synchronisation,\n    network,\n    network-mux,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    ouroboros-network-protocols:testlib,\n    ouroboros-network-testing ^>=0.8.1,\n    serialise,\n    tasty,\n    tasty-quickcheck,\n    with-utf8,\n\n  if os(windows)\n    build-depends:\n      Win32 >=2.5.4.1 && <3.0,\n      Win32-network <0.3.0.0,\n  else\n    build-depends: process\n\n  ghc-options:\n    -threaded\n    -rtsopts\n    +RTS\n    -T\n    -RTS\n\nexecutable demo-chain-sync\n  import: ghc-options\n  hs-source-dirs: demo\n  main-is: chain-sync.hs\n  build-depends:\n    async,\n    base >=4.14 && <4.22,\n    bytestring,\n    containers,\n    contra-tracer,\n    directory,\n    infinite-list,\n    io-classes:{si-timers, strict-stm},\n    network-mux,\n    optparse-applicative,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    random,\n    serialise,\n    typed-protocols,\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -threaded\n    -rtsopts\n\nbenchmark sim-benchmarks\n  import: ghc-options-tests\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench\n  main-is: Main.hs\n  build-depends:\n    base,\n    ouroboros-network:testlib,\n    tasty-bench >=0.3.5,\n\n  ghc-options:\n    -fno-ignore-asserts\n    -with-rtsopts=-A32m\n    +RTS\n    -T\n    -RTS\n\n  -- We use this option to avoid skewed results due to changes in cache-line\n  -- alignment. See\n  -- https://github.com/Bodigrim/tasty-bench#comparison-against-baseline\n  if impl(ghc >=8.6)\n    ghc-options: -fproc-alignment=64\n";
  }