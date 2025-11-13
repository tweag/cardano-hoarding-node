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
        name = "ouroboros-consensus-diffusion";
        version = "0.24.0.0";
      };
      license = "Apache-2.0";
      copyright = "2022-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Integration for the Ouroboros Network layer";
      description = "Top level integration for consensus & network layers of the Ouroboros blockchain protocol.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network".components.sublibs.cardano-diffusion or (errorHandler.buildDepError "ouroboros-network:cardano-diffusion"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
          (hsPkgs."safe-wild-cards" or (errorHandler.buildDepError "safe-wild-cards"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
        ];
        buildable = true;
      };
      sublibs = {
        "unstable-diffusion-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-diffusion" or (errorHandler.buildDepError "ouroboros-consensus-diffusion"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
          buildable = true;
        };
        "unstable-mock-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-mock-block or (errorHandler.buildDepError "ouroboros-consensus:unstable-mock-block"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
          ];
          buildable = true;
        };
      };
      tests = {
        "infra-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
          ];
          buildable = true;
        };
        "mock-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-mock-block or (errorHandler.buildDepError "ouroboros-consensus:unstable-mock-block"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-mock-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-mock-testlib"))
          ];
          buildable = true;
        };
        "consensus-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-diffusion" or (errorHandler.buildDepError "ouroboros-consensus-diffusion"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-checked-vars" or (errorHandler.buildDepError "strict-checked-vars"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-consensus-diffusion-0.24.0.0.tar.gz";
      sha256 = "c8c07b590d8c36764e294baa259c41500aa298efe936f9ca00664f40a11bd2fc";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-consensus-diffusion\nversion: 0.24.0.0\nsynopsis: Integration for the Ouroboros Network layer\ndescription:\n  Top level integration for consensus & network layers of the Ouroboros blockchain protocol.\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:\n  2022-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\n\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/ouroboros-consensus\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\ncommon common-lib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\ncommon common-test\n  import: common-lib\n  ghc-options:\n    -threaded\n    -rtsopts\n\nlibrary\n  import: common-lib\n  hs-source-dirs: src/ouroboros-consensus-diffusion\n  exposed-modules:\n    Ouroboros.Consensus.Network.NodeToClient\n    Ouroboros.Consensus.Network.NodeToNode\n    Ouroboros.Consensus.Node\n    Ouroboros.Consensus.Node.DbLock\n    Ouroboros.Consensus.Node.DbMarker\n    Ouroboros.Consensus.Node.Exit\n    Ouroboros.Consensus.Node.ExitPolicy\n    Ouroboros.Consensus.Node.GSM\n    Ouroboros.Consensus.Node.Genesis\n    Ouroboros.Consensus.Node.Recovery\n    Ouroboros.Consensus.Node.RethrowPolicy\n    Ouroboros.Consensus.Node.Tracers\n    Ouroboros.Consensus.NodeKernel\n\n  reexported-modules:\n    Ouroboros.Consensus.Block,\n    Ouroboros.Consensus.Node.NetworkProtocolVersion,\n    Ouroboros.Consensus.Node.Run,\n\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring >=0.10 && <0.13,\n    cardano-slotting,\n    cborg ^>=0.2.2,\n    containers >=0.5 && <0.8,\n    contra-tracer,\n    deepseq,\n    filepath,\n    fs-api ^>=0.4,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8,\n    mtl,\n    network-mux ^>=0.9,\n    ouroboros-consensus ^>=0.28,\n    ouroboros-consensus-protocol ^>=0.13,\n    ouroboros-network:{cardano-diffusion, ouroboros-network} ^>=0.22.1,\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-framework ^>=0.19,\n    ouroboros-network-protocols ^>=0.15,\n    random,\n    resource-registry ^>=0.1,\n    safe-wild-cards ^>=1.0,\n    serialise ^>=0.2,\n    text,\n    time,\n    transformers,\n    typed-protocols:{stateful, typed-protocols},\n\nlibrary unstable-diffusion-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-diffusion-testlib\n  exposed-modules:\n    Test.ThreadNet.General\n    Test.ThreadNet.Network\n    Test.ThreadNet.Ref.PBFT\n    Test.ThreadNet.Rekeying\n    Test.ThreadNet.TxGen\n    Test.ThreadNet.Util\n    Test.ThreadNet.Util.Expectations\n    Test.ThreadNet.Util.HasCreator\n    Test.ThreadNet.Util.NodeJoinPlan\n    Test.ThreadNet.Util.NodeRestarts\n    Test.ThreadNet.Util.NodeToNodeVersion\n    Test.ThreadNet.Util.NodeTopology\n    Test.ThreadNet.Util.Seed\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-core,\n    cborg,\n    containers,\n    contra-tracer,\n    fgl,\n    fs-sim ^>=0.4,\n    graphviz >=2999.20.1.0,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    mtl,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-diffusion,\n    ouroboros-consensus-protocol,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    quiet ^>=0.2,\n    random,\n    resource-registry,\n    sop-core ^>=0.5,\n    sop-extras ^>=0.4,\n    strict-sop-core ^>=0.1,\n    text,\n    typed-protocols,\n\nlibrary unstable-mock-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-mock-testlib\n  exposed-modules:\n    Test.Consensus.Ledger.Mock.Generators\n    Test.ThreadNet.TxGen.Mock\n    Test.ThreadNet.Util.HasCreator.Mock\n    Test.ThreadNet.Util.SimpleBlock\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-tests ^>=2.2,\n    containers,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib, unstable-mock-block},\n    serialise,\n    unstable-diffusion-testlib,\n\ntest-suite infra-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/infra-test\n  main-is: Main.hs\n  other-modules: Test.ThreadNet.Util.Tests\n  build-depends:\n    base,\n    cardano-ledger-core,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    tasty,\n    tasty-quickcheck,\n    unstable-diffusion-testlib,\n\ntest-suite mock-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/mock-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.Ledger.Mock\n    Test.Consensus.Ledger.Mock.LedgerTables\n    Test.ThreadNet.BFT\n    Test.ThreadNet.LeaderSchedule\n    Test.ThreadNet.PBFT\n    Test.ThreadNet.Praos\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-core,\n    cborg,\n    constraints,\n    containers,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib, unstable-mock-block},\n    ouroboros-network-mock,\n    serialise,\n    tasty,\n    tasty-quickcheck,\n    unstable-diffusion-testlib,\n    unstable-mock-testlib,\n\ntest-suite consensus-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/consensus-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.BlockTree\n    Test.Consensus.GSM\n    Test.Consensus.Genesis.Setup\n    Test.Consensus.Genesis.Setup.Classifiers\n    Test.Consensus.Genesis.Setup.GenChains\n    Test.Consensus.Genesis.Tests\n    Test.Consensus.Genesis.Tests.CSJ\n    Test.Consensus.Genesis.Tests.DensityDisconnect\n    Test.Consensus.Genesis.Tests.LoE\n    Test.Consensus.Genesis.Tests.LoE.CaughtUp\n    Test.Consensus.Genesis.Tests.LoP\n    Test.Consensus.Genesis.Tests.LongRangeAttack\n    Test.Consensus.Genesis.Tests.Uniform\n    Test.Consensus.HardFork.Combinator\n    Test.Consensus.HardFork.Combinator.A\n    Test.Consensus.HardFork.Combinator.B\n    Test.Consensus.Network.AnchoredFragment.Extras\n    Test.Consensus.Node\n    Test.Consensus.PeerSimulator.BlockFetch\n    Test.Consensus.PeerSimulator.CSJInvariants\n    Test.Consensus.PeerSimulator.ChainSync\n    Test.Consensus.PeerSimulator.Config\n    Test.Consensus.PeerSimulator.Handlers\n    Test.Consensus.PeerSimulator.NodeLifecycle\n    Test.Consensus.PeerSimulator.Resources\n    Test.Consensus.PeerSimulator.Run\n    Test.Consensus.PeerSimulator.ScheduledBlockFetchServer\n    Test.Consensus.PeerSimulator.ScheduledChainSyncServer\n    Test.Consensus.PeerSimulator.ScheduledServer\n    Test.Consensus.PeerSimulator.StateDiagram\n    Test.Consensus.PeerSimulator.StateView\n    Test.Consensus.PeerSimulator.Tests\n    Test.Consensus.PeerSimulator.Tests.LinkedThreads\n    Test.Consensus.PeerSimulator.Tests.Rollback\n    Test.Consensus.PeerSimulator.Tests.Timeouts\n    Test.Consensus.PeerSimulator.Trace\n    Test.Consensus.PointSchedule\n    Test.Consensus.PointSchedule.NodeState\n    Test.Consensus.PointSchedule.Peers\n    Test.Consensus.PointSchedule.Shrinking\n    Test.Consensus.PointSchedule.Shrinking.Tests\n    Test.Consensus.PointSchedule.SinglePeer\n    Test.Consensus.PointSchedule.SinglePeer.Indices\n    Test.Consensus.PointSchedule.Tests\n    Test.Util.PartialAccessors\n    Test.Util.TersePrinting\n\n  build-depends:\n    QuickCheck,\n    base,\n    binary,\n    bytestring,\n    cardano-crypto-class ^>=2.2,\n    cardano-ledger-core,\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    containers,\n    contra-tracer,\n    directory,\n    fs-api ^>=0.4,\n    fs-sim ^>=0.4,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    mempack,\n    mtl,\n    nothunks,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-diffusion,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    pretty,\n    quickcheck-dynamic ^>=4.0.0,\n    quiet,\n    random,\n    reflection,\n    resource-registry,\n    serialise,\n    sop-core,\n    sop-extras,\n    strict-checked-vars,\n    strict-sop-core,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    temporary,\n    time,\n    tree-diff,\n    typed-protocols,\n    unstable-diffusion-testlib,\n    vector,\n";
  }