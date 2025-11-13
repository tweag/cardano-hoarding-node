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
    flags = { asserts = false; cddl = true; };
    package = {
      specVersion = "3.0";
      identifier = {
        name = "ouroboros-network-protocols";
        version = "0.15.0.0";
      };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Ouroboros Network Protocols";
      description = "Ouroboros Network Protocols.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
          (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          (hsPkgs."typed-protocols".components.sublibs.stateful-cborg or (errorHandler.buildDepError "typed-protocols:stateful-cborg"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
        "cddl" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          ];
          buildable = if flags.cddl then true else false;
        };
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-protocols-0.15.0.0.tar.gz";
      sha256 = "584240d92f1edc1ec1a1d6b1230c0aa6e8008ee2fb1aaefc968e20d950c789ce";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-network-protocols\nversion: 0.15.0.0\nsynopsis: Ouroboros Network Protocols\ndescription: Ouroboros Network Protocols.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\nflag cddl\n  description: Enable CDDL based tests of the CBOR encoding\n  manual: True\n  -- These tests need the cddl and the cbor-diag Ruby-package\n  default: True\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/ouroboros-network\n\nlibrary\n  hs-source-dirs: src\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:\n    Ouroboros.Network.Protocol.BlockFetch.Client\n    Ouroboros.Network.Protocol.BlockFetch.Codec\n    Ouroboros.Network.Protocol.BlockFetch.Server\n    Ouroboros.Network.Protocol.BlockFetch.Type\n    Ouroboros.Network.Protocol.ChainSync.Client\n    Ouroboros.Network.Protocol.ChainSync.ClientPipelined\n    Ouroboros.Network.Protocol.ChainSync.Codec\n    Ouroboros.Network.Protocol.ChainSync.PipelineDecision\n    Ouroboros.Network.Protocol.ChainSync.Server\n    Ouroboros.Network.Protocol.ChainSync.Type\n    Ouroboros.Network.Protocol.KeepAlive.Client\n    Ouroboros.Network.Protocol.KeepAlive.Codec\n    Ouroboros.Network.Protocol.KeepAlive.Server\n    Ouroboros.Network.Protocol.KeepAlive.Type\n    Ouroboros.Network.Protocol.LocalStateQuery.Client\n    Ouroboros.Network.Protocol.LocalStateQuery.Codec\n    Ouroboros.Network.Protocol.LocalStateQuery.Server\n    Ouroboros.Network.Protocol.LocalStateQuery.Type\n    Ouroboros.Network.Protocol.LocalTxMonitor.Client\n    Ouroboros.Network.Protocol.LocalTxMonitor.Codec\n    Ouroboros.Network.Protocol.LocalTxMonitor.Server\n    Ouroboros.Network.Protocol.LocalTxMonitor.Type\n    Ouroboros.Network.Protocol.LocalTxSubmission.Client\n    Ouroboros.Network.Protocol.LocalTxSubmission.Codec\n    Ouroboros.Network.Protocol.LocalTxSubmission.Server\n    Ouroboros.Network.Protocol.LocalTxSubmission.Type\n    Ouroboros.Network.Protocol.PeerSharing.Client\n    Ouroboros.Network.Protocol.PeerSharing.Codec\n    Ouroboros.Network.Protocol.PeerSharing.Server\n    Ouroboros.Network.Protocol.PeerSharing.Type\n    Ouroboros.Network.Protocol.TxSubmission2.Client\n    Ouroboros.Network.Protocol.TxSubmission2.Codec\n    Ouroboros.Network.Protocol.TxSubmission2.Server\n    Ouroboros.Network.Protocol.TxSubmission2.Type\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-extensions:\n    BangPatterns\n    DataKinds\n    EmptyCase\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTSyntax\n    GADTs\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    NamedFieldPuns\n    OverloadedStrings\n    PolyKinds\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    TemplateHaskell\n    TupleSections\n    TypeApplications\n    TypeFamilies\n    TypeInType\n\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring >=0.10 && <0.13,\n    cborg >=0.2.1 && <0.3,\n    containers,\n    deepseq,\n    io-classes:{io-classes, si-timers} ^>=1.8.0.1,\n    nothunks,\n    ouroboros-network-api ^>=0.15 || ^>=0.16,\n    quiet,\n    random,\n    serialise,\n    singletons,\n    text,\n    typed-protocols:{typed-protocols, cborg, stateful, stateful-cborg} ^>=1.0,\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary testlib\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  exposed-modules:\n    Ouroboros.Network.Protocol.BlockFetch.Codec.CDDL\n    Ouroboros.Network.Protocol.BlockFetch.Direct\n    Ouroboros.Network.Protocol.BlockFetch.Examples\n    Ouroboros.Network.Protocol.BlockFetch.Test\n    Ouroboros.Network.Protocol.ChainSync.Codec.CDDL\n    Ouroboros.Network.Protocol.ChainSync.Direct\n    Ouroboros.Network.Protocol.ChainSync.DirectPipelined\n    Ouroboros.Network.Protocol.ChainSync.Examples\n    Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined\n    Ouroboros.Network.Protocol.ChainSync.Test\n    Ouroboros.Network.Protocol.Handshake.Direct\n    Ouroboros.Network.Protocol.Handshake.Test\n    Ouroboros.Network.Protocol.KeepAlive.Direct\n    Ouroboros.Network.Protocol.KeepAlive.Examples\n    Ouroboros.Network.Protocol.KeepAlive.Test\n    Ouroboros.Network.Protocol.LocalStateQuery.Codec.CDDL\n    Ouroboros.Network.Protocol.LocalStateQuery.Direct\n    Ouroboros.Network.Protocol.LocalStateQuery.Examples\n    Ouroboros.Network.Protocol.LocalStateQuery.Test\n    Ouroboros.Network.Protocol.LocalTxMonitor.Codec.CDDL\n    Ouroboros.Network.Protocol.LocalTxMonitor.Direct\n    Ouroboros.Network.Protocol.LocalTxMonitor.Examples\n    Ouroboros.Network.Protocol.LocalTxMonitor.Test\n    Ouroboros.Network.Protocol.LocalTxSubmission.Codec.CDDL\n    Ouroboros.Network.Protocol.LocalTxSubmission.Direct\n    Ouroboros.Network.Protocol.LocalTxSubmission.Examples\n    Ouroboros.Network.Protocol.LocalTxSubmission.Test\n    Ouroboros.Network.Protocol.PeerSharing.Codec.CDDL\n    Ouroboros.Network.Protocol.PeerSharing.Direct\n    Ouroboros.Network.Protocol.PeerSharing.Examples\n    Ouroboros.Network.Protocol.PeerSharing.Test\n    Ouroboros.Network.Protocol.TxSubmission2.Codec.CDDL\n    Ouroboros.Network.Protocol.TxSubmission2.Direct\n    Ouroboros.Network.Protocol.TxSubmission2.Examples\n    Ouroboros.Network.Protocol.TxSubmission2.Test\n    Test.ChainGenerators\n    Test.ChainProducerState\n    Test.Data.CDDL\n    Test.Data.PipeliningDepth\n    Test.Ouroboros.Network.Protocol.Utils\n\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    cardano-slotting:testlib ^>=0.2.0,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    contra-tracer,\n    deepseq,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    network,\n    network-mux,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    ouroboros-network-testing,\n    pipes,\n    quickcheck-instances,\n    serialise,\n    tasty,\n    tasty-quickcheck,\n    text,\n    typed-protocols:{typed-protocols, stateful},\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  -- TODO: these two tests should be moved to `ouroboros-network-mock`\n  other-modules:\n    Test.AnchoredFragment\n    Test.Chain\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    QuickCheck ^>=2.16,\n    base >=4.14 && <4.22,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    ouroboros-network-protocols:testlib,\n    ouroboros-network-testing ^>=0.8,\n    tasty,\n    tasty-quickcheck,\n\n  ghc-options:\n    -threaded\n    -Wall\n    -Wunused-packages\n    -rtsopts\n\ntest-suite cddl\n  type: exitcode-stdio-1.0\n  hs-source-dirs: cddl\n  main-is: Main.hs\n\n  if flag(cddl)\n    buildable: True\n  else\n    buildable: False\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    containers,\n    directory,\n    filepath,\n    mtl,\n    network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    ouroboros-network-protocols:testlib,\n    process-extras,\n    quickcheck-instances,\n    serialise,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    temporary,\n    text,\n    typed-protocols:{typed-protocols, stateful},\n\n  ghc-options:\n    -threaded\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wunused-packages\n    -rtsopts\n    -with-rtsopts=-M400m\n\ntest-suite bench\n  type: exitcode-stdio-1.0\n  default-extensions: ImportQualifiedPost\n  hs-source-dirs: bench-cddl\n  main-is: Main.hs\n  default-language: Haskell2010\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    containers,\n    deepseq,\n    network,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-protocols,\n    ouroboros-network-protocols:testlib,\n    tasty-bench,\n    typed-protocols:{typed-protocols, stateful},\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wunused-packages\n    -rtsopts\n    -with-rtsopts=-A32m\n    -with-rtsopts=-T\n\n  -- Important for comparing benchmarks results against a baseline run.\n  -- Read: https://hackage.haskell.org/package/tasty-bench for details\n  if impl(ghc >=8.6)\n    ghc-options: -fproc-alignment=64\n";
  }