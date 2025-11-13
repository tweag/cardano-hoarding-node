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
    flags = { ipv6 = false; };
    package = {
      specVersion = "3.0";
      identifier = {
        name = "ouroboros-network-framework";
        version = "0.19.2.0";
      };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Ouroboros network framework";
      description = "Ouroboros network framework.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
          (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
        ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."quickcheck-monoids" or (errorHandler.buildDepError "quickcheck-monoids"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
          ];
          buildable = true;
        };
      };
      exes = {
        "demo-ping-pong" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
          ];
          buildable = true;
        };
        "demo-connection-manager" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
          ];
          buildable = true;
        };
      };
      tests = {
        "sim-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-framework".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-framework:testlib"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-monoids" or (errorHandler.buildDepError "quickcheck-monoids"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
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
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-framework".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-framework:testlib"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"));
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-framework-0.19.2.0.tar.gz";
      sha256 = "6a89d607db4d1845e036b244db1f8a985301495740f310ff5cf5972077d8cf0e";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-network-framework\nversion: 0.19.2.0\nsynopsis: Ouroboros network framework\ndescription: Ouroboros network framework.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nflag ipv6\n  description: Enable IPv6 test cases\n  manual: True\n  -- Default to False since travis lacks IPv6 support\n  default: False\n\nlibrary\n  exposed-modules:\n    Data.Cache\n    Data.Wedge\n    NoThunks.Class.Orphans\n    Ouroboros.Network.Channel\n    Ouroboros.Network.ConnectionHandler\n    Ouroboros.Network.ConnectionId\n    Ouroboros.Network.ConnectionManager.ConnMap\n    Ouroboros.Network.ConnectionManager.Core\n    Ouroboros.Network.ConnectionManager.State\n    Ouroboros.Network.ConnectionManager.Types\n    Ouroboros.Network.Context\n    Ouroboros.Network.Driver\n    Ouroboros.Network.Driver.Limits\n    Ouroboros.Network.Driver.Simple\n    Ouroboros.Network.Driver.Stateful\n    Ouroboros.Network.IOManager\n    Ouroboros.Network.InboundGovernor\n    Ouroboros.Network.InboundGovernor.InformationChannel\n    Ouroboros.Network.InboundGovernor.State\n    Ouroboros.Network.Mux\n    Ouroboros.Network.MuxMode\n    Ouroboros.Network.Protocol.Handshake\n    Ouroboros.Network.Protocol.Handshake.Client\n    Ouroboros.Network.Protocol.Handshake.Codec\n    Ouroboros.Network.Protocol.Handshake.Server\n    Ouroboros.Network.Protocol.Handshake.Type\n    Ouroboros.Network.Protocol.Handshake.Unversioned\n    Ouroboros.Network.Protocol.Handshake.Version\n    Ouroboros.Network.RawBearer\n    Ouroboros.Network.RethrowPolicy\n    Ouroboros.Network.Server\n    Ouroboros.Network.Server.ConnectionTable\n    Ouroboros.Network.Server.RateLimiting\n    Ouroboros.Network.Server.Simple\n    Ouroboros.Network.Snocket\n    Ouroboros.Network.Socket\n    Simulation.Network.Snocket\n\n  -- other-extensions:\n  build-depends:\n    -- ^ only to derive nothunk instances\n    Win32-network ^>=0.2,\n    base >=4.12 && <4.22,\n    bytestring >=0.10 && <0.13,\n    cardano-strict-containers,\n    cborg >=0.2.1 && <0.3,\n    containers >=0.5 && <0.8,\n    contra-tracer,\n    deepseq,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8.0.1,\n    monoidal-synchronisation ^>=0.1.0.6,\n    network ^>=3.2.7,\n    network-mux ^>=0.9,\n    nothunks,\n    nothunks ^>=0.1.4 || ^>=0.2,\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-testing,\n    psqueues,\n    quiet,\n    random ^>=1.2,\n    text,\n    typed-protocols:{typed-protocols, cborg, stateful} ^>=1.0,\n\n  if os(windows)\n    build-depends: Win32 >=2.5.4.1 && <3.0\n  hs-source-dirs: src\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n\nlibrary testlib\n  visibility: public\n  hs-source-dirs: testlib\n  exposed-modules:\n    Test.Ouroboros.Network.ConnectionManager.Experiments\n    Test.Ouroboros.Network.ConnectionManager.Timeouts\n    Test.Ouroboros.Network.ConnectionManager.Utils\n    Test.Ouroboros.Network.InboundGovernor.Utils\n    Test.Ouroboros.Network.Orphans\n    Test.Ouroboros.Network.RawBearer.Utils\n\n  other-modules:\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    containers,\n    contra-tracer,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    network-mux,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-testing,\n    quickcheck-monoids,\n    random,\n    serialise,\n    typed-protocols:{typed-protocols, examples},\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wno-unused-packages\n\ntest-suite sim-tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: sim-tests\n  other-modules:\n    Test.Ouroboros.Network.ConnectionManager\n    Test.Ouroboros.Network.RateLimiting\n    Test.Ouroboros.Network.RawBearer\n    Test.Ouroboros.Network.Server.Sim\n    Test.Simulation.Network.Snocket\n\n  mixins:\n    QuickCheck hiding (Test.QuickCheck.Monoids)\n\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    containers,\n    contra-tracer,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    monoidal-synchronisation,\n    network-mux,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-framework:testlib,\n    ouroboros-network-testing,\n    pretty-simple,\n    psqueues,\n    quickcheck-instances,\n    quickcheck-monoids,\n    quiet,\n    random,\n    serialise,\n    tasty,\n    tasty-quickcheck,\n    text,\n    typed-protocols:{typed-protocols, cborg, examples},\n    with-utf8,\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -rtsopts\n    -threaded\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wno-unused-packages\n\n  if flag(ipv6)\n    cpp-options: -DOUROBOROS_NETWORK_IPV6\n\ntest-suite io-tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: io-tests\n  other-modules:\n    Test.Ouroboros.Network.Driver\n    Test.Ouroboros.Network.RawBearer\n    Test.Ouroboros.Network.Server.IO\n    Test.Ouroboros.Network.Socket\n\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    bytestring,\n    contra-tracer,\n    directory,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    monoidal-synchronisation,\n    network,\n    network-mux,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    ouroboros-network-framework:testlib,\n    random,\n    tasty,\n    tasty-quickcheck,\n    time,\n    typed-protocols:{typed-protocols, examples, stateful},\n    with-utf8,\n\n  if os(windows)\n    build-depends: Win32-network <0.3\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -rtsopts\n    -threaded\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n\nexecutable demo-ping-pong\n  hs-source-dirs: demo\n  main-is: ping-pong.hs\n  build-depends:\n    async,\n    base >=4.14 && <4.22,\n    bytestring,\n    contra-tracer,\n    directory,\n    network-mux,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    typed-protocols:examples,\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -threaded\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n\nexecutable demo-connection-manager\n  hs-source-dirs: demo\n  main-is: connection-manager.hs\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring,\n    contra-tracer,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-stm},\n    network,\n    network-mux,\n    optparse-applicative,\n    ouroboros-network-api,\n    ouroboros-network-framework,\n    random,\n    typed-protocols:{typed-protocols, examples},\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -threaded\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n";
  }