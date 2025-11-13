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
    flags = { ipv6 = false; tracetcpinfo = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "network-mux"; version = "0.9.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Duncan Coutts, Marc Fontaine, Karl Knutsson, Marcin Szamotulski, Alexander Vieth, Neil Davies";
      homepage = "";
      url = "";
      synopsis = "Multiplexing library";
      description = "Multiplexing library.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."statistics-linreg" or (errorHandler.buildDepError "statistics-linreg"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ] ++ pkgs.lib.optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
        ];
        buildable = true;
      };
      exes = {
        "mux-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ] ++ (if system.isWindows
            then [
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            ]
            else [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]);
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [
              (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ]);
          buildable = true;
        };
      };
      benchmarks = {
        "socket-read-write-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/network-mux-0.9.0.0.tar.gz";
      sha256 = "c675ae6f8d6133a73c404feb1af6fbcfdcdd33a54977e88f21567ff5af3f7e1d";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: network-mux\nversion: 0.9.0.0\nsynopsis: Multiplexing library\ndescription: Multiplexing library.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Duncan Coutts, Marc Fontaine, Karl Knutsson, Marcin Szamotulski, Alexander Vieth, Neil Davies\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nflag ipv6\n  description: Enable IPv6 test cases\n  manual: True\n  -- Default to False since travis lacks IPv6 support\n  default: False\n\nflag tracetcpinfo\n  description: Enable costly Linux only tracing of the kernel's tcpinfo\n  manual: True\n  default: False\n\ncommon demo-deps\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -threaded\n    -Wall\n    -fno-ignore-asserts\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  build-depends:\n    -- The Windows version of network-3.1.2 is missing\n    -- functions, see\n    -- https://github.com/haskell/network/issues/484\n    array >=0.5 && <0.6,\n    base >=4.14 && <4.22,\n    binary >=0.8 && <0.11,\n    bytestring >=0.10 && <0.13,\n    containers >=0.5 && <0.8,\n    contra-tracer >=0.1 && <0.2,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8.0.1,\n    monoidal-synchronisation >=0.1 && <0.2,\n    network ^>=3.2.7,\n    process ^>=1.6,\n    quiet,\n    statistics-linreg >=0.3 && <0.4,\n    time >=1.9.1 && <1.14,\n    vector >=0.12 && <0.14,\n\n  if os(windows)\n    build-depends:\n      Win32 >=2.5.4.1 && <3.0,\n      Win32-network >=0.2 && <0.3,\n\n  if flag(tracetcpinfo)\n    cpp-options: -DMUX_TRACE_TCPINFO\n  hs-source-dirs: src\n  exposed-modules:\n    Control.Concurrent.JobPool\n    Network.Mux\n    Network.Mux.Bearer\n    Network.Mux.Bearer.AttenuatedChannel\n    Network.Mux.Bearer.Pipe\n    Network.Mux.Bearer.Queues\n    Network.Mux.Bearer.Socket\n    Network.Mux.Channel\n    Network.Mux.Codec\n    Network.Mux.DeltaQ.TraceStats\n    Network.Mux.DeltaQ.TraceStatsSupport\n    Network.Mux.DeltaQ.TraceTransformer\n    Network.Mux.DeltaQ.TraceTypes\n    Network.Mux.Egress\n    Network.Mux.Ingress\n    Network.Mux.TCPInfo\n    Network.Mux.Time\n    Network.Mux.Timeout\n    Network.Mux.Trace\n    Network.Mux.Types\n\n  if os(linux)\n    other-modules: Network.Mux.TCPInfo.Linux\n\n  if os(windows)\n    exposed-modules:\n      Network.Mux.Bearer.NamedPipe\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Widentities\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wno-unticked-promoted-constructors\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    Test.Mux\n    Test.Mux.ReqResp\n    Test.Mux.Timeout\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    QuickCheck,\n    Win32-network,\n    base >=4.14 && <4.22,\n    binary,\n    bytestring,\n    cborg,\n    containers,\n    contra-tracer,\n    io-classes:{io-classes, si-timers, strict-stm},\n    io-sim,\n    network,\n    network-mux,\n    primitive,\n    serialise,\n    splitmix,\n    tasty,\n    tasty-quickcheck,\n\n  if os(windows)\n    build-depends: Win32 >=2.5.4.1 && <3.0\n  else\n    build-depends: process\n\n  ghc-options:\n    -threaded\n    -rtsopts\n    -Wall\n    -fno-ignore-asserts\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n\n  if flag(ipv6)\n    cpp-options: -DOUROBOROS_NETWORK_IPV6\n\nexecutable mux-demo\n  import: demo-deps\n  hs-source-dirs:\n    demo\n    test\n\n  main-is: mux-demo.hs\n  other-modules: Test.Mux.ReqResp\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    contra-tracer,\n    io-classes,\n    network-mux,\n    primitive,\n    serialise,\n    stm,\n\n  if os(windows)\n    build-depends:\n      Win32,\n      Win32-network,\n  else\n    build-depends:\n      directory,\n      network,\n\nbenchmark socket-read-write-benchmarks\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench/socket_read_write\n  main-is: Main.hs\n  other-modules:\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring,\n    contra-tracer,\n    io-classes:{io-classes, si-timers, strict-stm},\n    network,\n    network-mux,\n    tasty-bench,\n\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -threaded\n    -rtsopts\n    -fproc-alignment=64\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\n  default-language: Haskell2010\n";
  }