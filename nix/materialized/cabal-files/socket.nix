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
      specVersion = "1.10";
      identifier = { name = "socket"; version = "0.8.3.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "info@lars-petersen.net";
      author = "Lars Petersen";
      homepage = "https://github.com/lpeterse/haskell-socket";
      url = "";
      synopsis = "An extensible socket library.";
      description = "This library is a minimal cross-platform interface for\nBSD style networking.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        ];
        libs = pkgs.lib.optional (system.isWindows) (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"));
        build-tools = [
          (hsPkgs.pkgsBuildBuild.hsc2hs.components.exes.hsc2hs or (pkgs.pkgsBuildBuild.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
        ];
        buildable = true;
      };
      tests = {
        "default" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."socket" or (errorHandler.buildDepError "socket"))
          ];
          buildable = true;
        };
        "threaded" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."socket" or (errorHandler.buildDepError "socket"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/socket-0.8.3.0.tar.gz";
      sha256 = "796573319d7381691e84c58aec601e94c084013d3cca61d9ae91fe5b0dcfa03d";
    });
  }) // {
    package-description-override = "name:                socket\r\nversion:             0.8.3.0\r\nx-revision: 2\r\nsynopsis:            An extensible socket library.\r\ndescription:\r\n  This library is a minimal cross-platform interface for\r\n  BSD style networking.\r\n\r\nlicense:             MIT\r\nlicense-file:        LICENSE\r\nauthor:              Lars Petersen\r\nmaintainer:          info@lars-petersen.net\r\ncategory:            System, Network\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\nhomepage:            https://github.com/lpeterse/haskell-socket\r\nbug-reports:         https://github.com/lpeterse/haskell-socket/issues\r\ntested-with:         GHC==8.0.2, GHC==8.2.2, GHC==8.4.3, GHC==8.8.3\r\nextra-source-files:  README.md\r\n                     CHANGELOG.md\r\n                     CONTRIBUTORS.txt\r\n                     platform/linux/src/System/Socket/Internal/Platform.hsc\r\n                     platform/linux/include/hs_socket.h\r\n                     platform/linux/cbits/hs_socket.c\r\n                     platform/win32/src/System/Socket/Internal/Platform.hsc\r\n                     platform/win32/include/hs_socket.h\r\n                     platform/win32/cbits/hs_socket.c\r\n\r\nlibrary\r\n  exposed-modules:     System.Socket\r\n                     , System.Socket.Family.Inet\r\n                     , System.Socket.Family.Inet6\r\n                     , System.Socket.Type.Raw\r\n                     , System.Socket.Type.Datagram\r\n                     , System.Socket.Type.Stream\r\n                     , System.Socket.Type.SequentialPacket\r\n                     , System.Socket.Protocol.UDP\r\n                     , System.Socket.Protocol.TCP\r\n                     , System.Socket.Protocol.Default\r\n                     , System.Socket.Unsafe\r\n  other-modules:       System.Socket.Internal.Socket\r\n                     , System.Socket.Internal.SocketOption\r\n                     , System.Socket.Internal.Exception\r\n                     , System.Socket.Internal.Message\r\n                     , System.Socket.Internal.AddressInfo\r\n                     , System.Socket.Internal.Platform\r\n  build-depends:       base >= 4.8 && < 5\r\n                     , bytestring < 0.13\r\n  hs-source-dirs:      src\r\n  build-tools:         hsc2hs\r\n  default-language:    Haskell2010\r\n  install-includes:    hs_socket.h\r\n  ghc-options:         -Wall\r\n  if os(windows)\r\n    include-dirs:      platform/win32/include\r\n    hs-source-dirs:    platform/win32/src\r\n    c-sources:         platform/win32/cbits/hs_socket.c\r\n    extra-libraries:   ws2_32\r\n  else\r\n    include-dirs:      platform/linux/include\r\n    hs-source-dirs:    platform/linux/src\r\n    c-sources:         platform/linux/cbits/hs_socket.c\r\n\r\ntest-suite default\r\n  default-language:\r\n    Haskell2010\r\n  type:\r\n    exitcode-stdio-1.0\r\n  hs-source-dirs:\r\n    test\r\n  main-is:\r\n    test.hs\r\n  build-depends:\r\n      base >= 4.7 && < 5\r\n    , tasty\r\n    , tasty-hunit\r\n    , tasty-quickcheck\r\n    , QuickCheck >= 2.9\r\n    , async\r\n    , bytestring\r\n    , socket\r\n\r\ntest-suite threaded\r\n  default-language:\r\n    Haskell2010\r\n  type:\r\n    exitcode-stdio-1.0\r\n  hs-source-dirs:\r\n    test\r\n  main-is:\r\n    test.hs\r\n  ghc-options:\r\n    -threaded\r\n  build-depends:\r\n      base >= 4.7 && < 5\r\n    , tasty\r\n    , tasty-hunit\r\n    , tasty-quickcheck\r\n    , QuickCheck >= 2.9\r\n    , async\r\n    , bytestring\r\n    , socket\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/lpeterse/haskell-socket.git\r\n";
  }