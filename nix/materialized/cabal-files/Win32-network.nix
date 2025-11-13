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
    flags = { demo = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "Win32-network"; version = "0.2.0.1"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Win32 network API";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
        ] ++ pkgs.lib.optionals (system.isWindows) [
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
        ];
        libs = pkgs.lib.optional (system.isWindows) (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"));
        buildable = true;
      };
      exes = {
        "named-pipe-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ pkgs.lib.optionals (system.isWindows) [
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ pkgs.lib.optionals (system.isWindows) [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/Win32-network-0.2.0.1.tar.gz";
      sha256 = "0e71e603dcdbdfe31201e29ce71a953f5b7344cb8acae676d712928baf1de656";
    });
  }) // {
    package-description-override = "cabal-version:          3.0\nname:                   Win32-network\nversion:                0.2.0.1\nsynopsis:               Win32 network API\nlicense:                Apache-2.0\nlicense-files:          LICENSE NOTICE\nauthor:                 Duncan Coutts, Marcin Szamotulski\nmaintainer:             duncan@well-typed.com, marcin.szamotulski@iohk.io\ncopyright:              2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect\ncategory:               System\nbuild-type:             Simple\nextra-source-files:     README.md\n                        ChangeLog.md\n                        include/Win32-network.h\ntested-with:            GHC == {8.10, 9.2, 9.4, 9.6, 9.8}\n\nsource-repository head\n  type:                 git\n  location:             https://github.com/haskell-works/Win32-network\n\nflag demo\n  description:  Build the named pipe demos\n  default:      False\n\ncommon project-config\n  default-language:     Haskell2010\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-uni-patterns\n                        -Wincomplete-record-updates\n                        -Wpartial-fields\n                        -Widentities\n                        -Wredundant-constraints\n\nlibrary\n  import:               project-config\n  hs-source-dirs:       src\n  exposed-modules:      System.IOManager\n  build-depends:        base >=4.5 && <5\n\n  if os(windows)\n    exposed-modules:    System.Win32.Async\n                        System.Win32.Async.File\n                        System.Win32.Async.ErrCode\n                        System.Win32.Async.Socket\n                        System.Win32.Async.Socket.ByteString\n                        System.Win32.Async.Socket.ByteString.Lazy\n                        System.Win32.Async.Internal\n    other-modules:      System.Win32.Async.IOData\n                        System.Win32.Async.IOManager\n                        System.Win32.Async.Overlapped\n                        System.Win32.Async.Socket.Syscalls\n                        System.Win32.Async.WSABuf\n    build-depends:    , bytestring >=0.10    && <0.13\n                      , network\n                      , Win32      ^>=2.14\n    include-dirs:       include\n    extra-libraries:    ws2_32\n\nexecutable named-pipe-demo\n  import:               project-config\n  hs-source-dirs:       demo\n  main-is:              named-pipe-demo.hs\n  ghc-options:          -threaded\n  build-depends:        base\n  if os(windows)\n    build-depends:      binary\n                      , bytestring\n                      , Win32\n                      , Win32-network\n\ntest-suite test\n  import:               project-config\n  type:                 exitcode-stdio-1.0\n  hs-source-dirs:       test\n  main-is:              Main.hs\n  build-depends:        base\n\n  if os(windows)\n    build-depends:    , async\n                      , binary\n                      , bytestring\n                      , network\n                      , stm\n                      , tasty\n                      , tasty-hunit\n                      , tasty-quickcheck\n                      , QuickCheck\n                      , quickcheck-instances\n                      , Win32\n                      , Win32-network\n\n    other-modules:      Test.Generators\n                        Test.Async.PingPong\n                        Test.Async.Handle\n                        Test.Async.Socket\n\n  ghc-options:          -threaded\n";
  }