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
      identifier = { name = "fs-api"; version = "0.4.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2024 Input Output Global Inc (IOG)";
      maintainer = "operations@iohk.io, Joris Dral (joris@well-typed.com)";
      author = "IOG Engineering Team";
      homepage = "https://github.com/input-output-hk/fs-sim";
      url = "";
      synopsis = "Abstract interface for the file system";
      description = "Abstract interface for the file system.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."safe-wild-cards" or (errorHandler.buildDepError "safe-wild-cards"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unix-bytestring" or (errorHandler.buildDepError "unix-bytestring"))
          ]);
        buildable = true;
      };
      tests = {
        "fs-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fs-api-0.4.0.0.tar.gz";
      sha256 = "b5018aaf7c037cbcfedc425f97f2c9e7baf71dbf8f0d7d8b77a252f718e989ab";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            fs-api\nversion:         0.4.0.0\nsynopsis:        Abstract interface for the file system\ndescription:     Abstract interface for the file system.\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:       2019-2024 Input Output Global Inc (IOG)\nauthor:          IOG Engineering Team\nmaintainer:      operations@iohk.io, Joris Dral (joris@well-typed.com)\nhomepage:        https://github.com/input-output-hk/fs-sim\nbug-reports:     https://github.com/input-output-hk/fs-sim/issues\ncategory:        System\nbuild-type:      Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ntested-with:     GHC ==9.2 || ==9.4 || ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/fs-sim\n  subdir:   fs-api\n\nsource-repository this\n  type:     git\n  location: https://github.com/input-output-hk/fs-sim\n  subdir:   fs-api\n  tag:      fs-api-0.4.0.0\n\nlibrary\n  hs-source-dirs:   src\n  exposed-modules:\n    System.FS.API\n    System.FS.API.Lazy\n    System.FS.API.Strict\n    System.FS.API.Types\n    System.FS.CallStack\n    System.FS.Condense\n    System.FS.CRC\n    System.FS.IO\n    System.FS.IO.Handle\n\n  default-language: Haskell2010\n  build-depends:\n    , base             >=4.16  && <4.22\n    , bytestring       ^>=0.10 || ^>=0.11 || ^>=0.12\n    , containers       ^>=0.5  || ^>=0.6  || ^>=0.7\n    , deepseq          ^>=1.4  || ^>=1.5\n    , digest           ^>=0.0\n    , directory        ^>=1.3\n    , filepath         ^>=1.4  || ^>=1.5\n    , io-classes       ^>=1.6  || ^>=1.7  || ^>=1.8.0.1\n    , primitive        ^>=0.9\n    , safe-wild-cards  ^>=1.0\n    , text             ^>=1.2  || ^>=2.0  || ^>=2.1\n\n  if os(windows)\n    hs-source-dirs:  src-win32\n    exposed-modules: System.FS.IO.Windows\n    build-depends:   Win32 ^>=2.14\n\n  -- every other distribution is handled like it is Unix-based\n  else\n    hs-source-dirs:  src-unix\n    exposed-modules: System.FS.IO.Unix\n    build-depends:\n      , unix             ^>=2.7 || ^>=2.8\n      , unix-bytestring  ^>=0.4\n\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n\ntest-suite fs-api-test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          Main.hs\n  other-modules:\n    Test.System.FS.API.FsPath\n    Test.System.FS.IO\n\n  default-language: Haskell2010\n  build-depends:\n    , base\n    , bytestring\n    , filepath\n    , fs-api\n    , primitive\n    , tasty\n    , tasty-quickcheck\n    , temporary\n    , text\n\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n    -fno-ignore-asserts\n";
  }