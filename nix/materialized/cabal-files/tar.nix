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
      specVersion = "2.2";
      identifier = { name = "tar"; version = "0.7.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2007 Bjorn Bringert <bjorn@bringert.net>\n2008-2016 Duncan Coutts <duncan@community.haskell.org>";
      maintainer = "Bodigrim <andrew.lelechenko@gmail.com>";
      author = "Duncan Coutts <duncan@community.haskell.org>\nBjorn Bringert <bjorn@bringert.net>";
      homepage = "";
      url = "";
      synopsis = "Reading, writing and manipulating \".tar\" archive files.";
      description = "This library is for working with \\\"@.tar@\\\" archive files. It\ncan read and write a range of common variations of archive\nformat including V7, POSIX USTAR and GNU formats.\n\nIt provides support for packing and unpacking portable\narchives. This makes it suitable for distribution but not\nbackup because details like file ownership and exact\npermissions are not preserved.\n\nIt also provides features for random access to archive\ncontent using an index.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."tar".components.sublibs.tar-internal or (errorHandler.buildDepError "tar:tar-internal"))
        ];
        buildable = true;
      };
      sublibs = {
        "tar-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."directory-ospath-streaming" or (errorHandler.buildDepError "directory-ospath-streaming"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."os-string" or (errorHandler.buildDepError "os-string"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
      };
      tests = {
        "properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."directory-ospath-streaming" or (errorHandler.buildDepError "directory-ospath-streaming"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tar".components.sublibs.tar-internal or (errorHandler.buildDepError "tar:tar-internal"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "9.0") (hsPkgs."bytestring-handle" or (errorHandler.buildDepError "bytestring-handle"));
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tar-0.7.0.0.tar.gz";
      sha256 = "6e4ad12cecf8d945be3777d00a3c13ab80c837797b7a8df1fe853e0621729036";
    });
  }) // {
    package-description-override = "cabal-version:   2.2\r\nname:            tar\r\nversion:         0.7.0.0\r\nx-revision: 2\r\nlicense:         BSD-3-Clause\r\nlicense-file:    LICENSE\r\nauthor:          Duncan Coutts <duncan@community.haskell.org>\r\n                 Bjorn Bringert <bjorn@bringert.net>\r\nmaintainer:      Bodigrim <andrew.lelechenko@gmail.com>\r\nbug-reports:     https://github.com/haskell/tar/issues\r\ncopyright:       2007 Bjorn Bringert <bjorn@bringert.net>\r\n                 2008-2016 Duncan Coutts <duncan@community.haskell.org>\r\ncategory:        Codec\r\nsynopsis:        Reading, writing and manipulating \".tar\" archive files.\r\ndescription:     This library is for working with \\\"@.tar@\\\" archive files. It\r\n                 can read and write a range of common variations of archive\r\n                 format including V7, POSIX USTAR and GNU formats.\r\n                 .\r\n                 It provides support for packing and unpacking portable\r\n                 archives. This makes it suitable for distribution but not\r\n                 backup because details like file ownership and exact\r\n                 permissions are not preserved.\r\n                 .\r\n                 It also provides features for random access to archive\r\n                 content using an index.\r\nbuild-type:      Simple\r\nextra-source-files:\r\n                 test/data/long-filepath.tar\r\n                 test/data/long-symlink.tar\r\n                 test/data/symlink.tar\r\nextra-doc-files: changelog.md\r\n                 README.md\r\ntested-with:     GHC ==9.14.1, GHC==9.12.2, GHC==9.10.2, GHC==9.8.4,\r\n                 GHC==9.6.7, GHC==9.4.8, GHC==9.2.8, GHC==9.0.2,\r\n                 GHC==8.10.7, GHC==8.8.4, GHC==8.6.5\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell/tar.git\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  build-depends: tar-internal\r\n\r\n  reexported-modules:\r\n    Codec.Archive.Tar,\r\n    Codec.Archive.Tar.Entry,\r\n    Codec.Archive.Tar.Check,\r\n    Codec.Archive.Tar.Index\r\n\r\nlibrary tar-internal\r\n  default-language: Haskell2010\r\n  build-depends: base       >= 4.12  && < 5,\r\n                 array                 < 0.6,\r\n                 bytestring >= 0.10 && < 0.13,\r\n                 containers >= 0.2  && < 0.9,\r\n                 deepseq    >= 1.1  && < 1.6,\r\n                 directory  >= 1.3.8.0 && < 1.4,\r\n                 directory-ospath-streaming >= 0.2.1 && < 0.4,\r\n                 file-io                < 0.2,\r\n                 filepath   >= 1.4.100 && < 1.6,\r\n                 os-string  >= 2.0 && < 2.1,\r\n                 time                  < 1.16,\r\n                 transformers          < 0.7,\r\n\r\n  exposed-modules:\r\n    Codec.Archive.Tar\r\n    Codec.Archive.Tar.Entry\r\n    Codec.Archive.Tar.Check\r\n    Codec.Archive.Tar.Check.Internal\r\n    Codec.Archive.Tar.Index\r\n    Codec.Archive.Tar.LongNames\r\n    Codec.Archive.Tar.Types\r\n    Codec.Archive.Tar.Read\r\n    Codec.Archive.Tar.Write\r\n    Codec.Archive.Tar.Pack\r\n    Codec.Archive.Tar.PackAscii\r\n    Codec.Archive.Tar.Unpack\r\n    Codec.Archive.Tar.Index.StringTable\r\n    Codec.Archive.Tar.Index.IntTrie\r\n    Codec.Archive.Tar.Index.Internal\r\n    Codec.Archive.Tar.Index.Utils\r\n\r\n  other-extensions:\r\n    BangPatterns\r\n    CPP\r\n    GeneralizedNewtypeDeriving\r\n    PatternGuards\r\n    ScopedTypeVariables\r\n\r\n  ghc-options: -Wall -fno-warn-unused-imports\r\n\r\ntest-suite properties\r\n  type:          exitcode-stdio-1.0\r\n  default-language: Haskell2010\r\n  build-depends: base < 5,\r\n                 array,\r\n                 bytestring >= 0.10,\r\n                 containers,\r\n                 deepseq,\r\n                 directory >= 1.2,\r\n                 directory-ospath-streaming,\r\n                 file-embed,\r\n                 filepath,\r\n                 QuickCheck       == 2.*,\r\n                 tar-internal,\r\n                 tasty            >= 0.10 && <1.6,\r\n                 tasty-quickcheck >= 0.8  && <1,\r\n                 temporary < 1.4,\r\n                 time\r\n  if impl(ghc < 9.0)\r\n    build-depends: bytestring-handle < 0.2\r\n\r\n  hs-source-dirs: test\r\n\r\n  main-is: Properties.hs\r\n\r\n  other-modules:\r\n    Codec.Archive.Tar.Tests\r\n    Codec.Archive.Tar.Index.Tests\r\n    Codec.Archive.Tar.Index.IntTrie.Tests\r\n    Codec.Archive.Tar.Index.StringTable.Tests\r\n    Codec.Archive.Tar.Pack.Tests\r\n    Codec.Archive.Tar.Types.Tests\r\n    Codec.Archive.Tar.Unpack.Tests\r\n\r\n  other-extensions:\r\n    CPP\r\n    BangPatterns,\r\n    ScopedTypeVariables\r\n\r\n  ghc-options: -fno-ignore-asserts\r\n\r\nbenchmark bench\r\n  type:          exitcode-stdio-1.0\r\n  default-language: Haskell2010\r\n  hs-source-dirs: bench\r\n  main-is:       Main.hs\r\n  build-depends: base < 5,\r\n                 tar,\r\n                 bytestring >= 0.10,\r\n                 directory >= 1.2,\r\n                 temporary < 1.4,\r\n                 tasty-bench >= 0.4 && < 0.5\r\n";
  }