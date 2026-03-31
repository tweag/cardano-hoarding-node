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
      identifier = { name = "rawlock"; version = "0.1.2.0"; };
      license = "Apache-2.0";
      copyright = "2024 INTERSECT";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "A writer-biased RAW lock.";
      description = "A writer-biased RAW lock.\n\nIt allows for multiple readers to run concurrently with at most one\nappender, or a single writer running on isolation.\n\nThe code is safe in the presence of async exceptions, meaning that each\nactor will cleanup after itself if an exception is received.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
        ];
        buildable = true;
      };
      tests = {
        "rawlock-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."rawlock" or (errorHandler.buildDepError "rawlock"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/rawlock-0.1.2.0.tar.gz";
      sha256 = "d1694d3cd83b750c8b0efcf2b34d35a8314f192f6c81c4dfcb0f1d0797163c5f";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\r\nname: rawlock\r\nversion: 0.1.2.0\r\nsynopsis: A writer-biased RAW lock.\r\ndescription:\r\n  A writer-biased RAW lock.\r\n\r\n  It allows for multiple readers to run concurrently with at most one\r\n  appender, or a single writer running on isolation.\r\n\r\n  The code is safe in the presence of async exceptions, meaning that each\r\n  actor will cleanup after itself if an exception is received.\r\n\r\nlicense: Apache-2.0\r\nlicense-files:\r\n  LICENSE\r\n  NOTICE\r\n\r\ncopyright: 2024 INTERSECT\r\nauthor: IOG Engineering Team\r\nmaintainer: operations@iohk.io\r\ncategory: Concurrency\r\nbuild-type: Simple\r\nextra-doc-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\nbug-reports: https://github.com/IntersectMBO/io-classes-extra/issues\r\ntested-with: ghc ==9.6 || ==9.8 || ==9.10 || ==9.12\r\n\r\nx-revision: 1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/IntersectMBO/io-classes-extra\r\n  subdir: rawlock\r\n\r\nsource-repository this\r\n  type: git\r\n  location: https://github.com/IntersectMBO/io-classes-extra\r\n  subdir: rawlock\r\n  tag: rawlock-0.1.2.0\r\n\r\ncommon warnings\r\n  ghc-options:\r\n    -Wall\r\n    -Wunused-packages\r\n    -Wcompat\r\n    -Wincomplete-uni-patterns\r\n    -Wincomplete-record-updates\r\n    -Wpartial-fields\r\n    -Widentities\r\n    -Wredundant-constraints\r\n    -Wmissing-export-lists\r\n    -Wno-unticked-promoted-constructors\r\n\r\nlibrary\r\n  import: warnings\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n  exposed-modules: Control.RAWLock\r\n  default-extensions: ImportQualifiedPost\r\n  build-depends:\r\n    base >=4.18 && <4.23,\r\n    io-classes:{io-classes, strict-mvar, strict-stm} ^>=1.8 || ^>=1.9,\r\n    nothunks ^>=0.2 || ^>=0.3,\r\n\r\ntest-suite rawlock-test\r\n  import: warnings\r\n  default-language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: test\r\n  main-is: Main.hs\r\n  build-depends:\r\n    QuickCheck,\r\n    base,\r\n    io-classes:{io-classes, strict-stm},\r\n    io-sim,\r\n    mtl,\r\n    rawlock,\r\n    tasty,\r\n    tasty-quickcheck,\r\n";
  }