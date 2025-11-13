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
    package-description-override = "cabal-version: 3.0\nname: rawlock\nversion: 0.1.2.0\nsynopsis: A writer-biased RAW lock.\ndescription:\n  A writer-biased RAW lock.\n\n  It allows for multiple readers to run concurrently with at most one\n  appender, or a single writer running on isolation.\n\n  The code is safe in the presence of async exceptions, meaning that each\n  actor will cleanup after itself if an exception is received.\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2024 INTERSECT\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Concurrency\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nbug-reports: https://github.com/IntersectMBO/io-classes-extra/issues\ntested-with: ghc ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: rawlock\n\nsource-repository this\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: rawlock\n  tag: rawlock-0.1.2.0\n\ncommon warnings\n  ghc-options:\n    -Wall\n    -Wunused-packages\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import: warnings\n  hs-source-dirs: src\n  default-language: Haskell2010\n  exposed-modules: Control.RAWLock\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    base >=4.18 && <4.22,\n    io-classes:{io-classes, strict-mvar, strict-stm} ^>=1.8,\n    nothunks ^>=0.2,\n\ntest-suite rawlock-test\n  import: warnings\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  build-depends:\n    QuickCheck,\n    base,\n    io-classes:{io-classes, strict-stm},\n    io-sim,\n    mtl,\n    rawlock,\n    tasty,\n    tasty-quickcheck,\n";
  }