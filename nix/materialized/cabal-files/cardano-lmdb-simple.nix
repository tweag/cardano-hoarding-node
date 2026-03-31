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
      identifier = { name = "cardano-lmdb-simple"; version = "0.8.1.1"; };
      license = "BSD-3-Clause";
      copyright = "© 2017–2018 Robert Leslie";
      maintainer = "operations@iohk.io";
      author = "Rob Leslie";
      homepage = "https://github.com/input-output-hk/lmdb-simple#readme";
      url = "";
      synopsis = "Simple API for LMDB";
      description = "This package provides a simple API for using the\nLightning Memory-mapped Database (LMDB).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
        ];
        buildable = true;
      };
      tests = {
        "test-cursors" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
            (hsPkgs."quickcheck-lockstep" or (errorHandler.buildDepError "quickcheck-lockstep"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "sample" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "hspec" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "criterion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-lmdb-simple-0.8.1.1.tar.gz";
      sha256 = "3193710adde0b550defa34e073c0c4bc6e9ed82a1aa5e72676e0f9a8392d83ae";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-lmdb-simple\nversion: 0.8.1.1\nsynopsis: Simple API for LMDB\ndescription:\n  This package provides a simple API for using the\n  Lightning Memory-mapped Database (LMDB).\n\nhomepage: https://github.com/input-output-hk/lmdb-simple#readme\nbug-reports: https://github.com/input-output-hk/lmdb-simple/issues\nlicense: BSD-3-Clause\nlicense-file: LICENSE\nauthor: Rob Leslie\nmaintainer: operations@iohk.io\ncopyright: © 2017–2018 Robert Leslie\nstability: experimental\ncategory: Database\ntested-with: ghc =={8.10, 9.2, 9.4, 9.6, 9.8, 9.10, 9.12}\nbuild-type: Simple\nextra-source-files: README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/lmdb-simple.git\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/lmdb-simple.git\n  tag: cardano-lmdb-simple-0.8.1.1\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules:\n    Database.LMDB.Simple\n    Database.LMDB.Simple.Cursor\n    Database.LMDB.Simple.DBRef\n    Database.LMDB.Simple.Extra\n    Database.LMDB.Simple.Internal\n    Database.LMDB.Simple.TransactionHandle\n    Database.LMDB.Simple.View\n\n  build-depends:\n    async,\n    base >=4.14 && <4.23,\n    bytestring >=0.10 && <0.13,\n    cardano-lmdb ^>=0.4,\n    containers,\n    exceptions,\n    mtl,\n    serialise >=0.2 && <0.3,\n    unliftio-core,\n\n  ghc-options:\n    -Wall\n    -Wno-name-shadowing\n    -Wno-unused-do-bind\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n  default-extensions: Trustworthy\n  other-extensions:\n    ConstraintKinds\n    TypeFamilies\n\ntest-suite test-cursors\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test-cursors\n  main-is: Main.hs\n  other-modules:\n    Test.Database.LMDB.Simple.Cursor\n    Test.Database.LMDB.Simple.Cursor.Lockstep\n    Test.Database.LMDB.Simple.Cursor.Lockstep.Mock\n    Test.Database.LMDB.Simple.TransactionHandle\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-lmdb,\n    cardano-lmdb-simple,\n    containers,\n    directory,\n    exceptions,\n    mtl,\n    quickcheck-dynamic,\n    quickcheck-lockstep >=0.8,\n    serialise,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    temporary,\n\n  ghc-options:\n    -Wall\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n\nbenchmark bench\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench\n  main-is: Main.hs\n  other-modules:\n    Bench.Database.LMDB.Simple.Cursor\n    Bench.Utils\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-lmdb-simple,\n    containers,\n    deepseq,\n    directory,\n    random,\n    serialise,\n    tasty,\n    tasty-bench,\n    tasty-quickcheck,\n    temporary,\n\n  ghc-options:\n    -Wall\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    -with-rtsopts=-A32m\n    -Werror\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n\ntest-suite sample\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: sample.hs\n  build-depends:\n    base,\n    cardano-lmdb-simple,\n    temporary,\n\n  ghc-options:\n    -Wall\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n\ntest-suite hspec\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: hspec.hs\n  other-modules:\n    Database.LMDB.Simple.DBRefSpec\n    Database.LMDB.SimpleSpec\n    Harness\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-lmdb-simple,\n    directory,\n    hspec,\n    temporary,\n\n  build-tool-depends: hspec-discover:hspec-discover\n  ghc-options:\n    -Wall\n    -Wno-unused-do-bind\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n\nbenchmark criterion\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: criterion.hs\n  other-modules: Harness\n  build-depends:\n    base,\n    cardano-lmdb,\n    cardano-lmdb-simple,\n    criterion,\n    deepseq,\n    directory,\n    temporary,\n\n  ghc-options:\n    -Wall\n    -Wno-name-shadowing\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n    -Wno-unticked-promoted-constructors\n\n  default-language: Haskell2010\n";
  }