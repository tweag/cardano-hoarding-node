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
      identifier = { name = "cardano-lmdb-simple"; version = "0.8.1.0"; };
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
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
        ];
        buildable = true;
      };
      tests = {
        "test-cursors" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
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
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
            (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
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
      url = "https://chap.intersectmbo.org/package/cardano-lmdb-simple-0.8.1.0.tar.gz";
      sha256 = "7ccba3eb67fb9407e7295982c50cf0d881539a4a9762be22e3d7e4497c6328a7";
    });
  }) // {
    package-description-override = "cabal-version:       3.0\nname:                cardano-lmdb-simple\nversion:             0.8.1.0\nsynopsis:            Simple API for LMDB\ndescription:         This package provides a simple API for using the\n                     Lightning Memory-mapped Database (LMDB).\nhomepage:            https://github.com/input-output-hk/lmdb-simple#readme\nbug-reports:         https://github.com/input-output-hk/lmdb-simple/issues\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Rob Leslie\nmaintainer:          operations@iohk.io\ncopyright:           © 2017–2018 Robert Leslie\nstability:           experimental\ncategory:            Database\ntested-with:         GHC == { 8.10, 9.2, 9.4, 9.6, 9.8, 9.10, 9.12 }\n\nbuild-type:          Simple\nextra-source-files:  README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/lmdb-simple.git\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/lmdb-simple.git\n  tag:      cardano-lmdb-simple-0.8.1.0\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:     Database.LMDB.Simple\n                       Database.LMDB.Simple.Cursor\n                       Database.LMDB.Simple.DBRef\n                       Database.LMDB.Simple.Extra\n                       Database.LMDB.Simple.Internal\n                       Database.LMDB.Simple.TransactionHandle\n                       Database.LMDB.Simple.View\n\n  build-depends:       async\n                     , base >= 4.14 && < 4.22\n                     , bytestring >= 0.10 && < 0.13\n                     , containers\n                     , exceptions\n                     , cardano-lmdb ^>= 0.4\n                     , mtl\n                     , serialise >= 0.2 && < 0.3\n                     , unliftio-core\n  ghc-options:         -Wall -Wno-name-shadowing -Wno-unused-do-bind\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wmissing-export-lists\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n  default-extensions:  Trustworthy\n  other-extensions:    ConstraintKinds\n                       TypeFamilies\n\ntest-suite test-cursors\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-cursors\n  main-is:             Main.hs\n  other-modules:       Test.Database.LMDB.Simple.Cursor\n                       Test.Database.LMDB.Simple.Cursor.Lockstep\n                       Test.Database.LMDB.Simple.Cursor.Lockstep.Mock\n                       Test.Database.LMDB.Simple.TransactionHandle\n  build-depends:       base\n                     , cardano-lmdb\n                     , cardano-lmdb-simple\n                     , containers\n                     , directory\n                     , exceptions\n                     , mtl\n                     , QuickCheck\n                     , quickcheck-dynamic\n                     , quickcheck-lockstep >= 0.2\n                     , serialise\n                     , tasty\n                     , tasty-hunit\n                     , tasty-quickcheck\n                     , temporary\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Main.hs\n  other-modules:       Bench.Database.LMDB.Simple.Cursor\n                       Bench.Utils\n  build-depends:       base\n                     , bytestring\n                     , cardano-lmdb-simple\n                     , containers\n                     , deepseq\n                     , directory\n                     , QuickCheck\n                     , random\n                     , serialise\n                     , tasty\n                     , tasty-bench\n                     , tasty-quickcheck\n                     , temporary\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n                       -with-rtsopts=-A32m -Werror\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n\ntest-suite sample\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             sample.hs\n  build-depends:       base\n                     , cardano-lmdb-simple\n                     , temporary\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n\ntest-suite hspec\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             hspec.hs\n  other-modules:       Database.LMDB.SimpleSpec\n                       Database.LMDB.Simple.DBRefSpec\n                       Harness\n  build-depends:       base\n                     , cardano-lmdb-simple\n                     , directory\n                     , hspec\n                     , QuickCheck\n                     , temporary\n  build-tool-depends:  hspec-discover:hspec-discover\n  ghc-options:         -Wall -Wno-unused-do-bind\n                       -threaded -rtsopts -with-rtsopts=-N\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n\nbenchmark criterion\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             criterion.hs\n  other-modules:       Harness\n  build-depends:       base\n                     , criterion\n                     , cardano-lmdb\n                     , cardano-lmdb-simple\n                     , deepseq\n                     , directory\n                     , temporary\n  ghc-options:         -Wall -Wno-name-shadowing\n                       -threaded -rtsopts -with-rtsopts=-N\n                       -Wno-unticked-promoted-constructors\n  default-language:    Haskell2010\n";
  }