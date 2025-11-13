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
      specVersion = "1.18";
      identifier = { name = "memory-pool"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/memory-pool";
      url = "";
      synopsis = "Short description";
      description = "Lock-free threadsafe pinned memory pool manager";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."pvar" or (errorHandler.buildDepError "pvar"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."memory-pool" or (errorHandler.buildDepError "memory-pool"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."pvar" or (errorHandler.buildDepError "pvar"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."memory-pool" or (errorHandler.buildDepError "memory-pool"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/memory-pool-0.1.0.0.tar.gz";
      sha256 = "313f52b1d3b7a7402ab5c11ec42822cb70fbd5c138981ad63c7b7d7d5573e346";
    });
  }) // {
    package-description-override = "name:                memory-pool\nversion:             0.1.0.0\nsynopsis:            Short description\ndescription:         Lock-free threadsafe pinned memory pool manager\nhomepage:            https://github.com/input-output-hk/memory-pool\nlicense:             Apache-2.0\nlicense-file:        LICENSE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncategory:            Memory\nbuild-type:          Simple\nextra-source-files:  README.md\nextra-doc-files:     CHANGELOG.md\ncabal-version:       1.18\ntested-with:         GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.8\n                   , GHC == 9.4.8\n                   , GHC == 9.6.6\n                   , GHC == 9.8.2\n                   , GHC == 9.10.1\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     System.Memory.Pool\n\n  other-modules:\n  build-depends:       base >= 4.9 && < 5\n                     , primitive\n                     , pvar >= 1.0\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wincomplete-record-updates\n                       -Wincomplete-uni-patterns\n                       -Wredundant-constraints\n\ntest-suite tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            Main.hs\n  other-modules:      Common\n                    , Test.System.Memory.PoolTests\n  build-depends:      base\n                    , async\n                    , memory-pool\n                    , primitive\n                    , pvar\n                    , random\n                    , reflection\n                    , tasty\n                    , tasty-quickcheck\n                    , tasty-hunit\n                    , QuickCheck\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -fno-warn-orphans\n                      -threaded\n                      -with-rtsopts=-N\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Bench.hs\n  ghc-options:         -Wall\n                       -threaded\n                       -O2\n                       -with-rtsopts=-N\n  build-depends:       base\n                     , criterion\n                     , deepseq\n                     , memory-pool\n                     , unliftio\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/memory-pool\n";
  }