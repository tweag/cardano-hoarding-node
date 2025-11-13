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
      identifier = { name = "vector-map"; version = "1.1.0.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/intersectmbo/cardano-ledger";
      url = "";
      synopsis = "An efficient VMap that is backed by two vectors: one for keys and another for values.";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."quickcheck-classes-base" or (errorHandler.buildDepError "quickcheck-classes-base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/vector-map-1.1.0.1.tar.gz";
      sha256 = "320e23814d07d91e53b950292a7e2c3d3a7dadd4bd05968c5ffe0916bfcfef15";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: vector-map\nversion: 1.1.0.1\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/intersectmbo/cardano-ledger\nsynopsis:\n  An efficient VMap that is backed by two vectors: one for keys and another for values.\n\ncategory: Control\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/vector-map\n\nlibrary\n  exposed-modules: Data.VMap\n  hs-source-dirs: src\n  other-modules: Data.VMap.KVVector\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.18 && <5,\n    containers,\n    deepseq,\n    nothunks,\n    primitive,\n    vector,\n    vector-algorithms,\n\nlibrary testlib\n  exposed-modules: Test.Data.VMap.TreeDiff\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    base,\n    tree-diff,\n    vector,\n    vector-map,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Common\n    Test.VMap\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n\n  build-depends:\n    QuickCheck,\n    base,\n    containers,\n    quickcheck-classes-base,\n    tasty,\n    tasty-quickcheck,\n    vector-map,\n\nbenchmark bench\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n  hs-source-dirs: bench\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -threaded\n    -O2\n    -rtsopts\n\n  build-depends:\n    base,\n    containers,\n    criterion,\n    random,\n    vector-map,\n";
  }