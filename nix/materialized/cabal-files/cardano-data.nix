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
      identifier = { name = "cardano-data"; version = "1.2.4.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/intersectmbo/cardano-ledger";
      url = "";
      synopsis = "Specialized data for Cardano project";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          ];
          buildable = true;
        };
      };
      tests = {
        "cardano-data-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
            (hsPkgs."cardano-data".components.sublibs.testlib or (errorHandler.buildDepError "cardano-data:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-data-1.2.4.1.tar.gz";
      sha256 = "746ea7a335173d35d6da7bd11fbce9518d1418a93f66e6c6b28829af581fd4d3";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-data\nversion: 1.2.4.1\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/intersectmbo/cardano-ledger\nsynopsis: Specialized data for Cardano project\ncategory: Control\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/cardano-data\n\nlibrary\n  exposed-modules:\n    Data.CanonicalMaps\n    Data.ListMap\n    Data.MapExtras\n    Data.MonoTuple\n    Data.OMap.Strict\n    Data.OSet.Strict\n    Data.Pulse\n    Data.Universe\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson >=2.2,\n    base >=4.18 && <5,\n    cardano-ledger-binary >=1.4,\n    cardano-strict-containers >=0.1.2.1,\n    containers,\n    data-default,\n    deepseq,\n    microlens,\n    mtl,\n    nothunks,\n    vector,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Data\n    Test.Cardano.Data.Arbitrary\n    Test.Cardano.Data.TreeDiff\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-data,\n    cardano-ledger-binary:testlib,\n    containers,\n    hspec,\n    microlens,\n\ntest-suite cardano-data-tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Data.MapExtrasSpec\n    Test.Cardano.Data.OMap.StrictSpec\n    Test.Cardano.Data.OSet.StrictSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-data,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-strict-containers,\n    containers,\n    hspec,\n    microlens,\n    quickcheck-classes,\n    testlib,\n";
  }