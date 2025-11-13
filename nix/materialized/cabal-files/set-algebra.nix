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
      identifier = { name = "set-algebra"; version = "1.1.0.4"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/intersectmbo/cardano-ledger";
      url = "";
      synopsis = "Set Algebra";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/set-algebra-1.1.0.4.tar.gz";
      sha256 = "31ed540e7644f3206351a67ff68705bb4e312e7d7715ef88f33532b3f23650a1";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: set-algebra\nversion: 1.1.0.4\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/intersectmbo/cardano-ledger\nsynopsis: Set Algebra\ncategory: Control\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/set-algebra\n\nlibrary\n  exposed-modules:\n    Control.Iterate.BaseTypes\n    Control.Iterate.Collect\n    Control.Iterate.Exp\n    Control.Iterate.SetAlgebra\n    Control.SetAlgebra\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    base >=4.18 && <5,\n    cardano-data >=1.1,\n    containers,\n    prettyprinter >=1.7,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Control.Iterate.RelationReference\n    Test.Control.Iterate.SetAlgebra\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n\n  build-depends:\n    base,\n    cardano-data,\n    containers,\n    set-algebra,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n";
  }