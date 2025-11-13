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
      identifier = { name = "fingertree-rm"; version = "1.0.0.4"; };
      license = "Apache-2.0";
      copyright = "2022-2024 Input Output Global Inc (IOG).";
      maintainer = "operations@iohk.io, Joris Dral (joris@well-typed.com)";
      author = "Joris Dral";
      homepage = "";
      url = "";
      synopsis = "Finger-trees with root measures";
      description = "Finger-trees with root measures.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."monoid-subclasses" or (errorHandler.buildDepError "monoid-subclasses"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."fingertree-rm" or (errorHandler.buildDepError "fingertree-rm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
            (hsPkgs."fingertree-rm" or (errorHandler.buildDepError "fingertree-rm"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/fingertree-rm-1.0.0.4.tar.gz";
      sha256 = "e3515befeefcf670eb54215fb113e60f465c9d434449007b56422b6e4e3efc64";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            fingertree-rm\nversion:         1.0.0.4\nsynopsis:        Finger-trees with root measures\ndescription:     Finger-trees with root measures.\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ncopyright:       2022-2024 Input Output Global Inc (IOG).\nauthor:          Joris Dral\nmaintainer:      operations@iohk.io, Joris Dral (joris@well-typed.com)\ncategory:        Data Structures\nbuild-type:      Simple\ntested-with:\n  GHC ==8.10 || ==9.2 || ==9.4 || ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/anti-diffs\n  subdir:   fingertree-rm\n\nsource-repository this\n  type:     git\n  location: https://github.com/input-output-hk/anti-diffs\n  subdir:   fingertree-rm\n  tag:      fingertree-rm-1.0.0.4\n\ncommon options\n  default-language: Haskell2010\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import:          options\n  hs-source-dirs:  src\n  exposed-modules: Data.FingerTree.RootMeasured.Strict\n  build-depends:\n    , base                       >=4.9 && <4.22\n    , cardano-strict-containers\n    , monoid-subclasses\n    , nothunks\n\ntest-suite test\n  import:           options\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  hs-source-dirs:   test\n  main-is:          Main.hs\n  other-modules:    Test.Data.FingerTree.RootMeasured.Strict\n  build-depends:\n    , base              >=4.9 && <4.22\n    , fingertree-rm\n    , tasty\n    , tasty-quickcheck\n\nbenchmark bench\n  import:           options\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  hs-source-dirs:   bench\n  main-is:          Main.hs\n  other-modules:    Bench.Data.FingerTree.RootMeasured.Strict\n  build-depends:\n    , base                       >=4.9 && <4.22\n    , cardano-strict-containers\n    , deepseq\n    , fingertree\n    , fingertree-rm\n    , QuickCheck\n    , tasty\n    , tasty-bench\n    , tasty-quickcheck\n";
  }