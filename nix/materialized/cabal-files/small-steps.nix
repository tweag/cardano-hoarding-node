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
    flags = { sts_assert = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "small-steps"; version = "1.1.2.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/intersectmbo/cardano-ledger";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/small-steps-1.1.2.0.tar.gz";
      sha256 = "1f738739ea610a6b25108c4f1b7a17290cf5bf3b8bb5387ec66eecaad468e39b";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: small-steps\nversion: 1.1.2.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/intersectmbo/cardano-ledger\nsynopsis: Small step semantics\ncategory: Control\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/small-steps\n\nflag sts_assert\n  description: Enable STS assertions by default\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Control.Provenance\n    Control.State.Transition\n    Control.State.Transition.Extended\n    Control.State.Transition.Simple\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.18 && <5,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    free,\n    mtl,\n    nothunks,\n    text,\n    transformers >=0.5,\n    validation-selective,\n\n  if flag(sts_assert)\n    cpp-options: -DSTS_ASSERT\n\nlibrary testlib\n  exposed-modules:\n    Test.Control.State.Transition.Generator\n    Test.Control.State.Transition.Invalid.Trace\n    Test.Control.State.Transition.Trace\n    Test.Control.State.Transition.Trace.Generator.QuickCheck\n    Test.Hedgehog.Extra.Manual\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    QuickCheck,\n    base >=4.18 && <5,\n    cardano-ledger-binary:testlib >=1.4,\n    cardano-strict-containers,\n    deepseq,\n    hedgehog >=1.0.4,\n    microlens,\n    microlens-th,\n    mtl,\n    nothunks,\n    small-steps >=1.0,\n    tasty-hunit,\n    transformers >=0.5,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Control.State.Transition.Examples.CommitReveal\n    Test.Control.State.Transition.Examples.GlobalSum\n    Test.Control.State.Transition.Examples.Sum\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n\n  build-depends:\n    QuickCheck,\n    Unique,\n    base,\n    cardano-crypto-class,\n    cardano-ledger-binary,\n    containers,\n    hedgehog >=1.0.4,\n    hspec,\n    mtl,\n    small-steps,\n    testlib,\n";
  }