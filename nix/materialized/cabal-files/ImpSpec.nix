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
      specVersion = "2.4";
      identifier = { name = "ImpSpec"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/ImpSpec";
      url = "";
      synopsis = "Imperative approach to testing stateful applications.\nImpSpec is built on top of HSpec and QuickCheck.";
      description = "Let a little imp help you discover all the bugs in your stateful Haskell program.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
          (hsPkgs."hspec-expectations-lifted" or (errorHandler.buildDepError "hspec-expectations-lifted"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."quickcheck-transformer" or (errorHandler.buildDepError "quickcheck-transformer"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."ImpSpec" or (errorHandler.buildDepError "ImpSpec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ImpSpec-0.2.0.0.tar.gz";
      sha256 = "e0d2ec34fe40a0fcc44abddec126824dd6e94542181d42ad2c0e213277c24bb2";
    });
  }) // {
    package-description-override = "cabal-version: 2.4\nname: ImpSpec\nversion: 0.2.0.0\nlicense: Apache-2.0\nlicense-file: LICENSE\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/input-output-hk/ImpSpec\nsynopsis:\n  Imperative approach to testing stateful applications.\n  ImpSpec is built on top of HSpec and QuickCheck.\n\ndescription:\n  Let a little imp help you discover all the bugs in your stateful Haskell program.\n\ncategory: Testing\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\nextra-source-files: README.md\ntested-with:\n  ghc ==8.2.2\n  ghc ==8.4.4\n  ghc ==8.6.5\n  ghc ==8.8.4\n  ghc ==8.10.7\n  ghc ==9.0.2\n  ghc ==9.2.8\n  ghc ==9.4.8\n  ghc ==9.6.6\n  ghc ==9.8.2\n  ghc ==9.10.3\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/ImpSpec\n\nlibrary\n  exposed-modules:\n    Test.ImpSpec\n    Test.ImpSpec.Expectations\n    Test.ImpSpec.Expectations.Lifted\n    Test.ImpSpec.Main\n    Test.ImpSpec.Random\n\n  hs-source-dirs: src\n  other-modules: Test.ImpSpec.Internal\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n\n  build-depends:\n    HUnit,\n    QuickCheck,\n    base >=4.9 && <5,\n    bytestring,\n    deepseq,\n    hspec,\n    hspec-core >=2.11,\n    hspec-expectations-lifted,\n    mtl,\n    prettyprinter >=1.7,\n    prettyprinter-ansi-terminal >=1.1.2,\n    quickcheck-transformer,\n    random >=1.2,\n    text,\n    unliftio,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules: Test.Suite.ImpSpec\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -threaded\n    -rtsopts\n\n  build-depends:\n    ImpSpec,\n    base,\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/ImpSpec\n";
  }