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
      identifier = { name = "measures"; version = "0.1.0.3"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "An abstraction for (tuples of) measured quantities";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/measures-0.1.0.3.tar.gz";
      sha256 = "9dee462ce47bc86b00b1a86f4f1cdce8defb36b6343b2c2c76d95a98fd76b30a";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: measures\nversion: 0.1.0.3\nsynopsis: An abstraction for (tuples of) measured quantities\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: IOHK\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Widentities\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wmissing-export-lists\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  exposed-modules:\n    Data.Measure\n    Data.Measure.Class\n\n  build-depends:\n    base-deriving-via\n\ntest-suite tests\n  import: base, project-config\n  hs-source-dirs: test\n  main-is: Main.hs\n  type: exitcode-stdio-1.0\n  other-modules:\n    Test.Data.Measure\n\n  build-depends:\n    QuickCheck,\n    hspec,\n    measures,\n";
  }