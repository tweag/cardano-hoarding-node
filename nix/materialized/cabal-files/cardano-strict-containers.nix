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
      identifier = { name = "cardano-strict-containers"; version = "0.1.6.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Various strict container types";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-strict-containers-0.1.6.0.tar.gz";
      sha256 = "456c107b122f05a4eee767ad6899c25d7be3f3dd1fbac1e645fbb70daaff60bb";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-strict-containers\nversion: 0.1.6.0\nsynopsis: Various strict container types\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nextra-doc-files: CHANGELOG.md\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: IOHK\nbuild-type: Simple\n\ncommon language\n  default-language: Haskell2010\n\ncommon warnings\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: language, warnings\n  hs-source-dirs: src\n  exposed-modules:\n    Data.FingerTree.Strict\n    Data.Maybe.Strict\n    Data.Sequence.Strict\n    Data.Unit.Strict\n\n  build-depends:\n    aeson,\n    base,\n    cardano-binary >=1.6,\n    containers,\n    data-default-class,\n    deepseq,\n    fingertree,\n    nothunks,\n    serialise,\n\nlibrary testlib\n  import: language, warnings\n  visibility: public\n  hs-source-dirs: testlib\n  exposed-modules:\n    Test.Cardano.StrictContainers.Instances\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-strict-containers,\n";
  }