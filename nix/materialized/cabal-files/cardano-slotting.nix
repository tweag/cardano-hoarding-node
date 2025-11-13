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
      identifier = { name = "cardano-slotting"; version = "0.2.0.1"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Key slotting types for cardano libraries";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-slotting-0.2.0.1.tar.gz";
      sha256 = "b4fbffce9074f8367515724b55fc9e7743aa13288fa5802b48ff9e0787066773";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-slotting\nversion: 0.2.0.1\nsynopsis: Key slotting types for cardano libraries\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK Formal Methods Team\nmaintainer: formal.methods@iohk.io\ncopyright: IOHK\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  exposed-modules:\n    Cardano.Slotting.Block\n    Cardano.Slotting.EpochInfo\n    Cardano.Slotting.EpochInfo.API\n    Cardano.Slotting.EpochInfo.Extend\n    Cardano.Slotting.EpochInfo.Impl\n    Cardano.Slotting.Slot\n    Cardano.Slotting.Time\n\n  build-depends:\n    aeson,\n    base,\n    cardano-binary,\n    deepseq,\n    mmorph,\n    nothunks,\n    quiet,\n    serialise,\n    time,\n\nlibrary testlib\n  import: base, project-config\n  visibility: public\n  hs-source-dirs: testlib\n  exposed-modules:\n    Test.Cardano.Slotting.Arbitrary\n    Test.Cardano.Slotting.Numeric\n    Test.Cardano.Slotting.TreeDiff\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-slotting,\n    tree-diff,\n\ntest-suite tests\n  import: base, project-config\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules: Test.Cardano.Slotting.EpochInfo\n  build-depends:\n    base,\n    cardano-slotting,\n    tasty,\n    tasty-quickcheck,\n\n  ghc-options:\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n";
  }