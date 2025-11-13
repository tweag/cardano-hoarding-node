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
      specVersion = "1.10";
      identifier = { name = "cardano-strict-containers"; version = "0.1.5.0"; };
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
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-strict-containers-0.1.5.0.tar.gz";
      sha256 = "fd815ed978debc85cc7cf1c45b4d9db08349d9a6adb7a6782dea4f22498a7fef";
    });
  }) // {
    package-description-override = "cabal-version: >=1.10\nname: cardano-strict-containers\nversion: 0.1.5.0\nsynopsis: Various strict container types\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nextra-source-files: CHANGELOG.md\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: IOHK\nbuild-type: Simple\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs: src\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n\n  exposed-modules:\n    Data.FingerTree.Strict\n    Data.Maybe.Strict\n    Data.Sequence.Strict\n    Data.Unit.Strict\n\n  build-depends:\n    aeson,\n    base,\n    cardano-binary >=1.6,\n    cborg,\n    containers,\n    data-default-class,\n    deepseq,\n    fingertree,\n    nothunks,\n    serialise\n";
  }