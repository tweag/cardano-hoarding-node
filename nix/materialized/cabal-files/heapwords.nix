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
      identifier = { name = "heapwords"; version = "0.1.0.2"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Heapwords";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "9.0.0") (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/heapwords-0.1.0.2.tar.gz";
      sha256 = "a0285b2d6ce541a6ea76db152a61a7250b094712b4ad421b3afdc3fca458b5ef";
    });
  }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                heapwords\nversion:             0.1.0.2\nsynopsis:            Heapwords\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  exposed-modules:      Cardano.HeapWords\n  build-depends:        array\n                      , base\n                      , bytestring\n                      , containers\n                      , ghc-prim\n                      , text\n                      , time\n                      , vector\n\n\n  if impl(ghc < 9.0.0)\n    build-depends:\n      integer-gmp\n";
  }