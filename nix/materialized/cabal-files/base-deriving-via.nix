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
      identifier = { name = "base-deriving-via"; version = "0.1.0.2"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A general hook newtype for use with deriving via";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/base-deriving-via-0.1.0.2.tar.gz";
      sha256 = "d75e4927980f44c6af36f963f956d3d4aee640a4740b457887605fafde76b37f";
    });
  }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                base-deriving-via\nversion:             0.1.0.2\nsynopsis:            A general hook newtype for use with deriving via\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n\n  exposed-modules:\n                        Data.DerivingVia\n                        Data.DerivingVia.GHC.Generics.Monoid\n                        Data.DerivingVia.GHC.Generics.Semigroup\n\n  build-depends:        base\n";
  }