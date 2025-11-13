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
      identifier = { name = "ordered-containers"; version = "0.2.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "me@dmwit.com";
      author = "Daniel Wagner";
      homepage = "";
      url = "";
      synopsis = "Set- and Map-like types that remember the order elements were inserted";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ordered-containers-0.2.4.tar.gz";
      sha256 = "3da1673e24e12902c2879ee08b73e6978f6bfa70693b13995ebf48bfb7aee546";
    });
  }) // {
    package-description-override = "name:                ordered-containers\nversion:             0.2.4\nsynopsis:            Set- and Map-like types that remember the order elements were inserted\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Daniel Wagner\nmaintainer:          me@dmwit.com\ncategory:            Data\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       >=1.10\n\nsource-repository head\n  type:                git\n  location:            https://github.com/dmwit/ordered-containers\n\nlibrary\n  exposed-modules:     Data.Map.Ordered, Data.Map.Ordered.Strict, Data.Set.Ordered\n  other-modules:       Data.Map.Ordered.Internal, Data.Map.Util\n  build-depends:       base >=4.7 && <5, containers >=0.1 && <0.8, hashable >=1.2 && <2.0\n  default-language:    Haskell98\n  ghc-options:         -fno-warn-tabs\n";
  }