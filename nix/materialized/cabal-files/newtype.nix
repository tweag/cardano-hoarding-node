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
      specVersion = "1.12";
      identifier = { name = "newtype"; version = "0.2.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Andreas Abel";
      author = "Herbert Valerio Riedel, Darius Jahandarie, Conor McBride";
      homepage = "";
      url = "";
      synopsis = "A typeclass and set of functions for working with newtypes.";
      description = "Per Conor McBride, the 'Newtype' typeclass represents the packing and unpacking of a @newtype@, and allows you to operate under that @newtype@ with functions such as 'ala'. See \"Control.Newtype\" for documentation and examples.";
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
      url = "http://hackage.haskell.org/package/newtype-0.2.2.1.tar.gz";
      sha256 = "7bf0c71d2030de67efb0c5bb17eb4050ab2ca1ef5aa1d49c011bb827f2c97c6a";
    });
  }) // {
    package-description-override = "cabal-version:       1.12\nbuild-type:          Simple\nname:                newtype\nversion:             0.2.2.1\n\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Herbert Valerio Riedel, Darius Jahandarie, Conor McBride\nmaintainer:          Andreas Abel\ncategory:            Control\nbug-reports:         https://github.com/hvr/newtype/issues\n\nsynopsis:            A typeclass and set of functions for working with newtypes.\ndescription:         Per Conor McBride, the 'Newtype' typeclass represents the packing and unpacking of a @newtype@, and allows you to operate under that @newtype@ with functions such as 'ala'. See \"Control.Newtype\" for documentation and examples.\n\ntested-with:\n   GHC == 9.14.1\n   GHC == 9.12.2\n   GHC == 9.10.3\n   GHC == 9.8.4\n   GHC == 9.6.7\n   GHC == 9.4.8\n   GHC == 9.2.8\n   GHC == 9.0.2\n   GHC == 8.10.7\n   GHC == 8.8.4\n   GHC == 8.6.5\n   GHC == 8.4.4\n   GHC == 8.2.2\n   GHC == 8.0.2\n\nextra-source-files:    CHANGES.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/newtype.git\n\nlibrary\n  exposed-modules:     Control.Newtype\n\n  build-depends:       base >= 4.9 && < 5\n\n  default-language:    Haskell2010\n  other-extensions:\n    CPP\n    FlexibleInstances\n    FunctionalDependencies\n    MultiParamTypeClasses\n    Trustworthy\n    TypeFamilies\n\n  ghc-options:\n    -Wall\n    -Wno-trustworthy-safe\n";
  }