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
      identifier = { name = "MonadRandom"; version = "0.6.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Brent Yorgey <byorgey@gmail.com>";
      author = "Cale Gibbard and others";
      homepage = "";
      url = "";
      synopsis = "Random-number generation monad.";
      description = "Support for computations which consume random values.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/MonadRandom-0.6.2.1.tar.gz";
      sha256 = "b3103b475fa6cf4d14ef1b1bce0b4b00026d70b68e8842acfe87b894e4b178b6";
    });
  }) // {
    package-description-override = "name:                MonadRandom\nversion:             0.6.2.1\nsynopsis:            Random-number generation monad.\ndescription:         Support for computations which consume random values.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Cale Gibbard and others\nmaintainer:          Brent Yorgey <byorgey@gmail.com>\nbug-reports:         https://github.com/byorgey/MonadRandom/issues\ncategory:            Control\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:  CHANGES.markdown\ntested-with:         GHC ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.6 || ==9.8.2 || ==9.10.1 || ==9.12.1 || ==9.14.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/byorgey/MonadRandom\n\nlibrary\n  exposed-modules:\n    Control.Monad.Random,\n    Control.Monad.Random.Class,\n    Control.Monad.Random.Lazy,\n    Control.Monad.Random.Strict,\n    Control.Monad.Trans.Random,\n    Control.Monad.Trans.Random.Lazy,\n    Control.Monad.Trans.Random.Strict\n  build-depends:\n    base                >=4.8 && <5,\n    transformers        >=0.5 && <0.7,\n    mtl                 >=2.2.1 && <2.3 || >= 2.3.1 && < 2.4,\n    primitive           >=0.6 && <0.10,\n    random              >=1.0.1 && <1.4\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n  other-extensions:    Safe\n";
  }