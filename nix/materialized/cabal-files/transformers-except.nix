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
      identifier = { name = "transformers-except"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2023 Tim McGilchrist";
      maintainer = "Tim McGilchrist <timmcgil@gmail.com>";
      author = "Tim McGilchrist <timmcgil@gmail.com>";
      homepage = "http://github.com/tmcgilchrist/transformers-either/";
      url = "";
      synopsis = "An Except monad transformer with";
      description = "Extra pieces for working with Except";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-except-0.1.4.tar.gz";
      sha256 = "71990961552b1eaa66f54e65f0b709c9a96d2121c3959a13ddcb5b5a5d67e40d";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\r\nname:          transformers-except\r\nversion:       0.1.4\r\nx-revision: 1\r\nlicense:       BSD-3-Clause\r\nlicense-file:  LICENSE\r\nauthor:        Tim McGilchrist <timmcgil@gmail.com>\r\nmaintainer:    Tim McGilchrist <timmcgil@gmail.com>\r\ncopyright:     (c) 2023 Tim McGilchrist\r\nsynopsis:      An Except monad transformer with\r\ncategory:      System\r\nbuild-type:    Simple\r\ntested-with:\r\n  GHC ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.2 || ==9.2.5 || ==9.4.4 || ==9.6.1\r\n\r\ndescription:   Extra pieces for working with Except\r\nhomepage:      http://github.com/tmcgilchrist/transformers-either/\r\nbug-reports:   http://github.com/tmcgilchrist/transformers-either/issues\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/tmcgilchrist/transformers-either.git\r\n\r\nlibrary\r\n  build-depends:\r\n    , base          >=4.8 && <5\r\n    , exceptions    >=0.6 && <0.11\r\n    , text          >= 1.2 && <2.2\r\n    , transformers  >=0.4 && <0.7\r\n\r\n  ghc-options:      -Wall\r\n  default-language: Haskell98\r\n  hs-source-dirs:   src\r\n  exposed-modules:\r\n    Control.Monad.Trans.Except.Exit\r\n    Control.Monad.Trans.Except.Extra\r\n";
  }