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
      specVersion = "1.18";
      identifier = { name = "FailT"; version = "0.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "2022-2023 Alexey Kuleshevich";
      maintainer = "alexey@kuleshevi.ch";
      author = "Alexey Kuleshevich";
      homepage = "https://github.com/lehins/FailT";
      url = "";
      synopsis = "A 'FailT' monad transformer that plays well with 'MonadFail'";
      description = "Fail gracefully when stuck in a 'MonadFail'\n\n>>> runFailT (fail \"Failure!?\" >> pure \"Success!!\")\nLeft \"Failure!?\"\n>>> runFailT (fail \"Failure!?\" <|> pure \"Success!!\")\nRight \"Success!!\"\n>>> runFailT (pure [\"Success!!\"] <> fail \"Failure!?\" <> pure [\"At\", \"Last!\"])\nRight [\"Success!!\",\"At\",\"Last!\"]\n";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          ];
          buildable = true;
        };
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/FailT-0.1.2.0.tar.gz";
      sha256 = "f1e3d5714df7e7e134545239bd78927bc9949a9b4760dfa061ff2b35ff0a76c3";
    });
  }) // {
    package-description-override = "name:                FailT\nversion:             0.1.2.0\nsynopsis:            A 'FailT' monad transformer that plays well with 'MonadFail'\ndescription:\n    Fail gracefully when stuck in a 'MonadFail'\n    .\n    >>> runFailT (fail \"Failure!?\" >> pure \"Success!!\")\n    Left \"Failure!?\"\n    >>> runFailT (fail \"Failure!?\" <|> pure \"Success!!\")\n    Right \"Success!!\"\n    >>> runFailT (pure [\"Success!!\"] <> fail \"Failure!?\" <> pure [\"At\", \"Last!\"])\n    Right [\"Success!!\",\"At\",\"Last!\"]\n    .\n\n\nhomepage:             https://github.com/lehins/FailT\nlicense:              BSD3\nlicense-file:         LICENSE\nauthor:               Alexey Kuleshevich\nmaintainer:           alexey@kuleshevi.ch\ncopyright:            2022-2023 Alexey Kuleshevich\ncategory:             Control, Failure\nbuild-type:           Simple\nextra-source-files:   README.md\n                    , CHANGELOG.md\ncabal-version:        1.18\ntested-with:          GHC == 8.0.2\n                    , GHC == 8.2.2\n                    , GHC == 8.4.4\n                    , GHC == 8.6.5\n                    , GHC == 8.8.4\n                    , GHC == 8.10.7\n                    , GHC == 9.0.2\n                    , GHC == 9.2.5\n                    , GHC == 9.4.4\n\nlibrary\n  hs-source-dirs:     src\n  exposed-modules:    Control.Monad.Trans.Fail\n                    , Control.Monad.Trans.Fail.String\n                    , Control.Monad.Trans.Fail.Text\n\n  build-depends:      base >= 4.9 && < 5\n                    , exceptions\n                    , mtl\n                    , text\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\n\ntest-suite doctests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            doctests.hs\n  build-depends:      base\n                    , doctest >= 0.15\n                    , exceptions\n                    , FailT\n  default-language:   Haskell2010\n  ghc-options:       -Wall\n                     -fno-warn-orphans\n                     -threaded\n\ntest-suite tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            Main.hs\n  other-modules:      Test.Control.Monad.Trans.FailSpec\n  build-depends:      base\n                    , FailT\n                    , hspec\n                    , mtl\n                    , QuickCheck\n                    , quickcheck-classes >= 0.6\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n                      -fno-warn-orphans\n                      -threaded\n                      -with-rtsopts=-N2\n\nsource-repository head\n  type:     git\n  location: https://github.com/lehins/FailT\n";
  }