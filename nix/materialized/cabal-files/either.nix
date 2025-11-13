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
      identifier = { name = "either"; version = "5.0.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2017 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/either/";
      url = "";
      synopsis = "Combinators for working with sums";
      description = "Combinators for working with sums.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/either-5.0.3.tar.gz";
      sha256 = "6d66691e84955a90a2d169f71cf51c83d76c141635cb8da2a60ddffca4804801";
    });
  }) // {
    package-description-override = "name:          either\r\ncategory:      Control, Monads\r\nversion:       5.0.3\r\nx-revision: 1\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/either/\r\nbug-reports:   http://github.com/ekmett/either/issues\r\ncopyright:     Copyright (C) 2008-2017 Edward A. Kmett\r\nsynopsis:      Combinators for working with sums\r\ndescription:   Combinators for working with sums.\r\nbuild-type:    Simple\r\ntested-with:   GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.2\r\n             , GHC == 9.2.8\r\n             , GHC == 9.4.8\r\n             , GHC == 9.6.6\r\n             , GHC == 9.8.4\r\n             , GHC == 9.10.1\r\n             , GHC == 9.12.1\r\nextra-source-files:\r\n  .gitignore\r\n  .ghci\r\n  .vim.custom\r\n  CHANGELOG.markdown\r\n  README.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/ekmett/either.git\r\n\r\nlibrary\r\n  build-depends:\r\n    base              >= 4.9     && < 5,\r\n    bifunctors        >= 4       && < 6,\r\n    mtl               >= 2.0     && < 2.4,\r\n    profunctors       >= 4       && < 6,\r\n    semigroupoids     >= 4       && < 7\r\n\r\n  other-extensions: CPP Rank2Types\r\n  ghc-options: -Wall\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n  exposed-modules:\r\n    Data.Either.Combinators\r\n    Data.Either.Validation\r\n\r\ntest-suite tests\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  type: exitcode-stdio-1.0\r\n  main-is: Main.hs\r\n  hs-source-dirs: tests\r\n  build-depends:\r\n    base,\r\n    either,\r\n    tasty            >= 1.4  && < 1.6,\r\n    tasty-quickcheck >= 0.10 && < 0.12,\r\n    QuickCheck       >= 2.9  && < 2.18\r\n  default-language: Haskell2010\r\n";
  }