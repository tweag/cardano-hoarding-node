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
      identifier = { name = "validation"; version = "1.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2010-2013 Tony Morris, Nick Partridge\nCopyright (C) 2014,2015 NICTA Limited\nCopyright (c) 2016-2019, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.";
      maintainer = "Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>, Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>";
      author = "Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>";
      homepage = "https://github.com/qfpl/validation";
      url = "";
      synopsis = "A data-type like Either but with an accumulating Applicative";
      description = "<<https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png>>\n\nA data-type like Either but with differing properties and type-class\ninstances.\n\nLibrary support is provided for this different representation, include\n`lens`-related functions for converting between each and abstracting over their\nsimilarities.\n\n* `Validation`\n\nThe `Validation` data type is isomorphic to `Either`, but has an instance\nof `Applicative` that accumulates on the error side. That is to say, if two\n(or more) errors are encountered, they are appended using a `Semigroup`\noperation.\n\nAs a consequence of this `Applicative` instance, there is no corresponding\n`Bind` or `Monad` instance. `Validation` is an example of, \"An applicative\nfunctor that is not a monad.\"";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
        ];
        buildable = true;
      };
      tests = {
        "hedgehog" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
          buildable = true;
        };
        "hunit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/validation-1.1.3.tar.gz";
      sha256 = "7b41e914cdca32a4e17667e3eea76219fadbf36b46e748bc5a4ab1a23fdd3795";
    });
  }) // {
    package-description-override = "name:               validation\r\nversion:            1.1.3\r\nx-revision: 1\r\nlicense:            BSD3\r\nlicense-file:       LICENCE\r\nauthor:             Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>\r\nmaintainer:         Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>, Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>\r\ncopyright:          Copyright (C) 2010-2013 Tony Morris, Nick Partridge\r\n                    Copyright (C) 2014,2015 NICTA Limited\r\n                    Copyright (c) 2016-2019, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.\r\nsynopsis:           A data-type like Either but with an accumulating Applicative\r\ncategory:           Data\r\ndescription:\r\n  <<https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png>>\r\n  .\r\n  A data-type like Either but with differing properties and type-class\r\n  instances.\r\n  .\r\n  Library support is provided for this different representation, include\r\n  `lens`-related functions for converting between each and abstracting over their\r\n  similarities.\r\n  .\r\n  * `Validation`\r\n  .\r\n  The `Validation` data type is isomorphic to `Either`, but has an instance\r\n  of `Applicative` that accumulates on the error side. That is to say, if two\r\n  (or more) errors are encountered, they are appended using a `Semigroup`\r\n  operation.\r\n  .\r\n  As a consequence of this `Applicative` instance, there is no corresponding\r\n  `Bind` or `Monad` instance. `Validation` is an example of, \"An applicative\r\n  functor that is not a monad.\"\r\n\r\nhomepage:           https://github.com/qfpl/validation\r\nbug-reports:        https://github.com/qfpl/validation/issues\r\ncabal-version:      >= 1.10\r\nbuild-type:         Simple\r\nextra-source-files: changelog\r\ntested-with:        GHC==9.0.1, GHC==8.10.4, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4\r\n\r\nsource-repository   head\r\n  type:             git\r\n  location:         git@github.com:qfpl/validation.git\r\n\r\nlibrary\r\n  default-language:\r\n                    Haskell2010\r\n\r\n  build-depends:\r\n                      base          >= 4.11   && < 5\r\n                    , assoc         >= 1      && < 2\r\n                    , deepseq       >= 1.4.3  && < 1.6\r\n                    , semigroups    >= 0.18.2 && < 1\r\n                    , semigroupoids >= 5.2.2  && < 7\r\n                    , bifunctors    >= 5.5    && < 6\r\n                    , lens          >= 4.0.5  && < 6\r\n\r\n  ghc-options:\r\n                    -Wall\r\n\r\n  hs-source-dirs:\r\n                    src\r\n\r\n  exposed-modules:\r\n                    Data.Validation\r\n\r\ntest-suite hedgehog\r\n  type:\r\n                    exitcode-stdio-1.0\r\n\r\n  main-is:\r\n                    hedgehog_tests.hs\r\n\r\n  default-language:\r\n                    Haskell2010\r\n\r\n  build-depends:\r\n                      base       >= 4.11   && < 5\r\n                    , hedgehog   >= 0.5    && < 2\r\n                    , semigroups >= 0.18.2 && < 1\r\n                    , validation\r\n\r\n  ghc-options:\r\n                    -Wall\r\n                    -threaded\r\n\r\n  hs-source-dirs:\r\n                    test\r\n\r\ntest-suite hunit\r\n  type:\r\n                    exitcode-stdio-1.0\r\n\r\n  main-is:\r\n                    hunit_tests.hs\r\n\r\n  default-language:\r\n                    Haskell2010\r\n\r\n  build-depends:\r\n                      base       >= 4.11   && < 5\r\n                    , HUnit      >= 1.6    && < 1.7\r\n                    , lens       >= 4.0.5  && < 6\r\n                    , semigroups >= 0.18.2 && < 1\r\n                    , validation\r\n\r\n  ghc-options:\r\n                    -Wall\r\n                    -threaded\r\n\r\n  hs-source-dirs:\r\n                    test\r\n";
  }