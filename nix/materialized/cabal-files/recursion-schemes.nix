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
    flags = { template-haskell = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "recursion-schemes"; version = "5.2.3"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "\"Samuel Gélineau\" <gelisam@gmail.com>,\n\"Ryan Scott\" <ryan.gl.scott@gmail.com>,\n\"Luc Tielen\" <luc.tielen@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/recursion-schemes/";
      url = "";
      synopsis = "Representing common recursion patterns as higher-order functions";
      description = "Many recursive functions share the same structure, e.g. pattern-match on the input and, depending on the data constructor, either recur on a smaller input or terminate the recursion with the base case. Another one: start with a seed value, use it to produce the first element of an infinite list, and recur on a modified seed in order to produce the rest of the list. Such a structure is called a recursion scheme. Using higher-order functions to implement those recursion schemes makes your code clearer, faster, and safer. See README for details.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ pkgs.lib.optionals (flags.template-haskell) [
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
        ];
        buildable = true;
      };
      tests = {
        "Expr" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/recursion-schemes-5.2.3.tar.gz";
      sha256 = "3b4e84bcec8d6ea5583388a9fc8b98255d39623d8420784a1a35fe46f4990e08";
    });
  }) // {
    package-description-override = "name:          recursion-schemes\r\ncategory:      Control, Recursion\r\nversion:       5.2.3\r\nx-revision: 1\r\nlicense:       BSD2\r\ncabal-version: 1.18\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    \"Samuel Gélineau\" <gelisam@gmail.com>,\r\n               \"Ryan Scott\" <ryan.gl.scott@gmail.com>,\r\n               \"Luc Tielen\" <luc.tielen@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/recursion-schemes/\r\nbug-reports:   http://github.com/ekmett/recursion-schemes/issues\r\ncopyright:     Copyright (C) 2008-2015 Edward A. Kmett\r\nsynopsis:      Representing common recursion patterns as higher-order functions\r\ndescription:   Many recursive functions share the same structure, e.g. pattern-match on the input and, depending on the data constructor, either recur on a smaller input or terminate the recursion with the base case. Another one: start with a seed value, use it to produce the first element of an infinite list, and recur on a modified seed in order to produce the rest of the list. Such a structure is called a recursion scheme. Using higher-order functions to implement those recursion schemes makes your code clearer, faster, and safer. See README for details.\r\n\r\ntested-with:   GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.2, GHC==9.2.8, GHC==9.4.8, GHC==9.6.5, GHC==9.8.2, GHC==9.10.1\r\n\r\nbuild-type:    Simple\r\nextra-doc-files: docs/github-compression.png docs/flowchart.svg\r\nextra-source-files: CHANGELOG.markdown .gitignore README.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/recursion-schemes.git\r\n\r\nflag template-haskell\r\n  description: About Template Haskell derivations\r\n  manual: True\r\n  default: True\r\n\r\nlibrary\r\n  other-extensions:\r\n    CPP\r\n    TypeFamilies\r\n    Rank2Types\r\n    FlexibleContexts\r\n    FlexibleInstances\r\n    GADTs\r\n    StandaloneDeriving\r\n    UndecidableInstances\r\n\r\n  hs-source-dirs: src\r\n\r\n  build-depends:\r\n    base                 >= 4.9     && < 5,\r\n    containers           >= 0.4.2.1 && < 0.8,\r\n    comonad              >= 4       && < 6,\r\n    data-fix             >= 0.3.0   && < 0.4,\r\n    free                 >= 4       && < 6,\r\n    transformers         >= 0.4.2.0 && < 1\r\n\r\n  if !impl(ghc >= 8.2)\r\n    build-depends: bifunctors >= 4 && < 6\r\n\r\n  -- Foldable module is first, so cabal repl loads it!\r\n  exposed-modules:\r\n    Data.Functor.Foldable\r\n    Data.Functor.Base\r\n\r\n  if flag(template-haskell)\r\n    build-depends:\r\n      template-haskell >= 2.11.0.0 && < 2.24,\r\n      th-abstraction   >= 0.4      && < 0.8\r\n    exposed-modules:\r\n      Data.Functor.Foldable.TH\r\n\r\n    other-modules:\r\n      Paths_recursion_schemes\r\n\r\n  ghc-options: -Wall\r\n  if impl(ghc >= 8.6)\r\n    ghc-options: -Wno-star-is-type\r\n  default-language: Haskell2010\r\n\r\ntest-suite Expr\r\n  type: exitcode-stdio-1.0\r\n  main-is: Expr.hs\r\n  hs-source-dirs: examples\r\n  ghc-options: -Wall -threaded\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    HUnit <1.7,\r\n    recursion-schemes,\r\n    template-haskell,\r\n    transformers     >= 0.2     && < 1\r\n";
  }