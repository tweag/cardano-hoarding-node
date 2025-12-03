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
      identifier = { name = "parallel"; version = "3.3.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Parallel programming library";
      description = "This package provides a library for parallel programming.\n\nFor documentation, start from the \"Control.Parallel.Strategies\"\nmodule below.\n\nFor more tutorial documentation, see the book <https://simonmar.github.io/pages/pcph.html Parallel and Concurrent Programming in Haskell>.\n\nTo understand the principles behind the library, see\n<https://simonmar.github.io/bib/papers/strategies.pdf Seq no more: Better Strategies for Parallel Haskell>.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/parallel-3.3.0.0.tar.gz";
      sha256 = "47c21e778d8e8ebf657aa72fd30e189e71ffddb188660e9d09ca9062d7541791";
    });
  }) // {
    package-description-override = "cabal-version:  >=1.10\nname:           parallel\nversion:        3.3.0.0\n-- NOTE: Don't forget to update ./changelog.md\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     libraries@haskell.org\nbug-reports:    https://github.com/haskell/parallel/issues\nsynopsis:       Parallel programming library\ncategory:       Control, Parallelism\nbuild-type:     Simple\n\ntested-with:\n  GHC == 9.12.2\n  GHC == 9.10.1\n  GHC == 9.8.4\n  GHC == 9.6.7\n  GHC == 9.4.8\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  MHS\n\ndescription:\n    This package provides a library for parallel programming.\n    .\n    For documentation, start from the \"Control.Parallel.Strategies\"\n    module below.\n    .\n    For more tutorial documentation, see the book <https://simonmar.github.io/pages/pcph.html Parallel and Concurrent Programming in Haskell>.\n    .\n    To understand the principles behind the library, see\n    <https://simonmar.github.io/bib/papers/strategies.pdf Seq no more: Better Strategies for Parallel Haskell>.\n\n\nextra-source-files: changelog.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/parallel.git\n\nlibrary\n    default-language: Haskell2010\n    other-extensions:\n        BangPatterns\n        CPP\n        MagicHash\n        UnboxedTuples\n\n    exposed-modules:\n        Control.Seq\n        Control.Parallel\n        Control.Parallel.Strategies\n\n    build-depends:\n        array      >= 0.3 && < 0.6,\n        base       >= 4.3 && < 4.22,\n        containers >= 0.4 && < 0.9,\n        deepseq    >= 1.1 && < 1.6\n\n    ghc-options: -Wall\n\n    if impl(ghc >= 6.11)\n        -- To improve parallel performance:\n        ghc-options: -feager-blackholing\n";
  }