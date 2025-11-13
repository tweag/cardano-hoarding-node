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
      specVersion = "1.24";
      identifier = { name = "singletons"; version = "3.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>";
      homepage = "http://www.github.com/goldfirere/singletons";
      url = "";
      synopsis = "Basic singleton types and definitions";
      description = "@singletons@ contains the basic types and definitions needed to support\ndependently typed programming techniques in Haskell. This library was\noriginally presented in /Dependently Typed Programming with Singletons/,\npublished at the Haskell Symposium, 2012.\n(<https://richarde.dev/papers/2012/singletons/paper.pdf>)\n\n@singletons@ is intended to be a small, foundational library on which other\nprojects can build. As such, @singletons@ has a minimal dependency\nfootprint and supports GHCs dating back to GHC 8.0. For more information,\nconsult the @singletons@\n@<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\n\nYou may also be interested in the following related libraries:\n\n* The @singletons-th@ library defines Template Haskell functionality that\nallows /promotion/ of term-level functions to type-level equivalents and\n/singling/ functions to dependently typed equivalents.\n\n* The @singletons-base@ library uses @singletons-th@ to define promoted and\nsingled functions from the @base@ library, including the \"Prelude\".";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "singletons-test-suite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/singletons-3.0.4.tar.gz";
      sha256 = "01334b3b8f1f7d8bc59e413756fa3bbca5f322fd326af7fac0f7c79d285a9d65";
    });
  }) // {
    package-description-override = "name:           singletons\nversion:        3.0.4\ncabal-version:  1.24\nsynopsis:       Basic singleton types and definitions\nhomepage:       http://www.github.com/goldfirere/singletons\ncategory:       Dependent Types\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\nbug-reports:    https://github.com/goldfirere/singletons/issues\nstability:      experimental\ntested-with:    GHC == 8.0.2\n              , GHC == 8.2.2\n              , GHC == 8.4.4\n              , GHC == 8.6.5\n              , GHC == 8.8.4\n              , GHC == 8.10.7\n              , GHC == 9.0.2\n              , GHC == 9.2.7\n              , GHC == 9.4.8\n              , GHC == 9.6.6\n              , GHC == 9.8.2\n              , GHC == 9.10.1\n              , GHC == 9.12.1\nextra-source-files: README.md, CHANGES.md\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ndescription:\n    @singletons@ contains the basic types and definitions needed to support\n    dependently typed programming techniques in Haskell. This library was\n    originally presented in /Dependently Typed Programming with Singletons/,\n    published at the Haskell Symposium, 2012.\n    (<https://richarde.dev/papers/2012/singletons/paper.pdf>)\n    .\n    @singletons@ is intended to be a small, foundational library on which other\n    projects can build. As such, @singletons@ has a minimal dependency\n    footprint and supports GHCs dating back to GHC 8.0. For more information,\n    consult the @singletons@\n    @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\n    .\n    You may also be interested in the following related libraries:\n    .\n    * The @singletons-th@ library defines Template Haskell functionality that\n      allows /promotion/ of term-level functions to type-level equivalents and\n      /singling/ functions to dependently typed equivalents.\n    .\n    * The @singletons-base@ library uses @singletons-th@ to define promoted and\n      singled functions from the @base@ library, including the \"Prelude\".\n\nsource-repository this\n  type:     git\n  location: https://github.com/goldfirere/singletons.git\n  subdir:   singletons\n  tag:      v3.0.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/goldfirere/singletons.git\n  subdir:   singletons\n  branch:   master\n\nlibrary\n  hs-source-dirs:     src\n  build-depends:      base >= 4.9 && < 4.22\n  default-language:   Haskell2010\n  exposed-modules:    Data.Singletons\n                      Data.Singletons.Decide\n                      Data.Singletons.ShowSing\n                      Data.Singletons.Sigma\n  ghc-options:        -Wall\n\ntest-suite singletons-test-suite\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  ghc-options:        -Wall -threaded\n  default-language:   Haskell2010\n  main-is:            SingletonsTestSuite.hs\n  other-modules:      ByHand\n                      ByHand2\n\n  build-depends:      base >= 4.9 && < 4.22,\n                      singletons\n";
  }