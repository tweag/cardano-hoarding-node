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
    package-description-override = "name:           singletons\r\nversion:        3.0.4\r\nx-revision: 1\r\ncabal-version:  1.24\r\nsynopsis:       Basic singleton types and definitions\r\nhomepage:       http://www.github.com/goldfirere/singletons\r\ncategory:       Dependent Types\r\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>\r\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\r\nbug-reports:    https://github.com/goldfirere/singletons/issues\r\nstability:      experimental\r\ntested-with:    GHC == 8.0.2\r\n              , GHC == 8.2.2\r\n              , GHC == 8.4.4\r\n              , GHC == 8.6.5\r\n              , GHC == 8.8.4\r\n              , GHC == 8.10.7\r\n              , GHC == 9.0.2\r\n              , GHC == 9.2.7\r\n              , GHC == 9.4.8\r\n              , GHC == 9.6.6\r\n              , GHC == 9.8.2\r\n              , GHC == 9.10.1\r\n              , GHC == 9.12.1\r\nextra-source-files: README.md, CHANGES.md\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\ndescription:\r\n    @singletons@ contains the basic types and definitions needed to support\r\n    dependently typed programming techniques in Haskell. This library was\r\n    originally presented in /Dependently Typed Programming with Singletons/,\r\n    published at the Haskell Symposium, 2012.\r\n    (<https://richarde.dev/papers/2012/singletons/paper.pdf>)\r\n    .\r\n    @singletons@ is intended to be a small, foundational library on which other\r\n    projects can build. As such, @singletons@ has a minimal dependency\r\n    footprint and supports GHCs dating back to GHC 8.0. For more information,\r\n    consult the @singletons@\r\n    @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\r\n    .\r\n    You may also be interested in the following related libraries:\r\n    .\r\n    * The @singletons-th@ library defines Template Haskell functionality that\r\n      allows /promotion/ of term-level functions to type-level equivalents and\r\n      /singling/ functions to dependently typed equivalents.\r\n    .\r\n    * The @singletons-base@ library uses @singletons-th@ to define promoted and\r\n      singled functions from the @base@ library, including the \"Prelude\".\r\n\r\nsource-repository this\r\n  type:     git\r\n  location: https://github.com/goldfirere/singletons.git\r\n  subdir:   singletons\r\n  tag:      v3.0.2\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/goldfirere/singletons.git\r\n  subdir:   singletons\r\n  branch:   master\r\n\r\nlibrary\r\n  hs-source-dirs:     src\r\n  build-depends:      base >= 4.9 && < 4.23\r\n  default-language:   Haskell2010\r\n  exposed-modules:    Data.Singletons\r\n                      Data.Singletons.Decide\r\n                      Data.Singletons.ShowSing\r\n                      Data.Singletons.Sigma\r\n  ghc-options:        -Wall\r\n\r\ntest-suite singletons-test-suite\r\n  type:               exitcode-stdio-1.0\r\n  hs-source-dirs:     tests\r\n  ghc-options:        -Wall -threaded\r\n  default-language:   Haskell2010\r\n  main-is:            SingletonsTestSuite.hs\r\n  other-modules:      ByHand\r\n                      ByHand2\r\n\r\n  build-depends:      base >= 4.9 && < 4.23,\r\n                      singletons\r\n";
  }