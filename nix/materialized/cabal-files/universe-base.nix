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
      specVersion = "2.2";
      identifier = { name = "universe-base"; version = "1.1.4"; };
      license = "BSD-3-Clause";
      copyright = "2014 Daniel Wagner";
      maintainer = "me@dmwit.com";
      author = "Daniel Wagner";
      homepage = "https://github.com/dmwit/universe";
      url = "";
      synopsis = "A class for finite and recursively enumerable types.";
      description = "A class for finite and recursively enumerable types and some helper functions for enumerating them.\n\n@\nclass Universe a where universe :: [a]\nclass Universe a => Finite a where universeF :: [a]; universeF = universe\n@\n\nThis is slim package definiting only the type-classes and instances\nfor types in GHC boot libraries.\nFor more instances check @universe-instances-*@ packages.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ] ++ pkgs.lib.optionals (!(compiler.isGhc && compiler.version.ge "9.2")) (if compiler.isGhc && compiler.version.ge "9.0"
          then [
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ]
          else [
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          ]);
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."universe-base" or (errorHandler.buildDepError "universe-base"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/universe-base-1.1.4.tar.gz";
      sha256 = "aee5589f372927dc3fa66e0cf4e284b89235c0aa3793ded744885ab717f41e98";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               universe-base\nversion:            1.1.4\nx-revision:         1\nsynopsis:           A class for finite and recursively enumerable types.\ndescription:\n  A class for finite and recursively enumerable types and some helper functions for enumerating them.\n  .\n  @\n  class Universe a where universe :: [a]\n  class Universe a => Finite a where universeF :: [a]; universeF = universe\n  @\n  .\n  This is slim package definiting only the type-classes and instances\n  for types in GHC boot libraries.\n  For more instances check @universe-instances-*@ packages.\n\nhomepage:           https://github.com/dmwit/universe\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nauthor:             Daniel Wagner\nmaintainer:         me@dmwit.com\ncopyright:          2014 Daniel Wagner\ncategory:           Data\nbuild-type:         Simple\nextra-source-files: changelog\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/dmwit/universe\n  subdir:   universe-base\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  exposed-modules:\n    Data.Universe.Class\n    Data.Universe.Helpers\n    Data.Universe.Generic\n\n  other-extensions:\n    BangPatterns\n    DefaultSignatures\n    GADTs\n    ScopedTypeVariables\n    TypeFamilies\n\n  build-depends:\n      base          >=4.12    && <4.22\n    , containers    >=0.6.0.1 && <0.8\n    , tagged        >=0.8.8   && <0.9\n    , transformers  >=0.5.6.2 && <0.7\n\n  if !impl(ghc >=9.2)\n    if impl(ghc >=9.0)\n      build-depends: ghc-prim\n\n    else\n      build-depends: OneTuple >=0.4.2 && <0.5\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite tests\n  default-language: Haskell2010\n  other-extensions: ScopedTypeVariables\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   tests\n  ghc-options:      -Wall\n  build-depends:\n      base\n    , containers\n    , QuickCheck     >=2.8.2 && <2.16\n    , universe-base\n";
  }