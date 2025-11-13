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
      identifier = { name = "infinite-list"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andrew.lelechenko@gmail.com";
      author = "Bodigrim";
      homepage = "https://github.com/Bodigrim/infinite-list";
      url = "";
      synopsis = "Infinite lists";
      description = "Modern lightweight library for infinite lists with fusion:\n\n* API similar to \"Data.List\".\n* No dependencies other than `base`.\n* Top performance, driven by fusion.\n* Avoid dangerous instances like `Foldable`.\n* Use `NonEmpty` where applicable.\n* Use `Word` for indices.\n* Be lazy, but not too lazy.\n\n@\n{\\-# LANGUAGE PostfixOperators #-\\}\nimport Data.List.Infinite (Infinite(..), (...), (....))\nimport qualified Data.List.Infinite as Inf\n@";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "infinite-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."infinite-list" or (errorHandler.buildDepError "infinite-list"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
        "infinite-properties-O0" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."infinite-list" or (errorHandler.buildDepError "infinite-list"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
        "infinite-fusion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."infinite-list" or (errorHandler.buildDepError "infinite-list"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-inspection-testing" or (errorHandler.buildDepError "tasty-inspection-testing"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
          ];
          buildable = if compiler.isGhc && compiler.version.lt "9.2"
            then false
            else true;
        };
      };
      benchmarks = {
        "infinite-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."infinite-list" or (errorHandler.buildDepError "infinite-list"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/infinite-list-0.1.2.tar.gz";
      sha256 = "6bae26bd8deee7c7f0afa1dc374094eb26546116c24f5bcc47b631e7608bbd6c";
    });
  }) // {
    package-description-override = "cabal-version:   2.2\nname:            infinite-list\nversion:         0.1.2\nlicense:         BSD-3-Clause\nlicense-file:    LICENSE\nmaintainer:      andrew.lelechenko@gmail.com\nauthor:          Bodigrim\ntested-with:\n    ghc ==8.2.2 ghc ==8.4.4 ghc ==8.6.5 ghc ==8.8.4\n    ghc ==8.10.7 ghc ==9.0.2 ghc ==9.2.8 ghc ==9.4.8 ghc ==9.6.5\n    ghc ==9.8.4 ghc ==9.10.1 ghc ==9.12.1\n\nhomepage:        https://github.com/Bodigrim/infinite-list\nsynopsis:        Infinite lists\ndescription:\n    Modern lightweight library for infinite lists with fusion:\n    .\n    * API similar to \"Data.List\".\n    * No dependencies other than `base`.\n    * Top performance, driven by fusion.\n    * Avoid dangerous instances like `Foldable`.\n    * Use `NonEmpty` where applicable.\n    * Use `Word` for indices.\n    * Be lazy, but not too lazy.\n    .\n    @\n    {\\-# LANGUAGE PostfixOperators #-\\}\n    import Data.List.Infinite (Infinite(..), (...), (....))\n    import qualified Data.List.Infinite as Inf\n    @\n\ncategory:        Data\nbuild-type:      Simple\nextra-doc-files:\n    CHANGELOG.md\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/Bodigrim/infinite-list\n\nlibrary\n    exposed-modules:  Data.List.Infinite\n    hs-source-dirs:   src\n    other-modules:\n        Data.List.Infinite.Internal\n        Data.List.Infinite.Set\n        Data.List.Infinite.Zip\n\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:    base >=4.10 && <5\n\ntest-suite infinite-properties\n    type:             exitcode-stdio-1.0\n    main-is:          Properties.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        containers,\n        infinite-list,\n        QuickCheck,\n        tasty,\n        tasty-quickcheck\n\ntest-suite infinite-properties-O0\n    type:             exitcode-stdio-1.0\n    main-is:          Properties.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall -O0\n    build-depends:\n        base,\n        containers,\n        infinite-list,\n        QuickCheck,\n        tasty,\n        tasty-quickcheck\n\ntest-suite infinite-fusion\n    type:             exitcode-stdio-1.0\n    main-is:          Fusion.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        infinite-list,\n        tasty,\n        tasty-inspection-testing,\n        tasty-expected-failure\n\n    if impl(ghc <9.2)\n        buildable: False\n\nbenchmark infinite-bench\n    type:             exitcode-stdio-1.0\n    main-is:          Bench.hs\n    hs-source-dirs:   bench\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        infinite-list,\n        tasty-bench\n\n    if impl(ghc >=8.6)\n        ghc-options: -fproc-alignment=64\n";
  }