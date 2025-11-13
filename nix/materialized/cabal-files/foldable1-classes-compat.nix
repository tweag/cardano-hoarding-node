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
    flags = { tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "foldable1-classes-compat"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Edward Kmett, Oleg Grenrus";
      homepage = "https://github.com/haskell-compat/foldable1-classes-compat";
      url = "";
      synopsis = "Compatibility package for the Foldable1 and Bifoldable1 type classes";
      description = "A compatibility package for the @Foldable1@ and @Bifoldable1@ type classes,\nwhich were introduced in @base-4.18.0.0@ (GHC 9.6.1). For more information,\nsee <https://github.com/haskell/core-libraries-committee/issues/9 this Core\nLibraries Committee proposal>.\n\n@Foldable1@ and @Bifoldable1@ classify non-empty data structures that can be\nfolded to a summary value.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
        ] ++ pkgs.lib.optionals (!(compiler.isGhc && compiler.version.ge "9.6")) [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ]) ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.6")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.2")) (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"))) ++ pkgs.lib.optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.0") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/foldable1-classes-compat-0.1.2.tar.gz";
      sha256 = "e3160e89adea466c18f2ae3377efb7953daa86f01f17443cb4bcbd03d443cad8";
    });
  }) // {
    package-description-override = "cabal-version: >=1.10\nname:          foldable1-classes-compat\nversion:       0.1.2\nsynopsis:\n  Compatibility package for the Foldable1 and Bifoldable1 type classes\n\ndescription:\n  A compatibility package for the @Foldable1@ and @Bifoldable1@ type classes,\n  which were introduced in @base-4.18.0.0@ (GHC 9.6.1). For more information,\n  see <https://github.com/haskell/core-libraries-committee/issues/9 this Core\n  Libraries Committee proposal>.\n  .\n  @Foldable1@ and @Bifoldable1@ classify non-empty data structures that can be\n  folded to a summary value.\n\nlicense:       BSD3\nmaintainer:    Ryan Scott <ryan.gl.scott@gmail.com>\nauthor:        Edward Kmett, Oleg Grenrus\nhomepage:      https://github.com/haskell-compat/foldable1-classes-compat\nbug-reports:   https://github.com/haskell-compat/foldable1-classes-compat/issues\ncategory:      Data, Compatibility\nlicense-file:  LICENSE\nbuild-type:    Simple\nextra-source-files:\n  CHANGELOG.markdown\n  README.markdown\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.1\n\n-- , GHCJS ==8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-compat/foldable1-classes-compat.git\n\nflag tagged\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\n  default:     True\n  manual:      True\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  build-depends:    base >=4.9 && <4.22\n\n  if !impl(ghc >= 9.6)\n    hs-source-dirs: src\n    build-depends:\n        containers    >=0.4 && <0.9\n      , transformers  >=0.3 && <0.7\n    exposed-modules:\n      Data.Foldable1\n      Data.Bifoldable1\n\n  if !impl(ghc >=8.6)\n    build-depends: base-orphans >=0.8.1 && <0.10\n\n  if !impl(ghc >=8.2)\n    build-depends: bifunctor-classes-compat >=0.1 && <0.2\n\n  if flag(tagged)\n    build-depends: tagged >=0.4.4 && <1\n\n  if impl(ghc >= 9.0)\n    build-depends: ghc-prim >= 0.7 && <0.14\n\ntest-suite test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall\n  hs-source-dirs:   test\n  main-is:          Tests.hs\n\n  build-depends:\n      base\n    , containers\n    , foldable1-classes-compat\n    , transformers\n    , QuickCheck           >=2.13.2 && <2.17\n    , quickcheck-instances >=0.3.27 && <0.4\n    , tasty                >=1.4    && <1.6\n    , tasty-quickcheck     >=0.10   && <0.12\n\nbenchmark bench\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall\n  hs-source-dirs:   bench\n  main-is:          Bench.hs\n\n  build-depends:\n      base\n    , containers\n    , foldable1-classes-compat\n    , transformers\n    , tasty-bench >=0.3.5 && < 0.5\n    , deepseq     >=1.3   && <1.6\n";
  }