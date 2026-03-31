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
      specVersion = "2.2";
      identifier = { name = "foldable1-classes-compat"; version = "0.1.3"; };
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
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
        ] ++ pkgs.lib.optionals (!(compiler.isMhs && true)) ((((pkgs.lib.optionals (!(compiler.isGhc && compiler.version.ge "9.6")) [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.6")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.2")) (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"))) ++ pkgs.lib.optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.0") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim")));
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
      url = "http://hackage.haskell.org/package/foldable1-classes-compat-0.1.3.tar.gz";
      sha256 = "7701dca1ccfeb7a20a17693954508f1e6c73974d1b59fb1795da0742fb1ae8f9";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\r\nname:          foldable1-classes-compat\r\nversion:       0.1.3\r\nx-revision: 2\r\nsynopsis:\r\n  Compatibility package for the Foldable1 and Bifoldable1 type classes\r\n\r\ndescription:\r\n  A compatibility package for the @Foldable1@ and @Bifoldable1@ type classes,\r\n  which were introduced in @base-4.18.0.0@ (GHC 9.6.1). For more information,\r\n  see <https://github.com/haskell/core-libraries-committee/issues/9 this Core\r\n  Libraries Committee proposal>.\r\n  .\r\n  @Foldable1@ and @Bifoldable1@ classify non-empty data structures that can be\r\n  folded to a summary value.\r\n\r\nlicense:       BSD-3-Clause\r\nmaintainer:    Ryan Scott <ryan.gl.scott@gmail.com>\r\nauthor:        Edward Kmett, Oleg Grenrus\r\nhomepage:      https://github.com/haskell-compat/foldable1-classes-compat\r\nbug-reports:   https://github.com/haskell-compat/foldable1-classes-compat/issues\r\ncategory:      Data, Compatibility\r\nlicense-file:  LICENSE\r\nbuild-type:    Simple\r\nextra-doc-files:\r\n  CHANGELOG.markdown\r\n  README.markdown\r\ntested-with:\r\n  GHC ==8.0.2\r\n   || ==8.2.2\r\n   || ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.8\r\n   || ==9.4.8\r\n   || ==9.6.7\r\n   || ==9.8.4\r\n   || ==9.10.3\r\n   || ==9.12.2\r\n\r\n-- , GHCJS ==8.4\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell-compat/foldable1-classes-compat.git\r\n\r\nflag tagged\r\n  description:\r\n    You can disable the use of the `tagged` package using `-f-tagged`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n\r\n  default:     True\r\n  manual:      True\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  build-depends:    base >=4.9 && <4.23\r\n\r\n  if !impl(mhs)\r\n    if !impl(ghc >= 9.6)\r\n      -- no Data.Foldable1, Data.Bifoldable1 in base\r\n      hs-source-dirs: src\r\n      build-depends:\r\n          containers    >=0.4 && <0.9\r\n        , transformers  >=0.3 && <0.7\r\n      exposed-modules:\r\n        Data.Foldable1\r\n        Data.Bifoldable1\r\n\r\n    if !impl(ghc >=8.6)\r\n      build-depends: base-orphans >=0.8.1 && <0.10\r\n\r\n    if !impl(ghc >=8.2)\r\n      build-depends: bifunctor-classes-compat >=0.1 && <0.2\r\n\r\n    if flag(tagged)\r\n      build-depends: tagged >=0.4.4 && <1\r\n\r\n    if impl(ghc >= 9.0)\r\n      build-depends: ghc-prim >= 0.7 && <0.14\r\n\r\ntest-suite test\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  ghc-options:      -Wall\r\n  hs-source-dirs:   test\r\n  main-is:          Tests.hs\r\n\r\n  build-depends:\r\n      base\r\n    , containers\r\n    , foldable1-classes-compat\r\n    , transformers\r\n    , QuickCheck           >=2.13.2 && <2.19\r\n    , quickcheck-instances >=0.3.27 && <0.5\r\n    , tasty                >=1.4    && <1.6\r\n    , tasty-quickcheck     >=0.10   && <0.12\r\n\r\nbenchmark bench\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  ghc-options:      -Wall\r\n  hs-source-dirs:   bench\r\n  main-is:          Bench.hs\r\n\r\n  build-depends:\r\n      base\r\n    , containers\r\n    , foldable1-classes-compat\r\n    , transformers\r\n    , tasty-bench >=0.3.5 && < 0.6\r\n    , deepseq     >=1.3   && <1.6\r\n";
  }