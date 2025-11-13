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
    flags = { asserts = false; };
    package = {
      specVersion = "3.4";
      identifier = { name = "io-classes"; version = "1.8.0.1"; };
      license = "Apache-2.0";
      copyright = "2019-2025 Input Output Global Inc (IOG)";
      maintainer = "Duncan Coutts duncan@well-typed.com, Marcin Szamotulski coot@coot.me";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski, Neil Davies, Thomas Winant";
      homepage = "";
      url = "";
      synopsis = "Type classes for concurrency with STM, ST and timing";
      description = "IO Monad class hierarchy compatible with:\n\n  * [io-sim](https://hackage.haskell.org/package/io-sim),\n  * [base](https://hackage.haskell.org/package/base),\n  * [async](https://hackage.haskell.org/package/async),\n  * [stm](https://hackage.haskell.org/package/stm),\n  * [exceptions](https://hackage.haskell.org/package/exceptions) &\n  * [time](https://hackage.haskell.org/package/time)\n\npackages.\n\n= Sublibraries\n@io-classes@ provides non-standard extensions distributed in public\nsublibraries\n\n  * @io-classes:io-classes@ - the main library compatible with the above\n    packages\n  * @io-classes:strict-stm@ - strict @STM@ API\n  * @io-classes:strict-mvar@ - strict @MVar@ API\n  * @io-classes:si-timers@ - SI-unit based time / timers API, cancellable timers\n  * @io-classes:mtl@ - MTL instances, some of which are experiemental\n\n= Documentation\nHaddocks of all public sublibraries are published\n[here](https://input-output-hk.github.io/io-sim).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.10") (hsPkgs."ghc-internal" or (errorHandler.buildDepError "ghc-internal"));
        buildable = true;
      };
      sublibs = {
        "strict-stm" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
          buildable = true;
        };
        "strict-mvar" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
          buildable = true;
        };
        "si-timers" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
          buildable = true;
        };
        "mtl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          ];
          buildable = true;
        };
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test-strict-mvar" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."io-classes".components.sublibs.testlib or (errorHandler.buildDepError "io-classes:testlib"))
          ];
          buildable = true;
        };
        "test-si-timers" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/io-classes-1.8.0.1.tar.gz";
      sha256 = "b12df6bc63238a83fc1e3250475bb89affa6ca640332f10a77280a7a39d07047";
    });
  }) // {
    package-description-override = "cabal-version:       3.4\nname:                io-classes\nversion:             1.8.0.1\nsynopsis:            Type classes for concurrency with STM, ST and timing\ndescription:\n  IO Monad class hierarchy compatible with:\n\n    * [io-sim](https://hackage.haskell.org/package/io-sim),\n    * [base](https://hackage.haskell.org/package/base),\n    * [async](https://hackage.haskell.org/package/async),\n    * [stm](https://hackage.haskell.org/package/stm),\n    * [exceptions](https://hackage.haskell.org/package/exceptions) &\n    * [time](https://hackage.haskell.org/package/time)\n\n  packages.\n\n  = Sublibraries\n  @io-classes@ provides non-standard extensions distributed in public\n  sublibraries\n\n    * @io-classes:io-classes@ - the main library compatible with the above\n      packages\n    * @io-classes:strict-stm@ - strict @STM@ API\n    * @io-classes:strict-mvar@ - strict @MVar@ API\n    * @io-classes:si-timers@ - SI-unit based time / timers API, cancellable timers\n    * @io-classes:mtl@ - MTL instances, some of which are experiemental\n\n  = Documentation\n  Haddocks of all public sublibraries are published\n  [here](https://input-output-hk.github.io/io-sim).\n\nlicense:             Apache-2.0\nlicense-files:       LICENSE NOTICE\ncopyright:           2019-2025 Input Output Global Inc (IOG)\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski, Neil Davies, Thomas Winant\nmaintainer:          Duncan Coutts duncan@well-typed.com, Marcin Szamotulski coot@coot.me\ncategory:            Control\nbuild-type:          Simple\nextra-doc-files:     CHANGELOG.md README.md strict-stm/README.md strict-mvar/README.md\nbug-reports:         https://github.com/input-output-hk/io-sim/issues\ntested-with:         GHC == { 9.6, 9.8, 9.10, 9.12 }\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/io-sim\n  subdir:   io-classes\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\ncommon warnings\n    ghc-options: -Wall\n                 -Wcompat\n                 -Wincomplete-uni-patterns\n                 -Wincomplete-record-updates\n                 -Wpartial-fields\n                 -Widentities\n                 -Wunused-packages\n                 -Wno-redundant-constraints\n                 -Wno-unticked-promoted-constructors\n\nlibrary\n  import:              warnings\n  hs-source-dirs:      src\n\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:     Control.Concurrent.Class.MonadMVar\n                       Control.Concurrent.Class.MonadSTM\n                       Control.Concurrent.Class.MonadSTM.TArray\n                       Control.Concurrent.Class.MonadSTM.TBQueue\n                       Control.Concurrent.Class.MonadSTM.TChan\n                       Control.Concurrent.Class.MonadSTM.TMVar\n                       Control.Concurrent.Class.MonadSTM.TQueue\n                       Control.Concurrent.Class.MonadSTM.TSem\n                       Control.Concurrent.Class.MonadSTM.TVar\n                       Control.Monad.Class.MonadAsync\n                       Control.Monad.Class.MonadEventlog\n                       Control.Monad.Class.MonadFork\n                       Control.Monad.Class.MonadSay\n                       Control.Monad.Class.MonadST\n                       Control.Monad.Class.MonadSTM\n                       Control.Monad.Class.MonadSTM.Internal\n                       Control.Monad.Class.MonadThrow\n                       Control.Monad.Class.MonadTime\n                       Control.Monad.Class.MonadTimer\n                       Control.Monad.Class.MonadTest\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  build-depends:       base  >=4.16 && <4.22,\n                       array,\n                       async >=2.1 && <2.3,\n                       bytestring,\n                       mtl   >=2.2 && <2.4,\n                       primitive >= 0.7 && <0.11,\n                       stm   >=2.5 && <2.5.2 || ^>=2.5.3,\n                       time  >=1.9.1 && <1.13\n  if impl(ghc >= 9.10)\n    build-depends:     ghc-internal\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary strict-stm\n  import:              warnings\n  visibility:          public\n  hs-source-dirs:      strict-stm\n\n  exposed-modules:     Control.Concurrent.Class.MonadSTM.Strict\n                       Control.Concurrent.Class.MonadSTM.Strict.TArray\n                       Control.Concurrent.Class.MonadSTM.Strict.TBQueue\n                       Control.Concurrent.Class.MonadSTM.Strict.TChan\n                       Control.Concurrent.Class.MonadSTM.Strict.TMVar\n                       Control.Concurrent.Class.MonadSTM.Strict.TQueue\n                       Control.Concurrent.Class.MonadSTM.Strict.TVar\n  reexported-modules:  Control.Concurrent.Class.MonadSTM.TSem as Control.Concurrent.Class.MonadSTM.Strict.TSem\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  if impl(ghc < 9.4)\n    default-extensions: GADTs\n  build-depends:       base,\n                       array,\n\n                       io-classes:io-classes,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary strict-mvar\n  import:              warnings\n  visibility:          public\n  hs-source-dirs:      strict-mvar/src\n\n  exposed-modules:     Control.Concurrent.Class.MonadMVar.Strict\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  if impl(ghc < 9.4)\n    default-extensions: GADTs\n  build-depends:       base,\n                       io-classes:io-classes,\n\nlibrary si-timers\n  import:              warnings\n  visibility:          public\n  hs-source-dirs:      si-timers/src\n  exposed-modules:     Control.Monad.Class.MonadTime.SI\n                       Control.Monad.Class.MonadTimer.SI\n  other-modules:       Control.Monad.Class.MonadTimer.NonStandard\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  build-depends:       base,\n                       deepseq,\n                       mtl,\n                       nothunks,\n                       stm,\n                       time,\n\n                       io-classes:io-classes\n  if flag(asserts)\n     ghc-options:      -fno-ignore-asserts\n\nlibrary mtl\n    import:           warnings\n    visibility:       public\n    exposed-modules:  Control.Monad.Class.Trans\n                   ,  Control.Monad.Class.MonadEventlog.Trans\n                   ,  Control.Monad.Class.MonadSay.Trans\n                   ,  Control.Monad.Class.MonadST.Trans\n                   ,  Control.Monad.Class.MonadSTM.Trans\n                   ,  Control.Monad.Class.MonadThrow.Trans\n                   ,  Control.Monad.Class.MonadTime.Trans\n                   ,  Control.Monad.Class.MonadTime.SI.Trans\n                   ,  Control.Monad.Class.MonadTimer.Trans\n                   ,  Control.Monad.Class.MonadTimer.SI.Trans\n    build-depends:    base,\n                      array,\n                      mtl,\n\n                      io-classes:{io-classes,si-timers}\n\n    hs-source-dirs:   mtl\n    default-language: GHC2021\n    default-extensions: LambdaCase\n\nlibrary testlib\n  import:              warnings\n  visibility:          public\n  hs-source-dirs:      test\n  exposed-modules:     Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  build-depends:       base,\n                       nothunks,\n                       QuickCheck,\n                       io-classes:strict-mvar\n  if flag(asserts)\n     ghc-options:      -fno-ignore-asserts\n\ntest-suite test-strict-mvar\n  import:              warnings\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      strict-mvar/test\n  main-is:             Main.hs\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  build-depends:       base,\n                       QuickCheck,\n                       tasty,\n                       tasty-quickcheck,\n                       io-classes:testlib\n\n-- Since `io-sim` depends on `si-times` (`io-sim` depends on `Time`) some tests of\n-- are in `io-sim:test`: this is a good enough reason to pull `io-sim:test`\n-- into a seprate package.\ntest-suite test-si-timers\n  import:              warnings\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      si-timers/test\n  main-is:             Main.hs\n  other-modules:       Test.MonadTimer\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  build-depends:       base,\n\n                       QuickCheck,\n                       tasty,\n                       tasty-quickcheck,\n\n                       io-classes:si-timers\n";
  }