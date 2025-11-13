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
      identifier = { name = "io-sim"; version = "1.8.0.1"; };
      license = "Apache-2.0";
      copyright = "2022-2025 Input Output Global Inc (IOG)";
      maintainer = "Duncan Coutts duncan@well-typed.com, Marcin Szamotulski coot@coot.me";
      author = "Alexander Vieth, Duncan Coutts, John Hughes, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A pure simulator for monadic concurrency with STM.";
      description = "A pure simulator monad with support of concurrency (base & async style), stm,\nsynchronous and asynchronous exceptions, timeouts & delays, dynamic traces,\npartial order reduction, and more.\n\n= Documentation\nDocumentation is published\n[here](https://input-output-hk.github.io/io-sim/io-sim).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-classes".components.sublibs.testlib or (errorHandler.buildDepError "io-classes:testlib"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/io-sim-1.8.0.1.tar.gz";
      sha256 = "b6c0d5f2f5803b63ee945837e06c576ad09db732e950aacdd91c5843689060f7";
    });
  }) // {
    package-description-override = "cabal-version:       3.4\nname:                io-sim\nversion:             1.8.0.1\nsynopsis:            A pure simulator for monadic concurrency with STM.\ndescription:\n  A pure simulator monad with support of concurrency (base & async style), stm,\n  synchronous and asynchronous exceptions, timeouts & delays, dynamic traces,\n  partial order reduction, and more.\n\n  = Documentation\n  Documentation is published\n  [here](https://input-output-hk.github.io/io-sim/io-sim).\nlicense:             Apache-2.0\nlicense-files:       LICENSE NOTICE\ncopyright:           2022-2025 Input Output Global Inc (IOG)\nauthor:              Alexander Vieth, Duncan Coutts, John Hughes, Marcin Szamotulski\nmaintainer:          Duncan Coutts duncan@well-typed.com, Marcin Szamotulski coot@coot.me\ncategory:            Testing\nbuild-type:          Simple\nextra-doc-files:     CHANGELOG.md README.md\nbug-reports:         https://github.com/input-output-hk/io-sim/issues\ntested-with:         GHC == { 9.6, 9.8, 9.10, 9.12 }\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/io-sim\n  subdir:   io-sim\n\ncommon test-warnings\n    ghc-options: -Wall\n                 -Wcompat\n                 -Wincomplete-uni-patterns\n                 -Widentities\n                 -Wunused-packages\n                 -Wredundant-constraints\n                 -Wno-unticked-promoted-constructors\n\ncommon warnings\n    import:       test-warnings\n    ghc-options: -Wincomplete-record-updates\n                 -Wpartial-fields\n\nlibrary\n  import:              warnings\n  hs-source-dirs:      src\n  exposed-modules:     Data.List.Trace,\n                       Control.Monad.IOSim\n  other-modules:       Control.Monad.IOSim.CommonTypes,\n                       Control.Monad.IOSim.Types,\n                       Control.Monad.IOSim.Internal,\n                       Control.Monad.IOSim.InternalTypes,\n                       Control.Monad.IOSim.STM,\n                       Control.Monad.IOSimPOR.Internal,\n                       Control.Monad.IOSimPOR.Types,\n                       Control.Monad.IOSimPOR.QuickCheckUtils,\n                       Control.Monad.IOSimPOR.Timeout,\n                       Data.Deque.Strict\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  if impl(ghc < 9.4)\n    default-extensions: GADTs\n  build-depends:       base              >=4.16 && <4.22,\n                       io-classes:{io-classes,strict-stm,si-timers}\n                                        ^>=1.6 || ^>= 1.7 || ^>= 1.8,\n                       exceptions        >=0.10,\n                       containers,\n                       deepseq,\n                       hashable,\n                       nothunks,\n                       primitive         >=0.7 && <0.11,\n                       psqueues          >=0.2 && <0.3,\n                       time              >=1.9.1 && <1.13,\n                       quiet,\n                       QuickCheck,\n                       parallel\n\n\n  if flag(asserts)\n     ghc-options:      -fno-ignore-asserts\n\ntest-suite test\n  import:              test-warnings\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Control.Concurrent.Class.MonadMVar\n                       Test.Control.Concurrent.Class.MonadMVar.Strict\n                       Test.Control.Monad.STM\n                       Test.Control.Monad.Utils\n                       Test.Control.Monad.IOSim\n                       Test.Control.Monad.IOSimPOR\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  if impl(ghc < 9.4)\n    default-extensions: GADTs\n  build-depends:       base,\n                       array,\n                       containers,\n                       io-classes:{io-classes,strict-stm,si-timers,testlib},\n                       io-sim,\n                       QuickCheck,\n                       tasty,\n                       tasty-quickcheck,\n                       tasty-hunit,\n                       time\n  ghc-options:         -fno-ignore-asserts\n                       -rtsopts\n  if impl(ghc >= 9.8)\n    ghc-options:       -Wno-x-partial\n\nbenchmark bench\n  import:              warnings\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Main.hs\n  default-language:    GHC2021\n  default-extensions:  LambdaCase\n  if impl(ghc < 9.4)\n    default-extensions: GADTs\n  build-depends:       base,\n                       criterion ^>= 1.6,\n\n                       io-classes:io-classes,\n                       io-sim,\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n";
  }