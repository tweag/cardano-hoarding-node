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
      specVersion = "3.0";
      identifier = { name = "fs-sim"; version = "0.4.1.0"; };
      license = "Apache-2.0";
      copyright = "2019-2024 Input Output Global Inc (IOG)";
      maintainer = "operations@iohk.io, Joris Dral (joris@well-typed.com)";
      author = "IOG Engineering Team";
      homepage = "https://github.com/input-output-hk/fs-sim";
      url = "";
      synopsis = "Simulated file systems";
      description = "Simulated file systems.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."safe-wild-cards" or (errorHandler.buildDepError "safe-wild-cards"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "fs-sim-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fs-sim-0.4.1.0.tar.gz";
      sha256 = "2fe9ff2571fe0e953b4f307a8ed5037fb2ecb2b2c6a8b3d55e0ddb667c6f2f8e";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            fs-sim\nversion:         0.4.1.0\nsynopsis:        Simulated file systems\ndescription:     Simulated file systems.\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:       2019-2024 Input Output Global Inc (IOG)\nauthor:          IOG Engineering Team\nmaintainer:      operations@iohk.io, Joris Dral (joris@well-typed.com)\nhomepage:        https://github.com/input-output-hk/fs-sim\nbug-reports:     https://github.com/input-output-hk/fs-sim/issues\ncategory:        Testing\nbuild-type:      Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ntested-with:     GHC ==9.2 || ==9.4 || ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/fs-sim\n  subdir:   fs-sim\n\nsource-repository this\n  type:     git\n  location: https://github.com/input-output-hk/fs-sim\n  subdir:   fs-sim\n  tag:      fs-sim-0.4.1.0\n\nlibrary\n  hs-source-dirs:   src\n  exposed-modules:\n    System.FS.Sim.Error\n    System.FS.Sim.FsTree\n    System.FS.Sim.MockFS\n    System.FS.Sim.Prim\n    System.FS.Sim.STM\n    System.FS.Sim.Stream\n\n  default-language: Haskell2010\n  build-depends:\n    , base                   >=4.16  && <4.22\n    , base16-bytestring      ^>=0.1  || ^>=1.0\n    , bytestring             ^>=0.10 || ^>=0.11 || ^>=0.12\n    , containers             ^>=0.5  || ^>=0.6  || ^>=0.7     || ^>=0.8\n    , fs-api                 ^>=0.4\n    , io-classes             ^>=1.6  || ^>=1.7  || ^>=1.8.0.1\n    , io-classes:strict-stm\n    , mtl                    ^>=2.2  || ^>=2.3\n    , primitive              ^>=0.9\n    , QuickCheck             ^>=2.13 || ^>=2.14 || ^>=2.15    || ^>=2.16\n    , safe-wild-cards        ^>=1.0\n    , text                   ^>=1.2  || ^>=2.0  || ^>=2.1\n\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n\ntest-suite fs-sim-test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          Main.hs\n  other-modules:\n    Test.System.FS.Sim.Error\n    Test.System.FS.Sim.FsTree\n    Test.System.FS.Sim.Stream\n    Test.System.FS.StateMachine\n    Test.Util\n    Test.Util.RefEnv\n    Test.Util.WithEntryCounter\n\n  default-language: Haskell2010\n  build-depends:\n    , base\n    , bifunctors\n    , bytestring\n    , containers\n    , deepseq\n    , fs-api\n    , fs-sim\n    , generics-sop\n    , io-classes:strict-stm\n    , pretty-show\n    , primitive\n    , QuickCheck\n    , quickcheck-state-machine  >=0.10\n    , random\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n    , temporary\n    , text\n\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n    -fno-ignore-asserts\n";
  }