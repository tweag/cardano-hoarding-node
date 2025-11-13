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
      identifier = { name = "quickcheck-state-machine"; version = "0.10.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2017-2018, ATS Advanced Telematic Systems GmbH;\n2018-2019, HERE Europe B.V.;\n2019-2024, Stevan Andjelkovic.";
      maintainer = "Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>";
      author = "Stevan Andjelkovic";
      homepage = "https://github.com/stevana/quickcheck-state-machine#readme";
      url = "";
      synopsis = "Test monadic programs using state machine based models";
      description = "See README at <https://github.com/stevana/quickcheck-state-machine#readme>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."quickcheck-state-machine".components.sublibs.no-vendored-treediff or (errorHandler.buildDepError "quickcheck-state-machine:no-vendored-treediff"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      sublibs = {
        "no-vendored-treediff" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."filelock" or (errorHandler.buildDepError "filelock"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
            (hsPkgs."persistent-postgresql" or (errorHandler.buildDepError "persistent-postgresql"))
            (hsPkgs."persistent-sqlite" or (errorHandler.buildDepError "persistent-sqlite"))
            (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-state-machine-0.10.2.tar.gz";
      sha256 = "ac04a7c0adabd7e71adb2654381583e0d328aa6e8c8a0aa17d18cb7385209a05";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            quickcheck-state-machine\nversion:         0.10.2\nsynopsis:        Test monadic programs using state machine based models\ndescription:\n  See README at <https://github.com/stevana/quickcheck-state-machine#readme>\n\nhomepage:        https://github.com/stevana/quickcheck-state-machine#readme\nlicense:         BSD-3-Clause\nlicense-file:    LICENSE\nauthor:          Stevan Andjelkovic\nmaintainer:      Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>\ncopyright:\n  Copyright (C) 2017-2018, ATS Advanced Telematic Systems GmbH;\n  2018-2019, HERE Europe B.V.;\n  2019-2024, Stevan Andjelkovic.\n\ncategory:        Testing\nbuild-type:      Simple\nextra-doc-files:\n  CHANGELOG.md\n  CONTRIBUTING.md\n  README.md\n\ntested-with:\n  GHC ==8.8.4 || ==8.10.7 || ==9.2.8 || ==9.4.8 || ==9.6.5 || ==9.8.2\n\n-- Due to `tree-diff` being `GPL`, this library makes use of an interface\n-- (@CanDiff@) to diff models. This can be implemented with the vendored\n-- tree-diff or with the upstream one, but we only provide the former in the\n-- main library of the package below. See\n-- https://github.com/stevana/quickcheck-state-machine#readme for how to depend\n-- on either version.\nlibrary no-vendored-treediff\n  visibility:       public\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  exposed-modules:\n    Test.StateMachine\n    Test.StateMachine.BoxDrawer\n    Test.StateMachine.ConstructorName\n    Test.StateMachine.Diffing\n    Test.StateMachine.DotDrawing\n    Test.StateMachine.Labelling\n    Test.StateMachine.Lockstep.Auxiliary\n    Test.StateMachine.Lockstep.NAry\n    Test.StateMachine.Lockstep.Simple\n    Test.StateMachine.Logic\n    Test.StateMachine.Parallel\n    Test.StateMachine.Sequential\n    Test.StateMachine.Types\n    Test.StateMachine.Types.Environment\n    Test.StateMachine.Types.GenSym\n    Test.StateMachine.Types.History\n    Test.StateMachine.Types.Rank2\n    Test.StateMachine.Types.References\n    Test.StateMachine.Utils\n    Test.StateMachine.Z\n\n  -- GHC boot library dependencies:\n  -- (https://gitlab.haskell.org/ghc/ghc/-/blob/master/packages)\n  build-depends:\n    , base        >=4.10    && <5\n    , containers  >=0.5.7.1 && <0.8\n    , directory   >=1.0.0.0 && <1.4\n    , exceptions  >=0.8.3   && <0.11\n    , filepath    >=1.0     && <1.6\n    , mtl         >=2.2.1   && <2.4\n    , text        >=1.2.3.1 && <2.2\n    , time        >=1.7     && <1.15\n\n  build-depends:\n    , graphviz                     >=2999.20.0.3 && <2999.21\n    , pretty-show                  >=1.6.16      && <1.11\n    , prettyprinter                ^>=1.7.1\n    , prettyprinter-ansi-terminal  ^>=1.1.3\n    , QuickCheck                   >=2.12        && <2.17\n    , random                       >=1.1         && <1.3\n    , sop-core                     >=0.5.0.2     && <0.6\n    , split                        >=0.2.3.5     && <0.3\n    , unliftio                     >=0.2.7.0     && <0.3\n\n  default-language: Haskell2010\n\nlibrary\n  hs-source-dirs:     tree-diff\n  ghc-options:        -Wall\n  exposed-modules:\n    Test.StateMachine.TreeDiff\n    Test.StateMachine.TreeDiff.Class\n    Test.StateMachine.TreeDiff.Diffing\n    Test.StateMachine.TreeDiff.Expr\n    Test.StateMachine.TreeDiff.List\n    Test.StateMachine.TreeDiff.Pretty\n    Test.StateMachine.TreeDiff.Tree\n\n  reexported-modules:\n    Test.StateMachine,\n    Test.StateMachine.BoxDrawer,\n    Test.StateMachine.ConstructorName,\n    Test.StateMachine.Diffing,\n    Test.StateMachine.DotDrawing,\n    Test.StateMachine.Labelling,\n    Test.StateMachine.Lockstep.Auxiliary,\n    Test.StateMachine.Lockstep.NAry,\n    Test.StateMachine.Lockstep.Simple,\n    Test.StateMachine.Logic,\n    Test.StateMachine.Parallel,\n    Test.StateMachine.Sequential,\n    Test.StateMachine.Types,\n    Test.StateMachine.Types.Environment,\n    Test.StateMachine.Types.GenSym,\n    Test.StateMachine.Types.History,\n    Test.StateMachine.Types.Rank2,\n    Test.StateMachine.Types.References,\n    Test.StateMachine.Utils,\n    Test.StateMachine.Z\n\n  -- GHC boot library dependencies:\n  -- (https://gitlab.haskell.org/ghc/ghc/-/blob/master/packages)\n  build-depends:\n    , base\n    , containers\n    , text\n    , time\n\n  build-depends:\n    , prettyprinter\n    , prettyprinter-ansi-terminal\n    , QuickCheck\n    , sop-core\n\n  -- tree-diff dependencies:\n  build-depends:\n    , base-compat                                    >=0.9.3    && <0.15\n    , bytestring                                     >=0.10.4.0 && <0.13\n    , generics-sop                                   >=0.3.1.0  && <0.6\n    , MemoTrie                                       >=0.6.8    && <0.7\n    , pretty                                         >=1.1.1.1  && <1.2\n    , quickcheck-state-machine:no-vendored-treediff\n    , vector                                         >=0.12.0.1 && <0.14\n\n  default-language:   Haskell2010\n\ntest-suite test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          Spec.hs\n  build-depends:\n    , array                     >=0.5.4.0  && <0.6\n    , base\n    , bifunctors                >=5.5.7    && <5.7\n    , bytestring\n    , containers\n    , directory\n    , doctest                   >=0.16.2   && <0.23\n    , filelock                  >=0.1.1.4  && <0.2\n    , filepath\n    , hashable                  >=1.3.0.0  && <1.6\n    , hashtables                >=1.2.3.4  && <1.4\n    , http-client               >=0.6.4.1  && <0.8\n    , monad-logger              >=0.3.32   && <0.4\n    , mtl\n    , network                   >=3.1.1.1  && <3.3\n    , persistent                >=2.10.5.2 && <2.15\n    , persistent-postgresql     >=2.10.1.2 && <2.14\n    , persistent-sqlite         >=2.10.6.2 && <2.14\n    , postgresql-simple         >=0.6.2    && <0.8\n    , pretty-show\n    , process\n    , QuickCheck\n    , quickcheck-instances      >=0.3.22   && <0.4\n    , quickcheck-state-machine\n    , random\n    , resource-pool             >=0.2.3.2  && <0.5\n    , resourcet                 >=1.2.3    && <1.4\n    , servant-client            >=0.16.0.1 && <0.21\n    , servant-server            >=0.16.2   && <0.21\n    , split                     >=0.2.3.5  && <0.3\n    , stm                       >=2.5.0.0  && <2.6\n    , strict                    >=0.3.2    && <0.6\n    , string-conversions        >=0.4.0.1  && <0.5\n    , tasty                     >=1.2.3    && <1.6\n    , tasty-hunit               >=0.10.0.2 && <0.11\n    , tasty-quickcheck          >=0.10.1.1 && <0.12\n    , text\n    , unliftio\n    , unliftio-core             >=0.1.2.0  && <0.3\n    , vector\n    , warp                      >=3.3.9    && <3.5\n\n  other-modules:\n    Bookstore\n    CircularBuffer\n    Cleanup\n    CrudWebserverDb\n    DieHard\n    Echo\n    ErrorEncountered\n    Hanoi\n    IORefs\n    MemoryReference\n    Mock\n    Overflow\n    ProcessRegistry\n    Schema\n    ShrinkingProps\n    SQLite\n    TicketDispenser\n    UnionFind\n\n  ghc-options:\n    -threaded -rtsopts -with-rtsopts=-N -fno-ignore-asserts -Wall\n\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/stevana/quickcheck-state-machine\n\nsource-repository this\n  type:     git\n  location: https://github.com/stevana/quickcheck-state-machine\n  tag:      v0.10.2\n";
  }