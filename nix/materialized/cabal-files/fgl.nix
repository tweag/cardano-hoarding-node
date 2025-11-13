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
    flags = { containers042 = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "fgl"; version = "5.8.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "athas@sigkill.dk";
      author = "Martin Erwig, Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "Martin Erwig's Functional Graph Library";
      description = "An inductive representation of manipulating graph data structures.\n\nOriginal website can be found at <http://web.engr.oregonstate.edu/~erwig/fgl/haskell>.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
        ] ++ (if flags.containers042
          then [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ]
          else [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ])) ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "7.2" && (compiler.isGhc && compiler.version.lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
      };
      tests = {
        "fgl-tests" = {
          depends = [
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "fgl-benchmark" = {
          depends = [
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."microbench" or (errorHandler.buildDepError "microbench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
          buildable = if flags.containers042 then true else false;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fgl-5.8.3.0.tar.gz";
      sha256 = "a4ca15b162068a6cd8fd8685e2c1231ace4a24d56b2424b8e3f8988ff1ab63c1";
    });
  }) // {
    package-description-override = "name:          fgl\r\nversion:       5.8.3.0\r\nx-revision: 1\r\nlicense:       BSD3\r\nlicense-file:  LICENSE\r\nauthor:        Martin Erwig, Ivan Lazar Miljenovic\r\nmaintainer:    athas@sigkill.dk\r\ncategory:      Data Structures, Graphs\r\nsynopsis:      Martin Erwig's Functional Graph Library\r\n\r\ndescription:   {\r\nAn inductive representation of manipulating graph data structures.\r\n.\r\nOriginal website can be found at <http://web.engr.oregonstate.edu/~erwig/fgl/haskell>.\r\n}\r\ncabal-version: >= 1.10\r\nbuild-type:    Simple\r\nextra-source-files:\r\n               ChangeLog\r\n\r\ntested-with:   GHC == 7.2.2,  GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4,\r\n               GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3,\r\n               GHC == 8.6.2,  GHC == 8.8.2, GHC == 8.10.7, GHC == 9.0.2,\r\n               GHC == 9.2.4,  GHC == 9.4.4, GHC == 9.6.3, GHC == 9.8.1\r\n\r\nsource-repository head\r\n    type:         git\r\n    location:     https://github.com/haskell/fgl.git\r\n\r\nflag containers042 {\r\n    manual:  False\r\n    default: True\r\n}\r\n\r\nlibrary {\r\n    default-language: Haskell98\r\n\r\n    exposed-modules:\r\n        Data.Graph.Inductive.Internal.Heap,\r\n        Data.Graph.Inductive.Internal.Queue,\r\n        Data.Graph.Inductive.Internal.RootPath,\r\n        Data.Graph.Inductive.Internal.Thread,\r\n        Data.Graph.Inductive.Basic,\r\n        Data.Graph.Inductive.Example,\r\n        Data.Graph.Inductive.Graph,\r\n        Data.Graph.Inductive.Monad,\r\n        Data.Graph.Inductive.NodeMap,\r\n        Data.Graph.Inductive.PatriciaTree,\r\n        Data.Graph.Inductive.Query,\r\n        Data.Graph.Inductive.Tree,\r\n        Data.Graph.Inductive.Monad.IOArray,\r\n        Data.Graph.Inductive.Monad.STArray,\r\n        Data.Graph.Inductive.Query.ArtPoint,\r\n        Data.Graph.Inductive.Query.BCC,\r\n        Data.Graph.Inductive.Query.BFS,\r\n        Data.Graph.Inductive.Query.DFS,\r\n        Data.Graph.Inductive.Query.Dominators,\r\n        Data.Graph.Inductive.Query.GVD,\r\n        Data.Graph.Inductive.Query.Indep,\r\n        Data.Graph.Inductive.Query.MST,\r\n        Data.Graph.Inductive.Query.MaxFlow,\r\n        Data.Graph.Inductive.Query.MaxFlow2,\r\n        Data.Graph.Inductive.Query.Monad,\r\n        Data.Graph.Inductive.Query.SP,\r\n        Data.Graph.Inductive.Query.TransClos,\r\n        Data.Graph.Inductive\r\n\r\n    other-modules:\r\n        Paths_fgl\r\n\r\n    build-depends:    base >= 4.3 && < 5\r\n                    , transformers\r\n                    , array\r\n\r\n    if flag(containers042)\r\n        build-depends:    containers >= 0.4.2\r\n                        , deepseq >= 1.1.0.0 && < 1.6\r\n    else\r\n        build-depends:    containers < 0.4.2\r\n\r\n    if impl(ghc >= 7.2) && impl(ghc < 7.6)\r\n        build-depends:\r\n            ghc-prim\r\n\r\n    ghc-options:      -Wall\r\n\r\n}\r\n\r\ntest-suite fgl-tests {\r\n    default-language: Haskell98\r\n\r\n    type:             exitcode-stdio-1.0\r\n\r\n    build-depends:    fgl\r\n                    , base\r\n                    , QuickCheck >= 2.8 && < 2.16\r\n                    , hspec >= 2.1 && < 2.12\r\n                    , containers\r\n\r\n    hs-source-dirs:   test\r\n                      fgl-arbitrary\r\n\r\n    main-is:          TestSuite.hs\r\n\r\n    other-modules:    Data.Graph.Inductive.Arbitrary\r\n                    , Data.Graph.Inductive.Graph.Properties\r\n                    , Data.Graph.Inductive.Proxy\r\n                    , Data.Graph.Inductive.Query.Properties\r\n\r\n    ghc-options:      -Wall\r\n    if impl(ghc >= 8.0)\r\n      ghc-options:    -Wall -Wno-star-is-type\r\n\r\n}\r\n\r\nbenchmark fgl-benchmark {\r\n    if flag(containers042)\r\n        buildable:    True\r\n    else\r\n        buildable:    False\r\n\r\n    default-language: Haskell98\r\n\r\n    type:             exitcode-stdio-1.0\r\n\r\n    hs-source-dirs:   test\r\n\r\n    main-is:          benchmark.hs\r\n\r\n    other-modules:    Data.Graph.Inductive.Proxy\r\n\r\n    build-depends:    fgl\r\n                    , base\r\n                    , microbench\r\n                    , deepseq\r\n\r\n    ghc-options:      -Wall\r\n\r\n}\r\n";
  }