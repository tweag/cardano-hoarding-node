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
      identifier = { name = "tree-diff"; version = "0.3.4"; };
      license = "GPL-2.0-or-later";
      copyright = "(c) 2017-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/tree-diff";
      url = "";
      synopsis = "Diffing of (expression) trees.";
      description = "Common diff algorithm works on list structures:\n\n@\ndiff :: Eq a => [a] -> [a] -> [Edit a]\n@\n\nThis package works on trees.\n\n@\ntreeDiff :: Eq a => Tree a -> Tree a -> Edit (EditTree a)\n@\n\nThis package also provides a way to diff arbitrary ADTs,\nusing @Generics@-derivable helpers.\n\nThis package differs from <http://hackage.haskell.org/package/gdiff gdiff>,\nin a two ways: @tree-diff@ doesn't have patch function,\nand the \"edit-script\" is a tree itself, which is useful for pretty-printing.\n\n@\n>>> prettyEditExpr $ ediff (Foo 42 [True, False] \"old\") (Foo 42 [False, False, True] \"new\")\nFoo\n\\  {fooBool = [-True, +False, False, +True],\n\\   fooInt = 42,\n\\   fooString = -\"old\" +\"new\"}\n@";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "8" && !(compiler.isGhc && compiler.version.ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
        buildable = true;
      };
      tests = {
        "tree-diff-test" = {
          depends = ([
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."trifecta" or (errorHandler.buildDepError "trifecta"))
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "8" && !(compiler.isGhc && compiler.version.ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"))) ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
          buildable = true;
        };
      };
      benchmarks = {
        "tree-diff-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tree-diff-0.3.4.tar.gz";
      sha256 = "5e9ae804207df625cf28385937a35152b12605dd4ed350c447c92db054f60e3b";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               tree-diff\nversion:            0.3.4\nx-revision:         2\nsynopsis:           Diffing of (expression) trees.\ncategory:           Data, Testing\ndescription:\n  Common diff algorithm works on list structures:\n  .\n  @\n  diff :: Eq a => [a] -> [a] -> [Edit a]\n  @\n  .\n  This package works on trees.\n  .\n  @\n  treeDiff :: Eq a => Tree a -> Tree a -> Edit (EditTree a)\n  @\n  .\n  This package also provides a way to diff arbitrary ADTs,\n  using @Generics@-derivable helpers.\n  .\n  This package differs from <http://hackage.haskell.org/package/gdiff gdiff>,\n  in a two ways: @tree-diff@ doesn't have patch function,\n  and the \"edit-script\" is a tree itself, which is useful for pretty-printing.\n  .\n  @\n  >>> prettyEditExpr $ ediff (Foo 42 [True, False] \"old\") (Foo 42 [False, False, True] \"new\")\n  Foo\n  \\  {fooBool = [-True, +False, False, +True],\n  \\   fooInt = 42,\n  \\   fooString = -\"old\" +\"new\"}\n  @\n\nhomepage:           https://github.com/phadej/tree-diff\nbug-reports:        https://github.com/phadej/tree-diff/issues\nlicense:            GPL-2.0-or-later\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2017-2021 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files:\n  ChangeLog.md\n  README.md\n\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.7\n   || ==9.8.4\n   || ==9.10.2\n   || ==9.12.2\n\nextra-source-files:\n  fixtures/exfoo.expr\n  fixtures/MyInt1.expr\n  fixtures/MyInt2.expr\n  fixtures/MyInt3.expr\n  fixtures/Positional.expr\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/tree-diff.git\n\nlibrary\n  exposed-modules:\n    Data.TreeDiff\n    Data.TreeDiff.Class\n    Data.TreeDiff.Expr\n    Data.TreeDiff.Golden\n    Data.TreeDiff.List\n    Data.TreeDiff.OMap\n    Data.TreeDiff.Parser\n    Data.TreeDiff.Pretty\n    Data.TreeDiff.QuickCheck\n    Data.TreeDiff.Tree\n\n  -- GHC boot libraries\n  build-depends:\n    , base        >=4.12.0.0  && <4.22\n    , bytestring  ^>=0.10.8.2 || ^>=0.11.0.0 || ^>=0.12.0.2\n    , containers  ^>=0.6.0.1  || ^>=0.7\n    , deepseq     ^>=1.4.4.0  || ^>=1.5.0.0\n    , parsec      ^>=3.1.13.0\n    , pretty      ^>=1.1.1.0\n    , text        ^>=1.2.3.0  || ^>=2.0      || ^>=2.1\n    , time        ^>=1.8.0.2  || ^>=1.9.3    || ^>=1.10     || ^>=1.11 || ^>=1.12 || ^>=1.14\n\n  build-depends:\n    , aeson                 ^>=2.2.0.0\n    , ansi-terminal         ^>=1.1\n    , ansi-wl-pprint        ^>=1.0.2\n    , hashable              ^>=1.4.4.0  || ^>=1.5.0.0\n    , parsers               ^>=0.12.11\n    , primitive             ^>=0.9.0.0\n    , QuickCheck            ^>=2.14.2   || ^>=2.15    || ^>=2.16.0.0\n    , scientific            ^>=0.3.8.0\n    , semialign             ^>=1.3.1\n    , strict                ^>=0.5\n    , tagged                ^>=0.8.8\n    , these                 ^>=1.2.1\n    , unordered-containers  ^>=0.2.20\n    , uuid-types            ^>=1.0.6\n    , vector                ^>=0.13.1.0\n\n  if (impl(ghc >=8) && !impl(ghc >=9.4))\n    build-depends: data-array-byte ^>=0.1.0.1\n\n  other-extensions:\n    CPP\n    ConstraintKinds\n    DefaultSignatures\n    FlexibleContexts\n    GADTs\n    RankNTypes\n    ScopedTypeVariables\n    TypeOperators\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n\ntest-suite tree-diff-test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   tests src-diff\n  ghc-options:      -Wall -threaded\n  other-modules:    RefDiffBy\n\n  -- dependencies from library\n  build-depends:\n    , ansi-terminal\n    , ansi-wl-pprint\n    , base\n    , parsec\n    , primitive\n    , QuickCheck\n    , tagged\n    , tree-diff\n    , unordered-containers\n\n  if (impl(ghc >=8) && !impl(ghc >=9.4))\n    build-depends: data-array-byte\n\n  if impl(ghc <7.5)\n    build-depends: ghc-prim\n\n  -- extra dependencies\n  build-depends:\n    , tasty             ^>=1.5\n    , tasty-golden      ^>=2.3.5\n    , tasty-quickcheck  ^>=0.10.3 || ^>=0.11\n    , trifecta          ^>=2.1.4\n\nbenchmark tree-diff-bench\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          tree-diff-bench.hs\n  hs-source-dirs:   bench\n  ghc-options:      -Wall -threaded\n\n  -- dependencies from library\n  build-depends:\n    , base\n    , deepseq\n    , tree-diff\n\n  -- extra dependencies\n  build-depends:\n    , criterion  ^>=1.6.3.0\n    , Diff       ^>=1.0\n";
  }