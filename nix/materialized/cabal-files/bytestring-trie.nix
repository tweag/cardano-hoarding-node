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
      identifier = { name = "bytestring-trie"; version = "0.2.7.6"; };
      license = "BSD-3-Clause";
      copyright = "2008–2025 wren gayle romano";
      maintainer = "wren@cpan.org";
      author = "wren gayle romano";
      homepage = "https://wrengr.org/software/hackage.html";
      url = "";
      synopsis = "An efficient finite map from bytestrings to values.";
      description = "An efficient finite map from bytestrings to values.\n\nThe implementation is based on big-endian patricia trees, like\n\"Data.IntMap\".  We first trie on the elements of \"Data.ByteString\"\nand then trie on the big-endian bit representation of those\nelements.  Patricia trees have efficient algorithms for union\nand other merging operations, but they're also quick for lookups\nand insertions.\n\nIf you are only interested in being able to associate strings\nto values, then you may prefer the @hashmap@ package which is\nfaster for those only needing a map-like structure.  This package\nis intended for those who need the extra capabilities that a\ntrie-like structure can offer (e.g., structure sharing to reduce\nmemory costs for highly redundant keys, taking the submap of\nall keys with a given prefix, contextual mapping, extracting\nthe minimum and maximum keys, etc.)";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
        ];
        buildable = true;
      };
      tests = {
        "test-all" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench-Regression" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
        "bench-Foldable" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
        "bench-MatchOne" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
        "bench-UnionWith" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bytestring-trie-0.2.7.6.tar.gz";
      sha256 = "96ae7afdbebf39edc1b2d546862a3cc213536556a447c9ce53015b732fbe6766";
    });
  }) // {
    package-description-override = "Cabal-Version:  2.2\n-- Cabal >=2.2 is required for:\n--    <https://cabal.readthedocs.io/en/latest/cabal-package.html#common-stanzas>\n-- Since 2.1, the Cabal-Version must be the absolutely first thing\n-- in the file, even before comments.  Also, no longer uses \">=\".\n--    <https://github.com/haskell/cabal/issues/4899>\n\n----------------------------------------------------------------\n-- wren gayle romano <wren@cpan.org>                ~ 2025-02-11\n----------------------------------------------------------------\n\nName:           bytestring-trie\nVersion:        0.2.7.6\nBuild-Type:     Simple\nStability:      provisional\nHomepage:       https://wrengr.org/software/hackage.html\nBug-Reports:    https://github.com/wrengr/bytestring-trie/issues\nAuthor:         wren gayle romano\nMaintainer:     wren@cpan.org\nCopyright:      2008–2025 wren gayle romano\n-- Cabal-2.2 requires us to say \"BSD-3-Clause\" not \"BSD3\"\nLicense:        BSD-3-Clause\nLicense-File:   LICENSE\n\nCategory:       Data, Data Structures\nSynopsis:       An efficient finite map from bytestrings to values.\nDescription:    An efficient finite map from bytestrings to values.\n    .\n    The implementation is based on big-endian patricia trees, like\n    \"Data.IntMap\".  We first trie on the elements of \"Data.ByteString\"\n    and then trie on the big-endian bit representation of those\n    elements.  Patricia trees have efficient algorithms for union\n    and other merging operations, but they're also quick for lookups\n    and insertions.\n    .\n    If you are only interested in being able to associate strings\n    to values, then you may prefer the @hashmap@ package which is\n    faster for those only needing a map-like structure.  This package\n    is intended for those who need the extra capabilities that a\n    trie-like structure can offer (e.g., structure sharing to reduce\n    memory costs for highly redundant keys, taking the submap of\n    all keys with a given prefix, contextual mapping, extracting\n    the minimum and maximum keys, etc.)\n\nExtra-source-files:\n    AUTHORS, CHANGELOG, README.md\n\n-- This package ought to work as far back as GHC 7.4.1; however,\n-- I've tightened the lower bounds on the Build-Depends to match\n-- only what we still verify via CI:\n-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history>\n-- <https://matrix.hackage.haskell.org/#/package/bytestring-lexing>\n-- <https://github.com/wrengr/bytestring-trie/actions?query=workflow%3Aci>\nTested-With:\n    GHC ==8.0.2,\n    GHC ==8.2.2,\n    GHC ==8.4.4,\n    GHC ==8.6.5,\n    GHC ==8.8.4,\n    GHC ==8.10.3,\n    GHC ==9.0.1,\n    GHC ==9.2.4,\n    GHC ==9.4.8,\n    GHC ==9.6.5,\n    GHC ==9.8.2,\n    GHC ==9.10.1,\n    GHC ==9.12.1\n\nSource-Repository head\n    Type:     git\n    Location: https://github.com/wrengr/bytestring-trie.git\n\n----------------------------------------------------------------\n-- This stanza requires Cabal>=2.2:\n--    <https://cabal.readthedocs.io/en/latest/cabal-package.html#common-stanzas>\n-- While Cabal-2.2 only ships with GHC 8.4.1, the dependencies to\n-- build it have essentially the same lower bounds as we do.  (They\n-- require bytestring>=0.9.2.1 and deepseq>=1.3)  So users of older\n-- GHC should still be able to compile it; and if they can't, then\n-- they already can't compile this package.\n--\n-- N.B., the \"import:\" field must be the first thing in a stanza.\nCommon library-build-depends\n    Default-Language: Haskell2010\n    Build-Depends: base       >= 4.9    && < 4.22\n                 , bytestring >= 0.10.8 && < 0.13\n                 , binary     >= 0.8.3  && < 0.9\n                 , deepseq    >= 1.4.2  && < 1.6\n\n-- TODO: in lieu of using CPP to expose internals to the tests/benchmarks,\n-- we should consider using:\n--    <https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs>\n\nLibrary\n    Import:          library-build-depends\n    Hs-Source-Dirs:  src\n    Exposed-Modules: Data.Trie\n                   , Data.Trie.Convenience\n                   , Data.Trie.Internal\n    Other-Modules:   Data.Trie.Internal.BitTwiddle\n                   , Data.Trie.Internal.ByteString\n                   , Data.Trie.Internal.Errors\n\n----------------------------------------------------------------\n-- See the cabal file for bytestring-lexing for more info about\n-- setting up Tasty.\nTest-Suite test-all\n    Import:         library-build-depends\n    Type:           exitcode-stdio-1.0\n    Hs-Source-Dirs: dev\n    -- NOTE: Test-Suite Main-Is is relative to Hs-Source-Dirs;\n    -- unlike Executable Main-Is, which is relative to this cabal\n    -- file's directory instead.\n    Main-Is:        Test/Main.hs\n    Other-Modules:  Shared.BaseCompat\n                 ,  Test.Utils\n                 ,  Test.Properties\n                 -- Test.Validity\n                 -- Test.ByteStringInternal\n    -- We must include our own library for the tests to use it; but\n    -- we must not give a version restriction lest Cabal give warnings.\n    -- There's also bug <https://github.com/haskell/cabal/issues/5119>:\n    -- if we don't pass -any, then Cabal will fill in \">= 0 && <= $ThisVersion\"\n    -- which will also give a warning.\n    Build-Depends:  bytestring-trie   -any\n                 ,  tasty             >= 0.10.1.2 && < 1.6\n                 ,  tasty-smallcheck  >= 0.8.0.1  && < 0.9\n                 ,  tasty-quickcheck  >= 0.8.3.2  && < 0.12\n                 -- N.B., @tasty-hunit@ is just a partial API clone;\n                 -- whereas @tasty-hunit-compat@ is a proper integration\n                 -- with HUnit itself.  Also, tasty-hunit-compat actually\n                 -- depends on tasty-hunit; if you're wanting to minimize\n                 -- dependencies.\n                 ,  tasty-hunit                      < 0.11\n                 ,  QuickCheck        >= 2.10     && < 2.16\n                 ,  smallcheck        >= 1.1.1    && < 1.3\n                 -- lazysmallcheck    >= 0.6      && < 0.7\n\n----------------------------------------------------------------\n-- Can't put the \"Type:\" field in here; or rather even if we do,\n-- it's still required in each of the Benchmark stanzas...\nCommon bench-common\n    Import:         library-build-depends\n    if impl(ghc)\n      GHC-Options:  -with-rtsopts=-A32m\n    -- TODO: this was recommended somewhere for benchmarks, but is\n    -- --nonmoving-gc actually good for our use case? (It's\n    -- concurrent-mark&sweep for the old generations; instead of\n    -- the default stop-the-world generational copying collector)\n    --if impl(ghc >= 8.10)\n    --  GHC-Options:  -with-rtsopts=--nonmoving-gc\n    if impl(ghc)\n      GHC-Options:  -Wall -O2 -rtsopts\n    Hs-Source-Dirs: dev\n    Build-Depends:  bytestring-trie -any\n                 -- TODO: try using @gauge@ instead of @criterion@,\n                 -- to reduce the transitive dependencies.\n                 -- BUG: @gauge@ depends on @basement@ which as of\n                 -- version 0.0.12 doesn't support GHC 9.2; so we'll\n                 -- have to revisit this later.\n                 --\n                 -- NOTE: If you're having issues building on ghc-9.6,\n                 -- then be sure to update your cabal info.\n                 -- The parallel-3.2.2.0 package has metadata revisions\n                 -- to allow building with base-4.18.\n                 ,  criterion\n                 ,  QuickCheck  >= 2.10  && < 2.16\n\nBenchmark bench-Regression\n    Import:         bench-common\n    Type:           exitcode-stdio-1.0\n    -- NOTE: Benchmark Main-Is behaves like Test-Suite Main-Is\n    -- (not like Executable Main-Is).\n    Main-Is:        Bench/Regression.hs\n    if impl(ghc)\n      GHC-Options:  -main-is Bench.Regression.main\n    Other-Modules:  Shared.BaseCompat\n                 ,  Bench.Foldable\n\nBenchmark bench-Foldable\n    Import:         bench-common\n    Type:           exitcode-stdio-1.0\n    Main-Is:        Bench/Foldable.hs\n    if impl(ghc)\n      GHC-Options:  -main-is Bench.Foldable.main\n    Other-Modules:  Shared.BaseCompat\n\nBenchmark bench-MatchOne\n    Import:         bench-common\n    Type:           exitcode-stdio-1.0\n    Main-Is:        Bench/MatchOne.hs\n\nBenchmark bench-UnionWith\n    Import:         bench-common\n    Type:           exitcode-stdio-1.0\n    Main-Is:        Bench/UnionWith.hs\n\n----------------------------------------------------------------\n----------------------------------------------------------- fin.\n";
  }