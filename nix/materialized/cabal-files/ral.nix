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
    flags = { adjunctions = true; distributive = true; semigroupoids = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "ral"; version = "0.2.2"; };
      license = "GPL-2.0-or-later";
      copyright = "(c) 2019-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/vec";
      url = "";
      synopsis = "Random access lists";
      description = "This package provides ordinary random access list, 'RAList', and also\na length indexed variant, 'RAVec'.\n\nThe data structure allows fast cons-operation (like ordinary list) but also fast random access (like non-functional arrays).\n\nFor @lens@ or @optics@ support see [ral-lens](https://hackage.haskell.org/package/ral-lens) and [ral-optics](https://hackage.haskell.org/package/ral-optics) packages respectively.\n\n=== Similar packages\n\nThese packages don't provide length-indexed variants, and their 'RAList' has\nopaque structure.\n\n* https://hackage.haskell.org/package/ralist\n* https://hackage.haskell.org/package/random-access-list";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."bin" or (errorHandler.buildDepError "bin"))
          (hsPkgs."fin" or (errorHandler.buildDepError "fin"))
          (hsPkgs."boring" or (errorHandler.buildDepError "boring"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
        ] ++ pkgs.lib.optionals (flags.distributive) ([
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
        ] ++ pkgs.lib.optional (flags.adjunctions) (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions")))) ++ pkgs.lib.optional (flags.semigroupoids) (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"));
        buildable = true;
      };
      benchmarks = {
        "ral-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ral-0.2.2.tar.gz";
      sha256 = "46c70ee0cae8625edf2aaf47ddcad9d582f5b3841b75b2dee4066156130d60b0";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               ral\nversion:            0.2.2\nx-revision:         2\nsynopsis:           Random access lists\ncategory:           Data, Dependent Types, Singletons\ndescription:\n  This package provides ordinary random access list, 'RAList', and also\n  a length indexed variant, 'RAVec'.\n  .\n  The data structure allows fast cons-operation (like ordinary list) but also fast random access (like non-functional arrays).\n  .\n  For @lens@ or @optics@ support see [ral-lens](https://hackage.haskell.org/package/ral-lens) and [ral-optics](https://hackage.haskell.org/package/ral-optics) packages respectively.\n  .\n  === Similar packages\n  .\n  These packages don't provide length-indexed variants, and their 'RAList' has\n  opaque structure.\n  .\n  * https://hackage.haskell.org/package/ralist\n  * https://hackage.haskell.org/package/random-access-list\n\nhomepage:           https://github.com/phadej/vec\nbug-reports:        https://github.com/phadej/vec/issues\nlicense:            GPL-2.0-or-later\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2019-2021 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/vec.git\n  subdir:   ral\n\nflag adjunctions\n  description: Depend on @adjunctions@ to provide its instances\n  manual:      True\n  default:     True\n\nflag distributive\n  description:\n    Depend on @distributive@ to provide its instances. Turning on, disables @adjunctions@ too.\n\n  manual:      True\n  default:     True\n\nflag semigroupoids\n  description:\n    Depend on @semigroupoids@ to provide its instances, and `traverse1`.\n\n  manual:      True\n  default:     True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall -fprint-explicit-kinds\n  exposed-modules:\n    Data.RAList\n    Data.RAList.NonEmpty\n    Data.RAList.Tree\n    Data.RAVec\n    Data.RAVec.NonEmpty\n    Data.RAVec.Tree\n    Data.RAVec.Tree.DF\n\n  other-modules:\n    Data.RAList.Internal\n    Data.RAList.NonEmpty.Internal\n    Data.RAList.Tree.Internal\n    TrustworthyCompat\n\n  -- GHC boot libs\n  build-depends:\n    , base     >=4.12.0.0 && <4.22\n    , deepseq  >=1.4.4.0  && <1.6\n\n  -- siblings\n  build-depends:\n    , bin  ^>=0.1.1.4\n    , fin  ^>=0.3.1\n\n  -- other dependencies\n  build-depends:\n    , boring               ^>=0.2.2\n    , hashable             ^>=1.4.4.0 || ^>=1.5.0.0\n    , indexed-traversable  ^>=0.1.4\n    , QuickCheck           ^>=2.14.2  || ^>=2.15\n\n  if flag(distributive)\n    build-depends: distributive ^>=0.6.2\n\n    if flag(adjunctions)\n      build-depends: adjunctions ^>=4.4.2\n\n  if flag(semigroupoids)\n    build-depends: semigroupoids ^>=6.0.1\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n-- dump-core\n--  build-depends: dump-core\n--  ghc-options: -fplugin=DumpCore -fplugin-opt DumpCore:core-html\n\nbenchmark ral-bench\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  hs-source-dirs:   bench\n  ghc-options:      -Wall -fprint-explicit-kinds -threaded\n  main-is:          Bench.hs\n  build-depends:\n    , base\n    , criterion\n    , ral\n    , vector\n";
  }