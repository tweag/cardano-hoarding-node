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
    flags = { benchpapi = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "statistics"; version = "0.16.4.0"; };
      license = "BSD-2-Clause";
      copyright = "2009-2014 Bryan O'Sullivan";
      maintainer = "Alexey Khudaykov <alexey.skladnoy@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>, Alexey Khudaykov <alexey.skladnoy@gmail.com>";
      homepage = "https://github.com/haskell/statistics";
      url = "";
      synopsis = "A library of statistical types, data, and functions";
      description = "This library provides a number of common functions and types useful\nin statistics.  We focus on high performance, numerical robustness,\nand use of good algorithms.  Where possible, we provide\nreferences to the statistical literature.\n.\nThe library's facilities can be divided into four broad categories:\n.\n* Working with widely used discrete and continuous probability\n  distributions.  (There are dozens of exotic distributions in use;\n  we focus on the most common.)\n.\n* Computing with sample data: quantile estimation, kernel density\n  estimation, histograms, bootstrap methods, significance testing,\n  and regression and autocorrelation analysis.\n.\n* Random variate generation under several different distributions.\n.\n* Common statistical tests for significant differences between\n  samples.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
          (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."dense-linear-algebra" or (errorHandler.buildDepError "dense-linear-algebra"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          (hsPkgs."vector-th-unbox" or (errorHandler.buildDepError "vector-th-unbox"))
          (hsPkgs."vector-binary-instances" or (errorHandler.buildDepError "vector-binary-instances"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
      };
      tests = {
        "statistics-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."dense-linear-algebra" or (errorHandler.buildDepError "dense-linear-algebra"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."erf" or (errorHandler.buildDepError "erf"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ieee754" or (errorHandler.buildDepError "ieee754"))
            (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ];
          buildable = true;
        };
        "statistics-doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
          ];
          buildable = (if compiler.isGhcjs && true || compiler.isGhc && compiler.version.lt "8.0"
            then false
            else true) && (if system.isOsx && (compiler.isGhc && compiler.version.lt "9.6")
            then false
            else true);
        };
      };
      benchmarks = {
        "statistics-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
          ];
          buildable = true;
        };
        "statistics-bench-papi" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-papi" or (errorHandler.buildDepError "tasty-papi"))
          ];
          buildable = if compiler.isGhcjs && true || !flags.benchpapi
            then false
            else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/statistics-0.16.4.0.tar.gz";
      sha256 = "82cbc42762bebb78b6e4e41cb1903103442b2b9a293538478d7e865494003d6b";
    });
  }) // {
    package-description-override = "cabal-version:  3.0\nbuild-type:     Simple\n\nname:           statistics\nversion:        0.16.4.0\nsynopsis:       A library of statistical types, data, and functions\ndescription:\n  This library provides a number of common functions and types useful\n  in statistics.  We focus on high performance, numerical robustness,\n  and use of good algorithms.  Where possible, we provide\n  references to the statistical literature.\n  .\n  The library's facilities can be divided into four broad categories:\n  .\n  * Working with widely used discrete and continuous probability\n    distributions.  (There are dozens of exotic distributions in use;\n    we focus on the most common.)\n  .\n  * Computing with sample data: quantile estimation, kernel density\n    estimation, histograms, bootstrap methods, significance testing,\n    and regression and autocorrelation analysis.\n  .\n  * Random variate generation under several different distributions.\n  .\n  * Common statistical tests for significant differences between\n    samples.\n\nlicense:        BSD-2-Clause\nlicense-file:   LICENSE\nhomepage:       https://github.com/haskell/statistics\nbug-reports:    https://github.com/haskell/statistics/issues\nauthor:         Bryan O'Sullivan <bos@serpentine.com>, Alexey Khudaykov <alexey.skladnoy@gmail.com>\nmaintainer:     Alexey Khudaykov <alexey.skladnoy@gmail.com>\ncopyright:      2009-2014 Bryan O'Sullivan\ncategory:       Math, Statistics\n\nextra-source-files:\n  README.markdown\n  examples/kde/KDE.hs\n  examples/kde/data/faithful.csv\n  examples/kde/kde.html\n  examples/kde/kde.tpl\n  tests/utils/Makefile\n  tests/utils/fftw.c\n\nextra-doc-files:\n  changelog.md\n\ntested-with:\n  GHC ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.7\n   || ==9.8.4\n   || ==9.10.2\n   || ==9.12.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/statistics\n\nflag BenchPAPI\n  Description: Enable building of benchmarks which use instruction counters.\n               It requires libpapi and only works on Linux so it's protected by flag\n  Default: False\n  Manual:  True\n\nlibrary\n  default-language: Haskell2010\n  exposed-modules:\n    Statistics.Autocorrelation\n    Statistics.ConfidenceInt\n    Statistics.Correlation\n    Statistics.Correlation.Kendall\n    Statistics.Distribution\n    Statistics.Distribution.Beta\n    Statistics.Distribution.Binomial\n    Statistics.Distribution.CauchyLorentz\n    Statistics.Distribution.ChiSquared\n    Statistics.Distribution.DiscreteUniform\n    Statistics.Distribution.Exponential\n    Statistics.Distribution.FDistribution\n    Statistics.Distribution.Gamma\n    Statistics.Distribution.Geometric\n    Statistics.Distribution.Hypergeometric\n    Statistics.Distribution.Laplace\n    Statistics.Distribution.Lognormal\n    Statistics.Distribution.NegativeBinomial\n    Statistics.Distribution.Normal\n    Statistics.Distribution.Poisson\n    Statistics.Distribution.StudentT\n    Statistics.Distribution.Transform\n    Statistics.Distribution.Uniform\n    Statistics.Distribution.Weibull\n    Statistics.Function\n    Statistics.Quantile\n    Statistics.Regression\n    Statistics.Resampling\n    Statistics.Resampling.Bootstrap\n    Statistics.Sample\n    Statistics.Sample.Internal\n    Statistics.Sample.Histogram\n    Statistics.Sample.KernelDensity\n    Statistics.Sample.KernelDensity.Simple\n    Statistics.Sample.Normalize\n    Statistics.Sample.Powers\n    Statistics.Test.Bartlett\n    Statistics.Test.Levene\n    Statistics.Test.ChiSquared\n    Statistics.Test.KolmogorovSmirnov\n    Statistics.Test.KruskalWallis\n    Statistics.Test.MannWhitneyU\n--    Statistics.Test.Runs\n    Statistics.Test.StudentT\n    Statistics.Test.Types\n    Statistics.Test.WilcoxonT\n    Statistics.Transform\n    Statistics.Types\n  other-modules:\n    Statistics.Distribution.Poisson.Internal\n    Statistics.Internal\n    Statistics.Test.Internal\n    Statistics.Types.Internal\n  build-depends: base                    >= 4.9 && < 5\n                 --\n               , math-functions          >= 0.3.4.1\n               , mwc-random              >= 0.15.0.0\n               , random                  >= 1.2\n                 --\n               , aeson                   >= 0.6.0.0\n               , async                   >= 2.2.2 && <2.3\n               , deepseq                 >= 1.1.0.2\n               , binary                  >= 0.5.1.0\n               , primitive               >= 0.3\n               , dense-linear-algebra    >= 0.1 && <0.2\n               , parallel                >= 3.2.2.0 && <3.4\n               , vector                  >= 0.10\n               , vector-algorithms       >= 0.4\n               , vector-th-unbox\n               , vector-binary-instances >= 0.2.1\n               , data-default-class      >= 0.1.2\n\n  -- Older GHC\n  if impl(ghc < 7.6)\n    build-depends:\n      ghc-prim\n  ghc-options: -O2 -Wall -fwarn-tabs -funbox-strict-fields\n\ntest-suite statistics-tests\n  default-language: Haskell2010\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is:        tests.hs\n  other-modules:\n    Tests.ApproxEq\n    Tests.Correlation\n    Tests.Distribution\n    Tests.ExactDistribution\n    Tests.Function\n    Tests.Helpers\n    Tests.KDE\n    Tests.Matrix\n    Tests.Matrix.Types\n    Tests.NonParametric\n    Tests.NonParametric.Table\n    Tests.Orphanage\n    Tests.Parametric\n    Tests.Serialization\n    Tests.Transform\n    Tests.Quantile\n  ghc-options:\n    -Wall -threaded -rtsopts -fsimpl-tick-factor=500\n  if impl(ghc >= 9.8)\n    ghc-options: -Wno-x-partial\n  build-depends: base\n               , statistics\n               , dense-linear-algebra\n               , QuickCheck >= 2.7.5\n               , binary\n               , erf\n               , aeson\n               , ieee754 >= 0.7.3\n               , math-functions\n               , primitive\n               , tasty\n               , tasty-hunit\n               , tasty-quickcheck\n               , tasty-expected-failure\n               , vector\n               , vector-algorithms\n\ntest-suite statistics-doctests\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          doctest.hs\n  if impl(ghcjs) || impl(ghc < 8.0)\n    Buildable: False\n  -- Linker on macos prints warnings to console which confuses doctests.\n  -- We simply disable doctests on ma for older GHC\n  -- > warning: -single_module is obsolete\n  if os(darwin) && impl(ghc < 9.6)\n    buildable: False\n  build-depends:\n            base       -any\n          , statistics -any\n          , doctest    >=0.15 && <0.25\n\n-- We want to be able to build benchmarks using both tasty-bench and tasty-papi.\n-- They have similar API so we just create two shim modules which reexport\n-- definitions from corresponding library and pick one in cabal file.\ncommon bench-stanza\n  ghc-options:      -Wall\n  default-language: Haskell2010\n  build-depends: base < 5\n               , vector          >= 0.12.3\n               , statistics\n               , mwc-random\n               , tasty           >=1.3.1\n\nbenchmark statistics-bench\n  import:         bench-stanza\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: benchmark bench-time\n  main-is:        Main.hs\n  Other-modules:  Bench\n  build-depends:  tasty-bench >= 0.3\n\nbenchmark statistics-bench-papi\n  import:         bench-stanza\n  type:           exitcode-stdio-1.0\n  if impl(ghcjs) || !flag(BenchPAPI)\n     buildable: False\n  hs-source-dirs: benchmark bench-papi\n  main-is:        Main.hs\n  Other-modules:  Bench\n  build-depends:  tasty-papi >= 0.1.2\n";
  }