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
      identifier = { name = "statistics"; version = "0.16.3.0"; };
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
      url = "http://hackage.haskell.org/package/statistics-0.16.3.0.tar.gz";
      sha256 = "03ec46e6641227cf7318b7a1f87acf005d38c8cfc4e13f40ff9014a9266ba1e7";
    });
  }) // {
    package-description-override = "cabal-version:  3.0\r\nbuild-type:     Simple\r\n\r\nname:           statistics\r\nversion:        0.16.3.0\r\nx-revision: 1\r\nsynopsis:       A library of statistical types, data, and functions\r\ndescription:\r\n  This library provides a number of common functions and types useful\r\n  in statistics.  We focus on high performance, numerical robustness,\r\n  and use of good algorithms.  Where possible, we provide\r\n  references to the statistical literature.\r\n  .\r\n  The library's facilities can be divided into four broad categories:\r\n  .\r\n  * Working with widely used discrete and continuous probability\r\n    distributions.  (There are dozens of exotic distributions in use;\r\n    we focus on the most common.)\r\n  .\r\n  * Computing with sample data: quantile estimation, kernel density\r\n    estimation, histograms, bootstrap methods, significance testing,\r\n    and regression and autocorrelation analysis.\r\n  .\r\n  * Random variate generation under several different distributions.\r\n  .\r\n  * Common statistical tests for significant differences between\r\n    samples.\r\n\r\nlicense:        BSD-2-Clause\r\nlicense-file:   LICENSE\r\nhomepage:       https://github.com/haskell/statistics\r\nbug-reports:    https://github.com/haskell/statistics/issues\r\nauthor:         Bryan O'Sullivan <bos@serpentine.com>, Alexey Khudaykov <alexey.skladnoy@gmail.com>\r\nmaintainer:     Alexey Khudaykov <alexey.skladnoy@gmail.com>\r\ncopyright:      2009-2014 Bryan O'Sullivan\r\ncategory:       Math, Statistics\r\n\r\nextra-source-files:\r\n  README.markdown\r\n  changelog.md\r\n  examples/kde/KDE.hs\r\n  examples/kde/data/faithful.csv\r\n  examples/kde/kde.html\r\n  examples/kde/kde.tpl\r\n  tests/utils/Makefile\r\n  tests/utils/fftw.c\r\n\r\ntested-with:\r\n  GHC ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.8\r\n   || ==9.4.8\r\n   || ==9.6.6\r\n   || ==9.8.4\r\n   || ==9.10.1\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/statistics\r\n\r\nflag BenchPAPI\r\n  Description: Enable building of benchmarks which use instruction counters.\r\n               It requires libpapi and only works on Linux so it's protected by flag\r\n  Default: False\r\n  Manual:  True\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  exposed-modules:\r\n    Statistics.Autocorrelation\r\n    Statistics.ConfidenceInt\r\n    Statistics.Correlation\r\n    Statistics.Correlation.Kendall\r\n    Statistics.Distribution\r\n    Statistics.Distribution.Beta\r\n    Statistics.Distribution.Binomial\r\n    Statistics.Distribution.CauchyLorentz\r\n    Statistics.Distribution.ChiSquared\r\n    Statistics.Distribution.DiscreteUniform\r\n    Statistics.Distribution.Exponential\r\n    Statistics.Distribution.FDistribution\r\n    Statistics.Distribution.Gamma\r\n    Statistics.Distribution.Geometric\r\n    Statistics.Distribution.Hypergeometric\r\n    Statistics.Distribution.Laplace\r\n    Statistics.Distribution.Lognormal\r\n    Statistics.Distribution.NegativeBinomial\r\n    Statistics.Distribution.Normal\r\n    Statistics.Distribution.Poisson\r\n    Statistics.Distribution.StudentT\r\n    Statistics.Distribution.Transform\r\n    Statistics.Distribution.Uniform\r\n    Statistics.Distribution.Weibull\r\n    Statistics.Function\r\n    Statistics.Quantile\r\n    Statistics.Regression\r\n    Statistics.Resampling\r\n    Statistics.Resampling.Bootstrap\r\n    Statistics.Sample\r\n    Statistics.Sample.Internal\r\n    Statistics.Sample.Histogram\r\n    Statistics.Sample.KernelDensity\r\n    Statistics.Sample.KernelDensity.Simple\r\n    Statistics.Sample.Normalize\r\n    Statistics.Sample.Powers\r\n    Statistics.Test.ChiSquared\r\n    Statistics.Test.KolmogorovSmirnov\r\n    Statistics.Test.KruskalWallis\r\n    Statistics.Test.MannWhitneyU\r\n--    Statistics.Test.Runs\r\n    Statistics.Test.StudentT\r\n    Statistics.Test.Types\r\n    Statistics.Test.WilcoxonT\r\n    Statistics.Transform\r\n    Statistics.Types\r\n  other-modules:\r\n    Statistics.Distribution.Poisson.Internal\r\n    Statistics.Internal\r\n    Statistics.Test.Internal\r\n    Statistics.Types.Internal\r\n  build-depends: base                    >= 4.9 && < 5\r\n                 --\r\n               , math-functions          >= 0.3.4.1\r\n               , mwc-random              >= 0.15.0.0\r\n               , random                  >= 1.2\r\n                 --\r\n               , aeson                   >= 0.6.0.0\r\n               , async                   >= 2.2.2 && <2.3\r\n               , deepseq                 >= 1.1.0.2\r\n               , binary                  >= 0.5.1.0\r\n               , primitive               >= 0.3\r\n               , dense-linear-algebra    >= 0.1 && <0.2\r\n               , parallel                >= 3.2.2.0 && <3.3\r\n               , vector                  >= 0.10\r\n               , vector-algorithms       >= 0.4\r\n               , vector-th-unbox\r\n               , vector-binary-instances >= 0.2.1\r\n               , data-default-class      >= 0.1.2\r\n\r\n  -- Older GHC\r\n  if impl(ghc < 7.6)\r\n    build-depends:\r\n      ghc-prim\r\n  ghc-options: -O2 -Wall -fwarn-tabs -funbox-strict-fields\r\n\r\ntest-suite statistics-tests\r\n  default-language: Haskell2010\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is:        tests.hs\r\n  other-modules:\r\n    Tests.ApproxEq\r\n    Tests.Correlation\r\n    Tests.Distribution\r\n    Tests.ExactDistribution\r\n    Tests.Function\r\n    Tests.Helpers\r\n    Tests.KDE\r\n    Tests.Matrix\r\n    Tests.Matrix.Types\r\n    Tests.NonParametric\r\n    Tests.NonParametric.Table\r\n    Tests.Orphanage\r\n    Tests.Parametric\r\n    Tests.Serialization\r\n    Tests.Transform\r\n    Tests.Quantile\r\n  ghc-options:\r\n    -Wall -threaded -rtsopts -fsimpl-tick-factor=500\r\n  build-depends: base\r\n               , statistics\r\n               , dense-linear-algebra\r\n               , QuickCheck >= 2.7.5\r\n               , binary\r\n               , erf\r\n               , aeson\r\n               , ieee754 >= 0.7.3\r\n               , math-functions\r\n               , primitive\r\n               , tasty\r\n               , tasty-hunit\r\n               , tasty-quickcheck\r\n               , tasty-expected-failure\r\n               , vector\r\n               , vector-algorithms\r\n\r\ntest-suite statistics-doctests\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   tests\r\n  main-is:          doctest.hs\r\n  if impl(ghcjs) || impl(ghc < 8.0)\r\n    Buildable: False\r\n  -- Linker on macos prints warnings to console which confuses doctests.\r\n  -- We simply disable doctests on ma for older GHC\r\n  -- > warning: -single_module is obsolete\r\n  if os(darwin) && impl(ghc < 9.6)\r\n    buildable: False\r\n  build-depends:\r\n            base       -any\r\n          , statistics -any\r\n          , doctest    >=0.15 && <0.25\r\n\r\n-- We want to be able to build benchmarks using both tasty-bench and tasty-papi.\r\n-- They have similar API so we just create two shim modules which reexport\r\n-- definitions from corresponding library and pick one in cabal file.\r\ncommon bench-stanza\r\n  ghc-options:      -Wall\r\n  default-language: Haskell2010\r\n  build-depends: base < 5\r\n               , vector          >= 0.12.3\r\n               , statistics\r\n               , mwc-random\r\n               , tasty           >=1.3.1\r\n\r\nbenchmark statistics-bench\r\n  import:         bench-stanza\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: benchmark bench-time\r\n  main-is:        bench.hs\r\n  Other-modules:  Bench\r\n  build-depends:  tasty-bench >= 0.3\r\n\r\nbenchmark statistics-bench-papi\r\n  import:         bench-stanza\r\n  type:           exitcode-stdio-1.0\r\n  if impl(ghcjs) || !flag(BenchPAPI)\r\n     buildable: False\r\n  hs-source-dirs: benchmark bench-papi\r\n  main-is:        bench.hs\r\n  Other-modules:  Bench\r\n  build-depends:  tasty-papi >= 0.1.2\r\n";
  }