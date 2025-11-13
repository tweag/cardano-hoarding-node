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
    flags = { fast = false; embed-data-files = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "criterion"; version = "1.6.4.1"; };
      license = "BSD-3-Clause";
      copyright = "2009-present Bryan O'Sullivan and others";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/criterion";
      url = "";
      synopsis = "Robust, reliable performance measurement and analysis";
      description = "This library provides a powerful but simple way to measure software\nperformance.  It consists of both a framework for executing and\nanalysing benchmarks and a set of driver functions that makes it\neasy to build and run benchmarks, and to analyse their results.\n\nThe fastest way to get started is to read the\n<https://github.com/haskell/criterion/blob/master/README.markdown#tutorial online tutorial>,\nfollowed by the documentation and examples in the \"Criterion.Main\"\nmodule.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."binary-orphans" or (errorHandler.buildDepError "binary-orphans"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
          (hsPkgs."js-chart" or (errorHandler.buildDepError "js-chart"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
        ] ++ pkgs.lib.optionals (flags.embed-data-files) [
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
        ];
        buildable = true;
      };
      exes = {
        "criterion-report" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          ];
          buildable = true;
        };
      };
      tests = {
        "sanity" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          buildable = true;
        };
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          ];
          buildable = true;
        };
        "cleanup" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/criterion-1.6.4.1.tar.gz";
      sha256 = "4ea56f6c5768c65a321e5546d077e764653994affd86a4d3e036d7883fb7aa0d";
    });
  }) // {
    package-description-override = "name:           criterion\r\nversion:        1.6.4.1\r\nsynopsis:       Robust, reliable performance measurement and analysis\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\r\ncopyright:      2009-present Bryan O'Sullivan and others\r\ncategory:       Development, Performance, Testing, Benchmarking\r\nhomepage:       https://github.com/haskell/criterion\r\nbug-reports:    https://github.com/haskell/criterion/issues\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\nextra-source-files:\r\n  README.markdown\r\n  changelog.md\r\n  examples/LICENSE\r\n  examples/*.cabal\r\n  examples/*.hs\r\n  www/fibber.html\r\n  www/report.html\r\n  www/fibber-screenshot.png\r\ntested-with:\r\n  GHC==8.0.2,\r\n  GHC==8.2.2,\r\n  GHC==8.4.4,\r\n  GHC==8.6.5,\r\n  GHC==8.8.4,\r\n  GHC==8.10.7,\r\n  GHC==9.0.2,\r\n  GHC==9.2.8,\r\n  GHC==9.4.8,\r\n  GHC==9.6.7,\r\n  GHC==9.8.4,\r\n  GHC==9.10.2,\r\n  GHC==9.12.2\r\n\r\ndata-files:\r\n  templates/*.css\r\n  templates/*.tpl\r\n  templates/*.js\r\n\r\ndescription:\r\n  This library provides a powerful but simple way to measure software\r\n  performance.  It consists of both a framework for executing and\r\n  analysing benchmarks and a set of driver functions that makes it\r\n  easy to build and run benchmarks, and to analyse their results.\r\n  .\r\n  The fastest way to get started is to read the\r\n  <https://github.com/haskell/criterion/blob/master/README.markdown#tutorial online tutorial>,\r\n  followed by the documentation and examples in the \"Criterion.Main\"\r\n  module.\r\n\r\nflag fast\r\n  description: compile without optimizations\r\n  default: False\r\n  manual: True\r\n\r\nflag embed-data-files\r\n  description: Embed the data files in the binary for a relocatable executable.\r\n               (Warning: This will increase the executable size significantly.)\r\n  default: False\r\n  manual: True\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Criterion\r\n    Criterion.Analysis\r\n    Criterion.IO\r\n    Criterion.IO.Printf\r\n    Criterion.Internal\r\n    Criterion.Main\r\n    Criterion.Main.Options\r\n    Criterion.Monad\r\n    Criterion.Report\r\n    Criterion.Types\r\n\r\n  other-modules:\r\n    Criterion.Monad.Internal\r\n\r\n  other-modules:\r\n    Paths_criterion\r\n\r\n  build-depends:\r\n    aeson >= 2 && < 2.3,\r\n    base >= 4.9 && < 5,\r\n    base-compat-batteries >= 0.10 && < 0.15,\r\n    binary >= 0.8.3.0,\r\n    binary-orphans >= 1.0.1 && < 1.1,\r\n    bytestring >= 0.10.8.1 && < 1.0,\r\n    cassava >= 0.3.0.0,\r\n    code-page,\r\n    containers,\r\n    criterion-measurement >= 0.2 && < 0.3,\r\n    deepseq >= 1.1.0.0,\r\n    directory,\r\n    exceptions >= 0.8.2 && < 0.11,\r\n    filepath,\r\n    Glob >= 0.7.2,\r\n    microstache >= 1.0.1 && < 1.1,\r\n    js-chart >= 2.9.4 && < 3,\r\n    mtl >= 2,\r\n    mwc-random >= 0.8.0.3,\r\n    optparse-applicative >= 0.18 && < 0.20,\r\n    parsec >= 3.1.0,\r\n    prettyprinter >= 1.7 && < 1.8,\r\n    prettyprinter-ansi-terminal >= 1.1 && < 1.2,\r\n    statistics >= 0.14 && < 0.17,\r\n    text >= 0.11,\r\n    time,\r\n    transformers,\r\n    transformers-compat >= 0.6.4,\r\n    vector >= 0.7.1,\r\n    vector-algorithms >= 0.4\r\n\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall -funbox-strict-fields -Wtabs\r\n  if flag(fast)\r\n    ghc-options: -O0\r\n  else\r\n    ghc-options: -O2\r\n\r\n  if flag(embed-data-files)\r\n    other-modules: Criterion.EmbeddedData\r\n    build-depends: file-embed < 0.1,\r\n                   template-haskell\r\n    cpp-options: \"-DEMBED\"\r\n\r\nExecutable criterion-report\r\n  Default-Language:     Haskell2010\r\n  GHC-Options:          -Wall -rtsopts\r\n  Main-Is:              Report.hs\r\n  Other-Modules:        Options\r\n                        Paths_criterion\r\n  Hs-Source-Dirs:       app\r\n\r\n  Build-Depends:\r\n    base,\r\n    base-compat-batteries,\r\n    criterion,\r\n    optparse-applicative >= 0.13\r\n\r\ntest-suite sanity\r\n  type:                 exitcode-stdio-1.0\r\n  hs-source-dirs:       tests\r\n  main-is:              Sanity.hs\r\n  default-language:     Haskell2010\r\n  ghc-options:          -Wall -rtsopts\r\n  if flag(fast)\r\n    ghc-options:        -O0\r\n  else\r\n    ghc-options:        -O2\r\n\r\n  build-depends:\r\n    HUnit,\r\n    base,\r\n    bytestring,\r\n    criterion,\r\n    deepseq,\r\n    tasty,\r\n    tasty-hunit\r\n\r\ntest-suite tests\r\n  type:                 exitcode-stdio-1.0\r\n  hs-source-dirs:       tests\r\n  main-is:              Tests.hs\r\n  default-language:     Haskell2010\r\n  other-modules:        Properties\r\n\r\n  ghc-options:\r\n    -Wall -threaded     -O0 -rtsopts\r\n\r\n  build-depends:\r\n    QuickCheck >= 2.4,\r\n    base,\r\n    base-compat-batteries,\r\n    criterion,\r\n    statistics,\r\n    HUnit,\r\n    tasty,\r\n    tasty-hunit,\r\n    tasty-quickcheck,\r\n    vector,\r\n    aeson\r\n\r\ntest-suite cleanup\r\n  type:                 exitcode-stdio-1.0\r\n  hs-source-dirs:       tests\r\n  default-language:     Haskell2010\r\n  main-is:              Cleanup.hs\r\n\r\n  ghc-options:\r\n    -Wall -threaded     -O0 -rtsopts\r\n\r\n  build-depends:\r\n    HUnit,\r\n    base,\r\n    base-compat,\r\n    bytestring,\r\n    criterion,\r\n    deepseq,\r\n    directory,\r\n    tasty,\r\n    tasty-hunit\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/criterion.git\r\n";
  }