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
      specVersion = "1.10";
      identifier = { name = "terminal-progress-bar"; version = "0.4.2"; };
      license = "BSD-3-Clause";
      copyright = "2012–2019 Roel van Dijk <roel@lambdacube.nl>";
      maintainer = "Roel van Dijk <roel@lambdacube.nl>";
      author = "Roel van Dijk <roel@lambdacube.nl>";
      homepage = "https://github.com/roelvandijk/terminal-progress-bar";
      url = "";
      synopsis = "A progress bar in the terminal";
      description = "A progress bar conveys the progress of a task. This package\nimplements a progress bar that is displayed in a terminal.\n\nSee the module 'System.ProgressBar' to get started or look at the\nterminal-progress-bar-example package.\n\nThe animated progress bar depends entirely on the interpretation of\nthe carriage return character (\\'\\\\r\\'). If your terminal interprets\nit as something else than \\\"move cursor to beginning of line\\\", the\nanimation won't work.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."terminal-size" or (errorHandler.buildDepError "terminal-size"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
      };
      tests = {
        "test-terminal-progress-bar" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."terminal-progress-bar" or (errorHandler.buildDepError "terminal-progress-bar"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench-terminal-progress-bar" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."terminal-progress-bar" or (errorHandler.buildDepError "terminal-progress-bar"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/terminal-progress-bar-0.4.2.tar.gz";
      sha256 = "fec9da4998c97b3e39b82b80427da7cd72bcc768b6b5535eae07d745b9e02052";
    });
  }) // {
    package-description-override = "name:          terminal-progress-bar\nversion:       0.4.2\ncabal-version: >=1.10\nbuild-type:    Simple\nauthor:        Roel van Dijk <roel@lambdacube.nl>\nmaintainer:    Roel van Dijk <roel@lambdacube.nl>\ncopyright:     2012–2019 Roel van Dijk <roel@lambdacube.nl>\nlicense:       BSD3\nlicense-file:  LICENSE\ncategory:      System, User Interfaces\nhomepage:      https://github.com/roelvandijk/terminal-progress-bar\nbug-reports:   https://github.com/roelvandijk/terminal-progress-bar/issues\nsynopsis:      A progress bar in the terminal\ndescription:\n  A progress bar conveys the progress of a task. This package\n  implements a progress bar that is displayed in a terminal.\n  .\n  See the module 'System.ProgressBar' to get started or look at the\n  terminal-progress-bar-example package.\n  .\n  The animated progress bar depends entirely on the interpretation of\n  the carriage return character (\\'\\\\r\\'). If your terminal interprets\n  it as something else than \\\"move cursor to beginning of line\\\", the\n  animation won't work.\n\nextra-source-files: LICENSE, README.markdown, changelog.md\n\nsource-repository head\n  type:     git\n  location: git://github.com/roelvandijk/terminal-progress-bar.git\n\nlibrary\n    hs-source-dirs: src\n    build-depends:\n        base          >= 4.5 && < 5\n      , deepseq       >= 1.4.3\n      , terminal-size >= 0.3.2\n      , text          >= 1.2\n      , time          >= 1.8\n    exposed-modules: System.ProgressBar\n    ghc-options: -Wall\n    default-language: Haskell2010\n\ntest-suite test-terminal-progress-bar\n    type: exitcode-stdio-1.0\n    main-is: test.hs\n    hs-source-dirs: test\n    ghc-options: -Wall\n    build-depends:\n        base                 >= 4.5 && < 5\n      , HUnit                >= 1.2.4.2\n      , terminal-progress-bar\n      , test-framework       >= 0.3.3\n      , test-framework-hunit >= 0.2.6\n      , text                 >= 1.2\n      , time                 >= 1.8\n    default-language: Haskell2010\n\nbenchmark bench-terminal-progress-bar\n    type: exitcode-stdio-1.0\n    main-is: bench.hs\n    hs-source-dirs: bench\n\n    build-depends:\n        base      >= 4.5 && < 5\n      , criterion >= 1.1.4\n      , terminal-progress-bar\n      , time      >= 1.8\n    ghc-options: -Wall -O2\n    default-language: Haskell2010\n";
  }