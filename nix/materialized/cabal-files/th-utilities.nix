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
      specVersion = "1.12";
      identifier = { name = "th-utilities"; version = "0.2.5.2"; };
      license = "MIT";
      copyright = "2016 FP Complete";
      maintainer = "Michael Sloan <mgsloan@gmail.com>";
      author = "";
      homepage = "https://github.com/fpco/th-utilities#readme";
      url = "";
      synopsis = "Collection of useful functions for use with Template Haskell";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
            (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
            (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
            (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-utilities-0.2.5.2.tar.gz";
      sha256 = "96627114f5ff49fd60ba3163fc400f86fd5d25d0356fe30c5d5f16309de58ed8";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.37.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:           th-utilities\nversion:        0.2.5.2\nsynopsis:       Collection of useful functions for use with Template Haskell\ncategory:       Template Haskell\nhomepage:       https://github.com/fpco/th-utilities#readme\nbug-reports:    https://github.com/fpco/th-utilities/issues\nmaintainer:     Michael Sloan <mgsloan@gmail.com>\ncopyright:      2016 FP Complete\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/fpco/th-utilities\n\nlibrary\n  exposed-modules:\n      TH.Derive\n      TH.Derive.Storable\n      TH.FixQ\n      TH.ReifySimple\n      TH.RelativePaths\n      TH.Utilities\n  other-modules:\n      TH.Derive.Internal\n  hs-source-dirs:\n      src\n  ghc-options: -Wall -fwarn-tabs -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates\n  build-depends:\n      base >=4.7 && <5\n    , bytestring\n    , containers\n    , directory\n    , filepath\n    , primitive\n    , syb\n    , template-haskell >=2.8\n    , text\n    , th-abstraction >=0.4\n    , th-orphans\n  default-language: Haskell2010\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n      TH.Derive.StorableSpec\n      TH.DeriveSpec\n      TH.DeriveSpec.TH\n      TH.UtilitiesSpec\n      Paths_th_utilities\n  hs-source-dirs:\n      test\n  ghc-options: -Wall -fwarn-tabs -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates\n  build-depends:\n      base >=4.7 && <5\n    , bytestring\n    , containers\n    , directory\n    , filepath\n    , hspec\n    , primitive\n    , syb\n    , template-haskell >=2.8\n    , text\n    , th-abstraction >=0.4\n    , th-lift\n    , th-orphans\n    , th-utilities\n    , vector\n  default-language: Haskell2010\n";
  }