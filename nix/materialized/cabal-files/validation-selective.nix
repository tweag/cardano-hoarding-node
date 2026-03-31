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
      specVersion = "2.4";
      identifier = {
        name = "validation-selective";
        version = "0.2.0.0.0.0.0.0.1";
      };
      license = "MPL-2.0";
      copyright = "2020-2023 Kowainik";
      maintainer = "Kowainik <xrom.xkov@gmail.com>";
      author = "Dmitrii Kovanikov, Veronika Romashkina";
      homepage = "https://github.com/kowainik/validation-selective";
      url = "";
      synopsis = "Lighweight pure data validation based on Applicative and Selective functors";
      description = "Lighweight pure data validation based on Applicative and Selective\nfunctors. The library builds validation interface around the\nfollowing data type:\n\n@\n__data__ Validation e a\n\\    = Failure e\n\\    | Success a\n@\n";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."selective" or (errorHandler.buildDepError "selective"))
        ];
        buildable = true;
      };
      tests = {
        "validation-selective-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."selective" or (errorHandler.buildDepError "selective"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
        "validation-selective-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/validation-selective-0.2.0.0.0.0.0.0.1.tar.gz";
      sha256 = "d1f97b6600455009e0a5c72b5e578875d241ab5960f79997be2f45931df94f57";
    });
  }) // {
    package-description-override = "cabal-version:       2.4\nname:                validation-selective\nversion:             0.2.0.0.0.0.0.0.1\nx-revision: 7\nsynopsis:            Lighweight pure data validation based on Applicative and Selective functors\ndescription:\n    Lighweight pure data validation based on Applicative and Selective\n    functors. The library builds validation interface around the\n    following data type:\n    .\n    @\n    __data__ Validation e a\n    \\    = Failure e\n    \\    | Success a\n    @\n    .\n\nhomepage:            https://github.com/kowainik/validation-selective\nbug-reports:         https://github.com/kowainik/validation-selective/issues\nlicense:             MPL-2.0\nlicense-file:        LICENSE\nauthor:              Dmitrii Kovanikov, Veronika Romashkina\nmaintainer:          Kowainik <xrom.xkov@gmail.com>\ncopyright:           2020-2023 Kowainik\ncategory:            Validation, Selective, Data\nbuild-type:          Simple\nextra-doc-files:     README.md\n                     CHANGELOG.md\ntested-with:\n  GHC == 9.6.2\n  GHC == 9.4.4\n  GHC == 9.2.7\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n\nsource-repository head\n  type:                git\n  location:            https://github.com/kowainik/validation-selective.git\n\ncommon common-options\n  build-depends:       base >= 4.12 && < 4.23\n\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Widentities\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wredundant-constraints\n                       -fhide-source-paths\n                       -Wmissing-export-lists\n                       -Wpartial-fields\n  if impl(ghc >= 8.8)\n    ghc-options:       -Wmissing-deriving-strategies\n  if impl(ghc >= 8.10)\n    ghc-options:       -Wunused-packages\n  if impl(ghc >= 9.0)\n    ghc-options:       -Winvalid-haddock\n  if impl(ghc >= 9.2)\n    ghc-options:       -Wredundant-bang-patterns\n                       -Woperator-whitespace\n\n  default-language:    Haskell2010\n  default-extensions:  ConstraintKinds\n                       DeriveGeneric\n                       DerivingStrategies\n                       GeneralizedNewtypeDeriving\n                       InstanceSigs\n                       KindSignatures\n                       LambdaCase\n                       OverloadedStrings\n                       RecordWildCards\n                       ScopedTypeVariables\n                       StandaloneDeriving\n                       TupleSections\n                       TypeApplications\n                       ViewPatterns\n\nlibrary\n  import:              common-options\n  hs-source-dirs:      src\n  exposed-modules:     Validation\n                         Validation.Combinators\n  build-depends:       deepseq   >= 1.4.3.0   && < 1.6\n                     , selective >= 0.3       && < 0.8\n\ntest-suite validation-selective-test\n  import:              common-options\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules:       Test.Gen\n                       Test.Laws\n                       Test.Properties\n  build-depends:       validation-selective\n                     , hedgehog         >= 1.0      && < 1.6\n                     , hspec            >= 2.7.1    && < 3\n                     , hspec-hedgehog   >= 0.0.1.1  && < 0.2\n                     , selective\n                     , text             >= 1.2.3    && < 2.2\n  ghc-options:         -threaded\n                       -rtsopts\n                       -with-rtsopts=-N\n\ntest-suite validation-selective-doctest\n  import:              common-options\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Doctest.hs\n  build-depends:       doctest >= 0.16 && < 1\n  ghc-options:         -threaded\n";
  }