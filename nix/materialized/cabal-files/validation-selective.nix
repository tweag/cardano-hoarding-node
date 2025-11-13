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
    package-description-override = "cabal-version:   2.4\nname:            validation-selective\nversion:         0.2.0.0.0.0.0.0.1\nlicense:         MPL-2.0\nlicense-file:    LICENSE\ncopyright:       2020-2023 Kowainik\nmaintainer:      Kowainik <xrom.xkov@gmail.com>\nauthor:          Dmitrii Kovanikov, Veronika Romashkina\ntested-with:\n    ghc ==8.6.5 ghc ==8.8.4 ghc ==8.10.7 ghc ==9.0.2 ghc ==9.2.7\n    ghc ==9.4.4 ghc ==9.6.1\n\nhomepage:        https://github.com/kowainik/validation-selective\nbug-reports:     https://github.com/kowainik/validation-selective/issues\nsynopsis:\n    Lighweight pure data validation based on Applicative and Selective functors\n\ndescription:\n    Lighweight pure data validation based on Applicative and Selective\n    functors. The library builds validation interface around the\n    following data type:\n    .\n    @\n    __data__ Validation e a\n    \\    = Failure e\n    \\    | Success a\n    @\n    .\n\ncategory:        Validation, Selective, Data\nbuild-type:      Simple\nextra-doc-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/kowainik/validation-selective.git\n\nlibrary\n    exposed-modules:\n        Validation\n        Validation.Combinators\n\n    hs-source-dirs:     src\n    default-language:   Haskell2010\n    default-extensions:\n        ConstraintKinds DeriveGeneric DerivingStrategies\n        GeneralizedNewtypeDeriving InstanceSigs KindSignatures LambdaCase\n        OverloadedStrings RecordWildCards ScopedTypeVariables\n        StandaloneDeriving TupleSections TypeApplications ViewPatterns\n\n    ghc-options:\n        -Wall -Wcompat -Widentities -Wincomplete-uni-patterns\n        -Wincomplete-record-updates -Wredundant-constraints\n        -fhide-source-paths -Wmissing-export-lists -Wpartial-fields\n\n    build-depends:\n        base >=4.12 && <4.20,\n        deepseq ^>=1.4.3.0,\n        selective >=0.3 && <0.7\n\n    if impl(ghc >=8.8)\n        ghc-options: -Wmissing-deriving-strategies\n\n    if impl(ghc >=8.10)\n        ghc-options: -Wunused-packages\n\n    if impl(ghc >=9.0)\n        ghc-options: -Winvalid-haddock\n\n    if impl(ghc >=9.2)\n        ghc-options: -Wredundant-bang-patterns -Woperator-whitespace\n\ntest-suite validation-selective-test\n    type:               exitcode-stdio-1.0\n    main-is:            Spec.hs\n    hs-source-dirs:     test\n    other-modules:\n        Test.Gen\n        Test.Laws\n        Test.Properties\n\n    default-language:   Haskell2010\n    default-extensions:\n        ConstraintKinds DeriveGeneric DerivingStrategies\n        GeneralizedNewtypeDeriving InstanceSigs KindSignatures LambdaCase\n        OverloadedStrings RecordWildCards ScopedTypeVariables\n        StandaloneDeriving TupleSections TypeApplications ViewPatterns\n\n    ghc-options:\n        -Wall -Wcompat -Widentities -Wincomplete-uni-patterns\n        -Wincomplete-record-updates -Wredundant-constraints\n        -fhide-source-paths -Wmissing-export-lists -Wpartial-fields\n        -threaded -rtsopts -with-rtsopts=-N\n\n    build-depends:\n        base >=4.12 && <4.20,\n        validation-selective,\n        hedgehog >=1.0 && <1.3,\n        hspec >=2.7.1 && <2.11,\n        hspec-hedgehog ^>=0.0.1.1,\n        selective,\n        text >=1.2.3 && <2.1\n\n    if impl(ghc >=8.8)\n        ghc-options: -Wmissing-deriving-strategies\n\n    if impl(ghc >=8.10)\n        ghc-options: -Wunused-packages\n\n    if impl(ghc >=9.0)\n        ghc-options: -Winvalid-haddock\n\n    if impl(ghc >=9.2)\n        ghc-options: -Wredundant-bang-patterns -Woperator-whitespace\n\ntest-suite validation-selective-doctest\n    type:               exitcode-stdio-1.0\n    main-is:            Doctest.hs\n    hs-source-dirs:     test\n    default-language:   Haskell2010\n    default-extensions:\n        ConstraintKinds DeriveGeneric DerivingStrategies\n        GeneralizedNewtypeDeriving InstanceSigs KindSignatures LambdaCase\n        OverloadedStrings RecordWildCards ScopedTypeVariables\n        StandaloneDeriving TupleSections TypeApplications ViewPatterns\n\n    ghc-options:\n        -Wall -Wcompat -Widentities -Wincomplete-uni-patterns\n        -Wincomplete-record-updates -Wredundant-constraints\n        -fhide-source-paths -Wmissing-export-lists -Wpartial-fields\n        -threaded\n\n    build-depends:\n        base >=4.12 && <4.20,\n        doctest >=0.16 && <0.22\n\n    if impl(ghc >=8.8)\n        ghc-options: -Wmissing-deriving-strategies\n\n    if impl(ghc >=8.10)\n        ghc-options: -Wunused-packages\n\n    if impl(ghc >=9.0)\n        ghc-options: -Winvalid-haddock\n\n    if impl(ghc >=9.2)\n        ghc-options: -Wredundant-bang-patterns -Woperator-whitespace\n";
  }