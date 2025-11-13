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
      specVersion = "2.0";
      identifier = { name = "Unique"; version = "0.4.8.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ualinuxcn@gmail.com";
      author = "Volodymyr Yashchenko";
      homepage = "";
      url = "";
      synopsis = "It provides the functionality like unix \"uniq\" utility";
      description = "Library provides the functions to find unique and duplicate elements in the list";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
        ];
        buildable = true;
      };
      tests = {
        "HspecTest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "Criterion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Unique-0.4.8.0.tar.gz";
      sha256 = "a664381cc6f099837c40d3ced302e28be1a58309b50f78922a350abda76e69f3";
    });
  }) // {
    package-description-override = "cabal-version: 2.0\r\nname:          Unique\r\nversion:       0.4.8.0\r\nx-revision: 1\r\nlicense:       BSD3\r\nlicense-file:  LICENSE\r\nmaintainer:    ualinuxcn@gmail.com\r\nauthor:        Volodymyr Yashchenko\r\ntested-with:   ghc >=7.4 && <8.2.1 || >8.2.1 && <8.12\r\nsynopsis:      It provides the functionality like unix \"uniq\" utility\r\ndescription:\r\n    Library provides the functions to find unique and duplicate elements in the list\r\n\r\ncategory:      Data\r\nbuild-type:    Simple\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/kapralVV/Unique.git\r\n\r\nlibrary\r\n    exposed-modules:\r\n        Data.List.Unique\r\n        Data.List.UniqueStrict\r\n        Data.List.UniqueUnsorted\r\n\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n    build-depends:\r\n        base >=4.0 && < 5,\r\n        containers >=0.5.0.0 && <=0.9,\r\n        extra >=1.6.2 && <=1.8,\r\n        hashable >= 1.2.6 && < 1.6,\r\n        unordered-containers >= 0.2.8 && <=0.3\r\n\r\ntest-suite HspecTest\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   tests\r\n    other-modules:\r\n        Unique.Complex\r\n        Unique.IsUnique\r\n        Unique.RepeatedBy\r\n        Unique.SortUniq\r\n        Unique.AllUnique\r\n        UniqueStrict.IsUnique\r\n        UniqueStrict.RepeatedBy\r\n        UniqueStrict.SortUniq\r\n        UniqueStrict.AllUnique\r\n        UniqueUnsorted.IsUnique\r\n        UniqueUnsorted.RemoveDuplicates\r\n        UniqueUnsorted.RepeatedBy\r\n        UniqueUnsorted.AllUnique\r\n\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n    build-depends:\r\n        base >=4.0 && <5,\r\n        Unique,\r\n        hspec -any,\r\n        containers >=0.5.0.0 && <=0.9,\r\n        QuickCheck    >= 2.10 && <2.17\r\n\r\nbenchmark Criterion\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall -rtsopts\r\n    build-depends:\r\n        base >=4.0 && <5,\r\n        Unique,\r\n        criterion -any,\r\n        QuickCheck    >= 2.10 && <2.17,\r\n        quickcheck-instances -any,\r\n        bytestring -any,\r\n        hashable -any\r\n";
  }