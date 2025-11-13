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
      identifier = { name = "multiset"; version = "0.3.4.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "twanvl@gmail.com";
      author = "Twan van Laarhoven";
      homepage = "";
      url = "";
      synopsis = "The Data.MultiSet container type";
      description = "A variation of Data.Set.\nMultisets, sometimes also called bags, can contain multiple copies of the same key.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
        ];
        buildable = true;
      };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
          ];
          buildable = if compiler.isGhc && compiler.version.lt "8.0"
            then false
            else true;
        };
        "multiset-properties" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/multiset-0.3.4.3.tar.gz";
      sha256 = "79fcae15a5d3ce28f0b973ad90290f7451396e81cc92007456ce2bb49b9415c4";
    });
  }) // {
    package-description-override = "name:             multiset\r\nversion:          0.3.4.3\r\nx-revision: 1\r\nauthor:           Twan van Laarhoven\r\nmaintainer:       twanvl@gmail.com\r\nbug-reports:      https://github.com/twanvl/multiset/issues\r\ncategory:         Data Structures\r\nsynopsis:         The Data.MultiSet container type\r\ndescription:\r\n    A variation of Data.Set.\r\n    Multisets, sometimes also called bags, can contain multiple copies of the same key.\r\nlicense:          BSD3\r\nlicense-file:     LICENSE\r\nbuild-type:       Simple\r\nCabal-version:    >= 1.10\r\nextra-source-files: include/Typeable.h CHANGELOG\r\ntested-with: GHC == 8.6.4, GHC == 8.4.4, GHC == 8.2.2, GHC == 8.0.2,\r\n             GHC == 7.10.3, GHC == 7.8.4, GHC == 7.6.3, GHC == 7.4.2,\r\n             GHC == 7.2.2, GHC == 7.0.4\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: http://github.com/twanvl/multiset.git\r\n\r\nLibrary\r\n  default-language:   Haskell2010\r\n  exposed-modules:    Data.MultiSet, Data.IntMultiSet\r\n\r\n  include-dirs:       include\r\n  default-extensions: CPP\r\n  ghc-options:        -Wall\r\n  build-depends:      containers >= 0.5.4, base >= 4 && < 5, deepseq >=1.2 && <1.6\r\n\r\ntest-suite doctests\r\n  default-language:   Haskell2010\r\n  type:               exitcode-stdio-1.0\r\n  ghc-options:        -threaded\r\n  hs-source-dirs:     test\r\n  main-is:            Main.hs\r\n  build-depends:      base >= 4 && < 5\r\n                    , doctest\r\n  if impl(ghc < 8.0)\r\n    buildable: False\r\n\r\ntest-suite multiset-properties\r\n  default-language:   Haskell2010\r\n  type:               exitcode-stdio-1.0\r\n  ghc-options:        -threaded\r\n  hs-source-dirs:     test\r\n  main-is:            multiset-properties.hs\r\n  build-depends:      QuickCheck\r\n                    , base\r\n                    , checkers >= 0.5\r\n                    , multiset\r\n                    , tasty\r\n                    , tasty-quickcheck\r\n";
  }