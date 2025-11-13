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
      identifier = { name = "mutable-containers"; version = "0.3.4.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@fpcomplete.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/mono-traversable#readme";
      url = "";
      synopsis = "Abstactions and concrete implementations of mutable containers";
      description = "See docs and README at <http://www.stackage.org/package/mutable-containers>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mutable-containers" or (errorHandler.buildDepError "mutable-containers"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "deque" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mutable-containers" or (errorHandler.buildDepError "mutable-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "ref" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mutable-containers" or (errorHandler.buildDepError "mutable-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mutable-containers-0.3.4.1.tar.gz";
      sha256 = "79429ec69c94ad08946bd1de1fe1c261d017b47258c99fe97ca0238d5a6c36cf";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.35.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:           mutable-containers\nversion:        0.3.4.1\nsynopsis:       Abstactions and concrete implementations of mutable containers\ndescription:    See docs and README at <http://www.stackage.org/package/mutable-containers>\ncategory:       Data\nhomepage:       https://github.com/snoyberg/mono-traversable#readme\nbug-reports:    https://github.com/snoyberg/mono-traversable/issues\nauthor:         Michael Snoyman\nmaintainer:     michael@fpcomplete.com\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/snoyberg/mono-traversable\n\nlibrary\n  exposed-modules:\n      Data.Mutable\n  other-modules:\n      Data.Mutable.BRef\n      Data.Mutable.Class\n      Data.Mutable.Deque\n      Data.Mutable.DLList\n      Data.Mutable.PRef\n      Data.Mutable.SRef\n      Data.Mutable.URef\n      Paths_mutable_containers\n  hs-source-dirs:\n      src\n  ghc-options: -O2\n  build-depends:\n      base >=4.13 && <5\n    , containers\n    , ghc-prim\n    , mono-traversable\n    , primitive >=0.5.2.1\n    , vector\n  default-language: Haskell2010\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Paths_mutable_containers\n  hs-source-dirs:\n      test\n  build-depends:\n      QuickCheck\n    , base >=4.13 && <5\n    , containers\n    , hspec\n    , mutable-containers\n    , primitive\n    , vector\n  default-language: Haskell2010\n\nbenchmark deque\n  type: exitcode-stdio-1.0\n  main-is: deque.hs\n  other-modules:\n      Paths_mutable_containers\n  hs-source-dirs:\n      bench\n  ghc-options: -Wall -O2 -rtsopts\n  build-depends:\n      base >=4.13 && <5\n    , containers\n    , gauge\n    , mutable-containers\n    , vector\n  default-language: Haskell2010\n\nbenchmark ref\n  type: exitcode-stdio-1.0\n  main-is: ref.hs\n  other-modules:\n      Paths_mutable_containers\n  hs-source-dirs:\n      bench\n  ghc-options: -Wall -O2 -rtsopts\n  build-depends:\n      base >=4.13 && <5\n    , containers\n    , gauge\n    , mutable-containers\n    , vector\n  default-language: Haskell2010\n";
  }