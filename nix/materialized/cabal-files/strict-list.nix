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
      specVersion = "3.0";
      identifier = { name = "strict-list"; version = "0.1.7.6"; };
      license = "MIT";
      copyright = "(c) 2019, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/strict-list";
      url = "";
      synopsis = "Strict linked list";
      description = "Implementation of strict linked list with care taken about stack.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."strict-list" or (errorHandler.buildDepError "strict-list"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/strict-list-0.1.7.6.tar.gz";
      sha256 = "756b16c12b5528cdb44ba905bdc78e5f4a749f33f630226645311a8d52207987";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: strict-list\nversion: 0.1.7.6\nsynopsis: Strict linked list\ndescription:\n  Implementation of strict linked list with care taken about stack.\n\ncategory: Data\nhomepage: https://github.com/nikita-volkov/strict-list\nbug-reports: https://github.com/nikita-volkov/strict-list/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2019, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/strict-list.git\n\ncommon language-settings\n  default-language: Haskell2010\n  default-extensions:\n    BangPatterns\n    DeriveDataTypeable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveTraversable\n    FlexibleContexts\n    FlexibleInstances\n    LambdaCase\n    NoImplicitPrelude\n    RankNTypes\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeFamilies\n\nlibrary\n  import: language-settings\n  hs-source-dirs: library\n  exposed-modules: StrictList\n  other-modules: StrictList.Prelude\n  build-depends:\n    base >=4.9 && <5,\n    deepseq >=1.4.3 && <2,\n    hashable >=1.2 && <2,\n    semigroupoids >=5.3 && <7,\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-extensions:\n    BangPatterns\n    DeriveDataTypeable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveTraversable\n    FlexibleContexts\n    FlexibleInstances\n    LambdaCase\n    NoImplicitPrelude\n    RankNTypes\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeFamilies\n\n  default-language: Haskell2010\n  main-is: Main.hs\n  build-depends:\n    rerebase <2,\n    strict-list,\n    tasty >=0.12 && <2,\n    tasty-quickcheck >=0.9 && <0.12,\n";
  }