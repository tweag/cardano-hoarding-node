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
      identifier = { name = "deque"; version = "0.4.4.2"; };
      license = "MIT";
      copyright = "(c) 2016, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/deque";
      url = "";
      synopsis = "Double-ended queues";
      description = "Strict and lazy implementations of Double-Ended Queue (aka Dequeue or Deque)\nbased on head-tail linked list.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."strict-list" or (errorHandler.buildDepError "strict-list"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/deque-0.4.4.2.tar.gz";
      sha256 = "a26dd89e6b1cb2cbf061717bd37a8c27d9bc2660e632a8c786b5c0f48740af14";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname:          deque\nversion:       0.4.4.2\nsynopsis:      Double-ended queues\ndescription:\n  Strict and lazy implementations of Double-Ended Queue (aka Dequeue or Deque)\n  based on head-tail linked list.\n\ncategory:      Data\nhomepage:      https://github.com/nikita-volkov/deque\nbug-reports:   https://github.com/nikita-volkov/deque/issues\nauthor:        Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:    Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:     (c) 2016, Nikita Volkov\nlicense:       MIT\nlicense-file:  LICENSE\n\nsource-repository head\n  type:     git\n  location: https://github.com/nikita-volkov/deque\n\nlibrary\n  hs-source-dirs:     library\n  default-extensions:\n    NoImplicitPrelude\n    BangPatterns\n    DeriveDataTypeable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveTraversable\n    FlexibleContexts\n    FlexibleInstances\n    LambdaCase\n    RankNTypes\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeFamilies\n\n  ghc-options:        -funbox-strict-fields\n  default-language:   Haskell2010\n  exposed-modules:\n    Deque.Lazy\n    Deque.Lazy.Reader\n    Deque.Lazy.State\n    Deque.Strict\n    Deque.Strict.Reader\n    Deque.Strict.State\n\n  other-modules:\n    Deque.Lazy.Defs\n    Deque.Prelude\n    Deque.Strict.Defs\n\n  build-depends:\n    , base >=4.9 && <5\n    , deepseq >=1.4.3 && <2\n    , hashable >=1.2 && <2\n    , mtl >=2.2 && <3\n    , strict-list >=0.1.6 && <0.2\n\ntest-suite test\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test\n  default-extensions:\n    NoImplicitPrelude\n    BangPatterns\n    DeriveDataTypeable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveTraversable\n    FlexibleContexts\n    FlexibleInstances\n    LambdaCase\n    RankNTypes\n    ScopedTypeVariables\n    StandaloneDeriving\n    TypeApplications\n    TypeFamilies\n\n  default-language:   Haskell2010\n  main-is:            Main.hs\n  build-depends:\n    , deque\n    , rerebase <2\n    , tasty >=0.12 && <2\n    , tasty-quickcheck >=0.9 && <0.12\n";
  }