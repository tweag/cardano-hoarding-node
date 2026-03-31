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
      specVersion = "1.18";
      identifier = { name = "selective"; version = "0.7.0.1"; };
      license = "MIT";
      copyright = "Andrey Mokhov, 2018-2024";
      maintainer = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard";
      author = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard";
      homepage = "https://github.com/snowleopard/selective";
      url = "";
      synopsis = "Selective applicative functors";
      description = "Selective applicative functors: declare your effects statically,\nselect which to execute dynamically.\n\nThis is a library for /selective applicative functors/, or just\n/selective functors/ for short, an abstraction between\napplicative functors and monads, introduced in\n<https://dl.acm.org/doi/10.1145/3341694 this paper>.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      tests = {
        "main" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."selective" or (errorHandler.buildDepError "selective"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/selective-0.7.0.1.tar.gz";
      sha256 = "dbfb4be71c7956f54e099c34ffe538dfaa99ed3911208df5828563da22b6aee3";
    });
  }) // {
    package-description-override = "name:          selective\nversion:       0.7.0.1\nsynopsis:      Selective applicative functors\nlicense:       MIT\nlicense-file:  LICENSE\nauthor:        Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard\nmaintainer:    Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard\ncopyright:     Andrey Mokhov, 2018-2024\nhomepage:      https://github.com/snowleopard/selective\nbug-reports:   https://github.com/snowleopard/selective/issues\ncategory:      Control\nbuild-type:    Simple\ncabal-version: 1.18\ntested-with:   GHC==9.8.2, GHC==9.6.3, GHC==9.4.7, GHC==9.2.8, GHC==9.0.2, GHC==8.10.7, GHC==8.8.4, GHC==8.6.5\ndescription:   Selective applicative functors: declare your effects statically,\n               select which to execute dynamically.\n               .\n               This is a library for /selective applicative functors/, or just\n               /selective functors/ for short, an abstraction between\n               applicative functors and monads, introduced in\n               <https://dl.acm.org/doi/10.1145/3341694 this paper>.\n\nextra-doc-files:\n    CHANGES.md\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/snowleopard/selective.git\n\nlibrary\n    hs-source-dirs:     src\n    exposed-modules:    Control.Selective,\n                        Control.Selective.Free,\n                        Control.Selective.Multi,\n                        Control.Selective.Rigid.Free,\n                        Control.Selective.Rigid.Freer,\n                        Control.Selective.Trans.Except\n    build-depends:      base         >= 4.12    && < 5,\n                        transformers >= 0.4.2.0 && < 0.7\n    default-language:   Haskell2010\n    other-extensions:   DeriveFunctor,\n                        FlexibleInstances,\n                        GADTs,\n                        GeneralizedNewtypeDeriving,\n                        RankNTypes,\n                        StandaloneDeriving,\n                        TupleSections,\n                        DerivingVia\n    ghc-options:        -Wall\n                        -fno-warn-name-shadowing\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wredundant-constraints\n    if impl(ghc >= 9.2)\n        ghc-options:    -Wno-operator-whitespace-ext-conflict\n\ntest-suite main\n    hs-source-dirs:     test, examples\n    other-modules:      Build,\n                        Laws,\n                        Parser,\n                        Processor,\n                        Query,\n                        Sketch,\n                        Teletype,\n                        Teletype.Rigid,\n                        Test,\n                        Validation\n    type:               exitcode-stdio-1.0\n    main-is:            Main.hs\n    build-depends:      base                   >= 4.7     && < 5,\n                        containers             >= 0.5.5.1 && < 0.8,\n                        QuickCheck             >= 2.8     && < 2.15,\n                        selective,\n                        transformers           >= 0.4.2.0 && < 0.7\n    default-language:   Haskell2010\n    ghc-options:        -Wall\n                        -fno-warn-name-shadowing\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wredundant-constraints\n    if impl(ghc >= 9.2)\n        ghc-options:    -Wno-operator-whitespace-ext-conflict\n";
  }