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
      specVersion = "2.2";
      identifier = { name = "optics-th"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Andrzej Rybczak";
      homepage = "";
      url = "";
      synopsis = "Optics construction using TemplateHaskell";
      description = "This package is part of the @<https://hackage.haskell.org/package/optics optics>@\npackage family.  It provides machinery to construct optics using @TemplateHaskell@.\n\nSee the @template-haskell-optics@ package for optics to work with @template-haskell@ types.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      tests = {
        "optics-th-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
            (hsPkgs."optics-th" or (errorHandler.buildDepError "optics-th"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optics-th-0.4.1.tar.gz";
      sha256 = "d73857b79dcd8f7c7e70fa4727f134145b62902e8d3e448f8b25c38a9da4fd17";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\r\nname:          optics-th\r\nversion:       0.4.1\r\nx-revision: 9\r\nlicense:       BSD-3-Clause\r\nlicense-file:  LICENSE\r\nbuild-type:    Simple\r\nmaintainer:    optics@well-typed.com\r\nauthor:        Andrzej Rybczak\r\ntested-with:   GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7\r\n                || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.5 || ==9.8.2\r\n                || ==9.10.1,\r\n               GHCJS ==8.4\r\nsynopsis:      Optics construction using TemplateHaskell\r\ncategory:      Data, Optics, Lenses\r\ndescription:\r\n  This package is part of the @<https://hackage.haskell.org/package/optics optics>@\r\n  package family.  It provides machinery to construct optics using @TemplateHaskell@.\r\n  .\r\n  See the @template-haskell-optics@ package for optics to work with @template-haskell@ types.\r\n\r\nextra-doc-files:\r\n  CHANGELOG.md\r\n\r\nbug-reports:   https://github.com/well-typed/optics/issues\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/well-typed/optics.git\r\n  subdir:   optics-th\r\n\r\ncommon language\r\n    ghc-options:        -Wall -Wcompat\r\n\r\n    default-language:   Haskell2010\r\n\r\n    default-extensions: BangPatterns\r\n                        ConstraintKinds\r\n                        DefaultSignatures\r\n                        DeriveFoldable\r\n                        DeriveFunctor\r\n                        DeriveGeneric\r\n                        DeriveTraversable\r\n                        EmptyCase\r\n                        FlexibleContexts\r\n                        FlexibleInstances\r\n                        FunctionalDependencies\r\n                        GADTs\r\n                        GeneralizedNewtypeDeriving\r\n                        InstanceSigs\r\n                        KindSignatures\r\n                        LambdaCase\r\n                        OverloadedLabels\r\n                        PatternSynonyms\r\n                        RankNTypes\r\n                        ScopedTypeVariables\r\n                        TupleSections\r\n                        TypeApplications\r\n                        TypeFamilies\r\n                        TypeOperators\r\n                        ViewPatterns\r\n\r\nlibrary\r\n  import:           language\r\n  hs-source-dirs:   src\r\n\r\n  build-depends: base                   >= 4.10      && <5\r\n               , containers             >= 0.5.10.2  && <0.8\r\n               , mtl                    >= 2.2.2     && <2.4\r\n               , optics-core            >= 0.4.1     && <0.5\r\n               , template-haskell       >= 2.12      && <2.24\r\n               , th-abstraction         >= 0.4       && <0.8\r\n               , transformers           >= 0.5       && <0.7\r\n\r\n  exposed-modules: Optics.TH\r\n\r\n                   -- internal modules\r\n                   Optics.TH.Internal.Utils\r\n                   Optics.TH.Internal.Product\r\n                   Optics.TH.Internal.Sum\r\n\r\n  other-modules:   Language.Haskell.TH.Optics.Internal\r\n\r\ntest-suite optics-th-tests\r\n  import:           language\r\n  hs-source-dirs:   tests\r\n\r\n  build-depends: base\r\n               , optics-core\r\n               , optics-th\r\n               , tagged\r\n\r\n  type:    exitcode-stdio-1.0\r\n  main-is: Optics/TH/Tests.hs\r\n\r\n  other-modules: Optics.TH.Tests.DuplicateRecordFields\r\n                 Optics.TH.Tests.T799\r\n";
  }