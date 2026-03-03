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
      identifier = { name = "ki-unlifted"; version = "1.0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2020-2025 Mitchell Rosen, Travis Staton";
      maintainer = "Mitchell Rosen <mitchellwrosen@gmail.com>, Travis Staton <hello@travisstaton.com>";
      author = "Mitchell Rosen";
      homepage = "https://github.com/awkward-squad/ki";
      url = "";
      synopsis = "A lightweight structured concurrency library";
      description = "A lightweight structured concurrency library.\n\nFor a specialised variant of this API that does not use\n@<https://hackage.haskell.org/package/unliftio-core unliftio-core>@, see\n@<https://hackage.haskell.org/package/ki ki>@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ki" or (errorHandler.buildDepError "ki"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ki-unlifted-1.0.0.2.tar.gz";
      sha256 = "bb87ed8f82999adeda0015ee26822a2ffcd5886d78d82b1cffcafe6042956485";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\n\nauthor: Mitchell Rosen\nbug-reports: https://github.com/awkward-squad/ki/issues\ncategory: Concurrency\ncopyright: Copyright (C) 2020-2025 Mitchell Rosen, Travis Staton\nhomepage: https://github.com/awkward-squad/ki\nlicense: BSD-3-Clause\nlicense-file: LICENSE\nmaintainer: Mitchell Rosen <mitchellwrosen@gmail.com>, Travis Staton <hello@travisstaton.com>\nname: ki-unlifted\nstability: stable\nsynopsis: A lightweight structured concurrency library\ntested-with: GHC == 9.8.4, GHC == 9.10.1, GHC == 9.12.1\nversion: 1.0.0.2\nx-revision: 3\n\ndescription:\n  A lightweight structured concurrency library.\n  .\n  For a specialised variant of this API that does not use\n  @<https://hackage.haskell.org/package/unliftio-core unliftio-core>@, see\n  @<https://hackage.haskell.org/package/ki ki>@.\n\nextra-source-files:\n  CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/awkward-squad/ki.git\n  subdir: ki-unlifted\n\ncommon component\n  build-depends:\n    base ^>= 4.12 || ^>= 4.13 || ^>= 4.14 || ^>= 4.15 || ^>= 4.16 || ^>= 4.17 || ^>= 4.18 || ^>= 4.19 || ^>= 4.20 || ^>= 4.21,\n  default-extensions:\n    AllowAmbiguousTypes\n    BangPatterns\n    BlockArguments\n    ConstraintKinds\n    DeriveAnyClass\n    DeriveDataTypeable\n    DeriveFunctor\n    DeriveGeneric\n    DerivingStrategies\n    DuplicateRecordFields\n    ExistentialQuantification\n    GeneralizedNewtypeDeriving\n    InstanceSigs\n    LambdaCase\n    NamedFieldPuns\n    NoImplicitPrelude\n    NumericUnderscores\n    PartialTypeSignatures\n    PatternSynonyms\n    RankNTypes\n    RoleAnnotations\n    ScopedTypeVariables\n    TypeApplications\n    ViewPatterns\n  default-language: Haskell2010\n  ghc-options:\n    -Weverything\n    -Wno-all-missed-specialisations\n    -Wno-implicit-prelude\n    -Wno-missed-specialisations\n    -Wno-missing-import-lists\n    -Wno-safe\n    -Wno-unsafe\n  if impl(ghc >= 8.10)\n    ghc-options:\n      -Wno-missing-safe-haskell-mode\n      -Wno-prepositive-qualified-module\n  if impl(ghc >= 9.2)\n    ghc-options:\n      -Wno-missing-kind-signatures\n  if impl(ghc >= 9.8)\n    ghc-options:\n      -Wno-missing-role-annotations\n\nlibrary\n  import: component\n  build-depends:\n    ki ^>= 1.0,\n    unliftio-core ^>= 0.2,\n  exposed-modules:\n    Ki.Unlifted\n  hs-source-dirs: src\n";
  }