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
    flags = { nightly = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "ouroboros-network-testing"; version = "0.8.2.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knuttson";
      homepage = "";
      url = "";
      synopsis = "Common modules used for testing in ouroboros-network and ouroboros-consensus";
      description = "Common modules used for testing in ouroboros-network and ouroboros-consensus.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deque" or (errorHandler.buildDepError "deque"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-testing-0.8.2.0.tar.gz";
      sha256 = "b9182e64eb7b7057796be6d4ca24d444083e3ea8dd871a351252809c7fb94805";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-network-testing\nversion: 0.8.2.0\nsynopsis: Common modules used for testing in ouroboros-network and ouroboros-consensus\ndescription: Common modules used for testing in ouroboros-network and ouroboros-consensus.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect\nauthor: Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knuttson\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/ouroboros-network\n\nflag nightly\n  description: Enable nightly tests\n  manual: False\n  default: False\n\nlibrary\n  hs-source-dirs: src\n  -- At this experiment/prototype stage everything is exposed.\n  -- This has to be tidied up once the design becomes clear.\n  exposed-modules:\n    Test.Ouroboros.Network.Data.AbsBearerInfo\n    Test.Ouroboros.Network.Data.Script\n    Test.Ouroboros.Network.Data.Signal\n    Test.Ouroboros.Network.QuickCheck\n    Test.Ouroboros.Network.Serialise\n    Test.Ouroboros.Network.Utils\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  other-extensions:\n    BangPatterns\n    DataKinds\n    EmptyCase\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTSyntax\n    GADTs\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    NamedFieldPuns\n    OverloadedStrings\n    PolyKinds\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    TemplateHaskell\n    TupleSections\n    TypeApplications\n    TypeFamilies\n    TypeInType\n\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    cborg >=0.2.1 && <0.3,\n    containers,\n    contra-tracer,\n    deque ^>=0.4,\n    io-classes:{io-classes, si-timers, strict-stm} ^>=1.8.0.1,\n    io-sim,\n    network-mux,\n    pretty-simple,\n    psqueues >=0.2.3 && <0.3,\n    serialise >=0.2 && <0.3,\n    tasty,\n    tasty-expected-failure,\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -fno-ignore-asserts\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n\n  if flag(nightly)\n    cpp-options: -DNIGHTLY\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules: Test.Ouroboros.Network.Data.AbsBearerInfo.Test\n  build-depends:\n    QuickCheck,\n    base >=4.14 && <4.22,\n    ouroboros-network-testing,\n    tasty,\n    tasty-quickcheck,\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:\n    -rtsopts\n    -threaded\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wno-unticked-promoted-constructors\n    -Wunused-packages\n";
  }