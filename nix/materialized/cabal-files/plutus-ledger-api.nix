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
      identifier = { name = "plutus-ledger-api"; version = "1.53.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Interface to the Plutus ledger for the Cardano ledger.";
      description = "Interface to the Plutus scripting support for the Cardano ledger.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "plutus-ledger-api-testlib" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      exes = {
        "test-onchain-evaluation" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          buildable = true;
        };
        "analyse-script-events" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          ];
          buildable = true;
        };
        "dump-cost-model-parameters" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
      tests = {
        "plutus-ledger-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
        "plutus-ledger-api-plugin-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = (if compiler.isGhc && compiler.version.lt "9.6" || compiler.isGhc && compiler.version.ge "9.7"
            then false
            else true) && (if system.isWindows then false else true);
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/plutus-ledger-api-1.53.1.0.tar.gz";
      sha256 = "afb38cd299ab9c8c3a7c50f3319c9f336ee22ebd284e0faa610689a80be3c90c";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            plutus-ledger-api\nversion:         1.53.1.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      michael.peyton-jones@iohk.io\nauthor:          Michael Peyton Jones, Jann Mueller\nsynopsis:        Interface to the Plutus ledger for the Cardano ledger.\ndescription:\n  Interface to the Plutus scripting support for the Cardano ledger.\n\ncategory:        Language\nbuild-type:      Simple\nextra-doc-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/IntersectMBO/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  -- See Plutus Tx readme for why we need the following flags:\n  -- -fobject-code -fno-ignore-interface-pragmas and -fno-omit-interface-pragmas\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n    -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n    -fno-strictness\n\ncommon ghc-version-support\n  -- This is only used for the plugin tests.\n  -- See the section on GHC versions in CONTRIBUTING\n  if (impl(ghc <9.6) || impl(ghc >=9.7))\n    buildable: False\n\nlibrary\n  import:           lang\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  exposed-modules:\n    PlutusLedgerApi.Common\n    PlutusLedgerApi.Common.Versions\n    PlutusLedgerApi.Data.V1\n    PlutusLedgerApi.Data.V2\n    PlutusLedgerApi.Data.V3\n    PlutusLedgerApi.Envelope\n    PlutusLedgerApi.MachineParameters\n    PlutusLedgerApi.V1\n    PlutusLedgerApi.V1.Address\n    PlutusLedgerApi.V1.Bytes\n    PlutusLedgerApi.V1.Contexts\n    PlutusLedgerApi.V1.Credential\n    PlutusLedgerApi.V1.Crypto\n    PlutusLedgerApi.V1.Data.Address\n    PlutusLedgerApi.V1.Data.Contexts\n    PlutusLedgerApi.V1.Data.Credential\n    PlutusLedgerApi.V1.Data.DCert\n    PlutusLedgerApi.V1.Data.Interval\n    PlutusLedgerApi.V1.Data.Time\n    PlutusLedgerApi.V1.Data.Tx\n    PlutusLedgerApi.V1.Data.Value\n    PlutusLedgerApi.V1.DCert\n    PlutusLedgerApi.V1.EvaluationContext\n    PlutusLedgerApi.V1.Interval\n    PlutusLedgerApi.V1.ParamName\n    PlutusLedgerApi.V1.Scripts\n    PlutusLedgerApi.V1.Time\n    PlutusLedgerApi.V1.Tx\n    PlutusLedgerApi.V1.Value\n    PlutusLedgerApi.V2\n    PlutusLedgerApi.V2.Contexts\n    PlutusLedgerApi.V2.Data.Contexts\n    PlutusLedgerApi.V2.Data.Tx\n    PlutusLedgerApi.V2.EvaluationContext\n    PlutusLedgerApi.V2.ParamName\n    PlutusLedgerApi.V2.Tx\n    PlutusLedgerApi.V3\n    PlutusLedgerApi.V3.Contexts\n    PlutusLedgerApi.V3.Data.Contexts\n    PlutusLedgerApi.V3.Data.MintValue\n    PlutusLedgerApi.V3.Data.Tx\n    PlutusLedgerApi.V3.EvaluationContext\n    PlutusLedgerApi.V3.MintValue\n    PlutusLedgerApi.V3.ParamName\n    PlutusLedgerApi.V3.Tx\n\n  other-modules:\n    PlutusLedgerApi.Common.Eval\n    PlutusLedgerApi.Common.ParamName\n    PlutusLedgerApi.Common.ProtocolVersions\n    PlutusLedgerApi.Common.SerialisedScript\n    Prettyprinter.Extras\n\n  build-depends:\n    , aeson\n    , aeson-pretty\n    , base               >=4.9       && <5\n    , base16-bytestring  >=1\n    , bytestring\n    , cborg\n    , containers\n    , deepseq\n    , lens\n    , mtl\n    , nothunks\n    , plutus-core        ^>=1.53.1.0\n    , plutus-tx          ^>=1.53.1.0\n    , prettyprinter\n    , serialise\n    , tagged\n    , text\n\nlibrary plutus-ledger-api-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    PlutusLedgerApi.Test.Common.EvaluationContext\n    PlutusLedgerApi.Test.EvaluationEvent\n    PlutusLedgerApi.Test.Examples\n    PlutusLedgerApi.Test.Scripts\n    PlutusLedgerApi.Test.V1.Data.EvaluationContext\n    PlutusLedgerApi.Test.V1.Data.Value\n    PlutusLedgerApi.Test.V1.EvaluationContext\n    PlutusLedgerApi.Test.V1.Value\n    PlutusLedgerApi.Test.V2.Data.EvaluationContext\n    PlutusLedgerApi.Test.V2.EvaluationContext\n    PlutusLedgerApi.Test.V3.Data.EvaluationContext\n    PlutusLedgerApi.Test.V3.Data.MintValue\n    PlutusLedgerApi.Test.V3.EvaluationContext\n    PlutusLedgerApi.Test.V3.MintValue\n\n  build-depends:\n    , barbies\n    , base                             >=4.9       && <5\n    , base16-bytestring\n    , base64-bytestring\n    , bytestring\n    , containers\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-ledger-api                ^>=1.53.1.0\n    , plutus-tx                        ^>=1.53.1.0\n    , prettyprinter\n    , QuickCheck\n    , serialise\n    , text\n\ntest-suite plutus-ledger-api-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: test\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    Spec.CBOR.DeserialiseFailureInfo\n    Spec.ContextDecoding\n    Spec.CostModelParams\n    Spec.Data.CostModelParams\n    Spec.Data.Eval\n    Spec.Data.Versions\n    Spec.Eval\n    Spec.Interval\n    Spec.ScriptDecodeError\n    Spec.V1.Data.Value\n    Spec.V1.Value\n    Spec.Versions\n\n  build-depends:\n    , base                                         >=4.9       && <5\n    , bytestring\n    , cborg\n    , containers\n    , extra\n    , hedgehog\n    , lens\n    , mtl\n    , nothunks\n    , plutus-core                                  ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-ledger-api                            ^>=1.53.1.0\n    , plutus-ledger-api:plutus-ledger-api-testlib\n    , plutus-tx                                    ^>=1.53.1.0\n    , plutus-tx:plutus-tx-testlib\n    , prettyprinter\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n\n-- A suite for tests that use the Plutus Tx plugin. We don't merge those into\n-- @plutus-ledger-api-test@, because @plutus-ledger-api@ has to be buildable for older versions of\n-- GHC (a requirement imposed by @cardano-node@) and while its tests don't have to, we don't want to\n-- give up on all @plutus-ledger-api@ tests for older versions of GHC.\ntest-suite plutus-ledger-api-plugin-test\n  import:         lang, ghc-version-support\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: test-plugin\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    Spec.Budget\n    Spec.Data.Budget\n    Spec.Data.MintValue.V3\n    Spec.Data.ScriptContext\n    Spec.Data.Value\n    Spec.Envelope\n    Spec.MintValue.V3\n    Spec.ReturnUnit.V1\n    Spec.ReturnUnit.V2\n    Spec.ReturnUnit.V3\n    Spec.ScriptSize\n    Spec.Value\n    Spec.Value.WithCurrencySymbol\n\n  if os(windows)\n    buildable: False\n\n  build-depends:\n    , base                                         >=4.9       && <5\n    , bytestring\n    , containers\n    , filepath\n    , lens\n    , mtl\n    , plutus-core                                  ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-ledger-api                            ^>=1.53.1.0\n    , plutus-ledger-api:plutus-ledger-api-testlib\n    , plutus-tx                                    ^>=1.53.1.0\n    , plutus-tx-plugin                             ^>=1.53.1.0\n    , plutus-tx:plutus-tx-testlib\n    , prettyprinter\n    , QuickCheck\n    , tasty\n    , tasty-golden\n    , tasty-hunit\n    , tasty-quickcheck\n\n-- This is a nightly test, so it is an executable instead of test-suite to avoid\n-- running this in CI.\nexecutable test-onchain-evaluation\n  import:           lang\n  main-is:          Main.hs\n  other-modules:    LoadScriptEvents\n  hs-source-dirs:   exe/test-onchain-evaluation exe/common\n  default-language: Haskell2010\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , async\n    , base                                         >=4.9       && <5\n    , extra\n    , filepath\n    , mtl\n    , plutus-core                                  ^>=1.53.1.0\n    , plutus-ledger-api                            ^>=1.53.1.0\n    , plutus-ledger-api:plutus-ledger-api-testlib\n    , serialise\n    , tasty\n    , tasty-hunit\n\nexecutable analyse-script-events\n  import:           lang\n  main-is:          Main.hs\n  other-modules:    LoadScriptEvents\n  hs-source-dirs:   exe/analyse-script-events exe/common\n  default-language: Haskell2010\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , base                                         >=4.9       && <5\n    , extra\n    , filepath\n    , lens\n    , mtl\n    , plutus-core                                  ^>=1.53.1.0\n    , plutus-ledger-api                            ^>=1.53.1.0\n    , plutus-ledger-api:plutus-ledger-api-testlib\n    , plutus-tx                                    ^>=1.53.1.0\n    , primitive\n    , serialise\n\nexecutable dump-cost-model-parameters\n  import:           lang\n  main-is:          Main.hs\n  other-modules:    Parsers\n  hs-source-dirs:   exe/dump-cost-model-parameters\n  default-language: Haskell2010\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , aeson\n    , aeson-pretty\n    , base                  >=4.9       && <5\n    , bytestring\n    , containers\n    , extra\n    , optparse-applicative\n    , plutus-core           ^>=1.53.1.0\n    , plutus-ledger-api     ^>=1.53.1.0\n    , text\n    , vector\n";
  }