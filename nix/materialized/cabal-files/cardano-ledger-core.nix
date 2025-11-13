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
    flags = { asserts = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-ledger-core"; version = "1.18.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Core components of Cardano ledgers from the Shelley release on.";
      description = "Cardano ledgers from the Shelley release onwards share a core basis rooted in\nthe Shelley ledger specification. This package abstracts a number of components\nwhich we expect to be shared amongst all future ledgers implemented around this base.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
        ];
        buildable = true;
      };
      sublibs = {
        "internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          ];
          buildable = true;
        };
        "testlib" = {
          depends = [
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."ImpSpec" or (errorHandler.buildDepError "ImpSpec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-crypto-wrapper".components.sublibs.testlib or (errorHandler.buildDepError "cardano-crypto-wrapper:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."heredoc" or (errorHandler.buildDepError "heredoc"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."quickcheck-transformer" or (errorHandler.buildDepError "quickcheck-transformer"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."vector-map".components.sublibs.testlib or (errorHandler.buildDepError "vector-map:testlib"))
          ];
          buildable = true;
        };
      };
      exes = {
        "plutus-debug" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
            (hsPkgs."genvalidity-scientific" or (errorHandler.buildDepError "genvalidity-scientific"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "umap" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
          ];
          buildable = true;
        };
        "addr" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-core-1.18.0.0.tar.gz";
      sha256 = "80d00b252710f19f4c7b4dc5161bbe9145e0323d2589ebb0587b5bd65662ad1f";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-core\nversion: 1.18.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis:\n  Core components of Cardano ledgers from the Shelley release on.\n\ndescription:\n  Cardano ledgers from the Shelley release onwards share a core basis rooted in\n  the Shelley ledger specification. This package abstracts a number of components\n  which we expect to be shared amongst all future ledgers implemented around this base.\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/cardano-ledger-core\n\nflag asserts\n  description: Enable assertions\n  default: False\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Address\n    Cardano.Ledger.AuxiliaryData\n    Cardano.Ledger.BHeaderView\n    Cardano.Ledger.BaseTypes\n    Cardano.Ledger.BaseTypes.NonZero\n    Cardano.Ledger.Block\n    Cardano.Ledger.Coin\n    Cardano.Ledger.Compactible\n    Cardano.Ledger.Core\n    Cardano.Ledger.Credential\n    Cardano.Ledger.Crypto\n    Cardano.Ledger.Crypto.Internal\n    Cardano.Ledger.DRep\n    Cardano.Ledger.EpochBoundary\n    Cardano.Ledger.Genesis\n    Cardano.Ledger.HKD\n    Cardano.Ledger.Hashes\n    Cardano.Ledger.Keys\n    Cardano.Ledger.Keys.Bootstrap\n    Cardano.Ledger.Keys.WitVKey\n    Cardano.Ledger.MemoBytes\n    Cardano.Ledger.MemoBytes.Internal\n    Cardano.Ledger.Metadata\n    Cardano.Ledger.Orphans\n    Cardano.Ledger.Plutus\n    Cardano.Ledger.Plutus.CostModels\n    Cardano.Ledger.Plutus.Data\n    Cardano.Ledger.Plutus.Evaluate\n    Cardano.Ledger.Plutus.ExUnits\n    Cardano.Ledger.Plutus.Language\n    Cardano.Ledger.Plutus.ToPlutusData\n    Cardano.Ledger.Plutus.TxInfo\n    Cardano.Ledger.PoolDistr\n    Cardano.Ledger.PoolParams\n    Cardano.Ledger.Rewards\n    Cardano.Ledger.Rules.ValidationMode\n    Cardano.Ledger.SafeHash\n    Cardano.Ledger.Slot\n    Cardano.Ledger.State\n    Cardano.Ledger.Tools\n    Cardano.Ledger.TxIn\n    Cardano.Ledger.UMap\n    Cardano.Ledger.UTxO\n    Cardano.Ledger.Val\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Core.Era\n    Cardano.Ledger.Core.PParams\n    Cardano.Ledger.Core.Translation\n    Cardano.Ledger.Core.TxCert\n    Cardano.Ledger.Keys.Internal\n    Cardano.Ledger.State.Account\n    Cardano.Ledger.State.CertState\n    Cardano.Ledger.State.ChainAccount\n    Cardano.Ledger.State.Governance\n    Cardano.Ledger.State.PoolDistr\n    Cardano.Ledger.State.SnapShots\n    Cardano.Ledger.State.Stake\n    Cardano.Ledger.State.StakePool\n    Cardano.Ledger.State.UTxO\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    aeson >=2,\n    base >=4.18 && <5,\n    base-deriving-via,\n    base16-bytestring,\n    base64-bytestring,\n    binary,\n    bytestring >=0.10 && <0.11.3 || >=0.11.4,\n    cardano-crypto,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-praos ^>=2.2,\n    cardano-crypto-wrapper,\n    cardano-data ^>=1.2,\n    cardano-ledger-binary ^>=1.7,\n    cardano-ledger-byron,\n    cardano-ledger-core:internal,\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    data-default >=0.8,\n    deepseq,\n    groups,\n    iproute,\n    measures,\n    mempack ^>=0.1,\n    microlens,\n    mtl,\n    non-integral >=1.0,\n    nothunks >=0.1.5 && <0.3,\n    partial-order,\n    plutus-core,\n    plutus-ledger-api,\n    prettyprinter,\n    primitive,\n    quiet,\n    random,\n    scientific,\n    serialise,\n    small-steps >=1.1,\n    text,\n    time,\n    transformers,\n    utf8-string,\n    validation-selective,\n    vector-map ^>=1.1,\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\nlibrary internal\n  build-depends:\n    base,\n    cardano-ledger-binary,\n\n  exposed-modules:\n    Cardano.Ledger.Internal.Era\n\n  other-modules:\n    Cardano.Ledger.Internal.Definition.Era\n\n  visibility: public\n  hs-source-dirs: internal\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Common\n    Test.Cardano.Ledger.Core.Address\n    Test.Cardano.Ledger.Core.Arbitrary\n    Test.Cardano.Ledger.Core.Binary\n    Test.Cardano.Ledger.Core.Binary.Annotator\n    Test.Cardano.Ledger.Core.Binary.CDDL\n    Test.Cardano.Ledger.Core.Binary.RoundTrip\n    Test.Cardano.Ledger.Core.JSON\n    Test.Cardano.Ledger.Core.KeyPair\n    Test.Cardano.Ledger.Core.Rational\n    Test.Cardano.Ledger.Core.Utils\n    Test.Cardano.Ledger.Era\n    Test.Cardano.Ledger.Imp.Common\n    Test.Cardano.Ledger.Plutus\n    Test.Cardano.Ledger.Plutus.ExUnits\n    Test.Cardano.Ledger.Plutus.Examples\n    Test.Cardano.Ledger.Plutus.Guardrail\n    Test.Cardano.Ledger.Plutus.ScriptTestContext\n    Test.Cardano.Ledger.Plutus.ToPlutusData\n    Test.Cardano.Ledger.TreeDiff\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    ImpSpec,\n    QuickCheck,\n    aeson,\n    aeson-pretty,\n    base,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-crypto-class,\n    cardano-crypto-wrapper:{cardano-crypto-wrapper, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib} >=1.5,\n    cardano-ledger-byron:{cardano-ledger-byron, testlib},\n    cardano-ledger-core,\n    cardano-slotting,\n    containers,\n    cuddle >=0.4,\n    data-default,\n    deepseq,\n    generic-random,\n    genvalidity,\n    hedgehog-quickcheck,\n    heredoc,\n    hspec,\n    microlens,\n    mtl,\n    nothunks,\n    plutus-ledger-api:{plutus-ledger-api, plutus-ledger-api-testlib},\n    primitive,\n    quickcheck-transformer,\n    random ^>=1.2,\n    small-steps >=1.1,\n    text,\n    time,\n    tree-diff,\n    unliftio,\n    vector-map:{vector-map, testlib},\n\nexecutable plutus-debug\n  main-is: PlutusDebug.hs\n  hs-source-dirs: app\n  other-modules:\n    CLI\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    base >=4.18 && <5,\n    cardano-ledger-binary,\n    cardano-ledger-core,\n    optparse-applicative,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.AddressSpec\n    Test.Cardano.Ledger.BaseTypesSpec\n    Test.Cardano.Ledger.BinarySpec\n    Test.Cardano.Ledger.JsonSpec\n    Test.Cardano.Ledger.PlutusSpec\n    Test.Cardano.Ledger.State.StakePoolSpec\n    Test.Cardano.Ledger.ToolsSpec\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    FailT,\n    aeson,\n    base,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-crypto-class,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-byron,\n    cardano-ledger-core,\n    containers,\n    genvalidity,\n    genvalidity-scientific,\n    plutus-ledger-api,\n    quickcheck-instances,\n    scientific,\n    testlib,\n\nbenchmark umap\n  type: exitcode-stdio-1.0\n  main-is: UMap.hs\n  hs-source-dirs: bench\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -O2\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-ledger-core,\n    containers,\n    criterion,\n    testlib,\n\nbenchmark addr\n  type: exitcode-stdio-1.0\n  main-is: Addr.hs\n  hs-source-dirs: bench\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -O2\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-core,\n    criterion,\n    testlib,\n";
  }