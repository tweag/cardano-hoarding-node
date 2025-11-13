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
      identifier = { name = "cardano-ledger-api"; version = "1.12.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Public API for the cardano ledger codebase";
      description = "This package a public interface into the cardano ledger codebase. It also\nprovides functionality that works for all eras and is useful for downstream\npackages.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-core".components.sublibs.internal or (errorHandler.buildDepError "cardano-ledger-core:internal"))
          (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          ];
          buildable = true;
        };
      };
      tests = {
        "cardano-ledger-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."cardano-ledger-api".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-api:testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-api-1.12.0.0.tar.gz";
      sha256 = "095abc80deed58c14c499ca9adbf6ed473799a42ea40ab14e449c6d034803587";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-api\nversion: 1.12.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nbug-reports: https://github.com/intersectmbo/cardano-ledger/issues\nsynopsis: Public API for the cardano ledger codebase\ndescription:\n  This package a public interface into the cardano ledger codebase. It also\n  provides functionality that works for all eras and is useful for downstream\n  packages.\n\nbuild-type: Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/cardano-ledger-api\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Api\n    Cardano.Ledger.Api.Era\n    Cardano.Ledger.Api.Governance\n    Cardano.Ledger.Api.PParams\n    Cardano.Ledger.Api.Scripts\n    Cardano.Ledger.Api.Scripts.Data\n    Cardano.Ledger.Api.State.Query\n    Cardano.Ledger.Api.Transition\n    Cardano.Ledger.Api.Tx\n    Cardano.Ledger.Api.Tx.Address\n    Cardano.Ledger.Api.Tx.AuxData\n    Cardano.Ledger.Api.Tx.Body\n    Cardano.Ledger.Api.Tx.Cert\n    Cardano.Ledger.Api.Tx.In\n    Cardano.Ledger.Api.Tx.Out\n    Cardano.Ledger.Api.Tx.Wits\n    Cardano.Ledger.Api.UTxO\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Api.Scripts.ExUnits\n    Cardano.Ledger.Api.State.Query.CommitteeMembersState\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    aeson >=2.2,\n    base >=4.18 && <5,\n    bytestring,\n    cardano-data,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-alonzo >=1.12,\n    cardano-ledger-babbage >=1.11,\n    cardano-ledger-binary >=1.4,\n    cardano-ledger-conway >=1.19,\n    cardano-ledger-core:{cardano-ledger-core, internal} >=1.17,\n    cardano-ledger-dijkstra >=0.1,\n    cardano-ledger-mary ^>=1.9,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    microlens,\n    transformers,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Api.Arbitrary\n    Test.Cardano.Ledger.Api.DebugTools\n    Test.Cardano.Ledger.Api.State.Query\n    Test.Cardano.Ledger.Api.Upgrade\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-ledger-api,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra:testlib,\n    containers,\n    data-default,\n    prettyprinter,\n\ntest-suite cardano-ledger-api-test\n  type: exitcode-stdio-1.0\n  main-is: Tests.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.Api.State.Imp.QuerySpec\n    Test.Cardano.Ledger.Api.State.QuerySpec\n    Test.Cardano.Ledger.Api.Tx\n    Test.Cardano.Ledger.Api.Tx.Body\n    Test.Cardano.Ledger.Api.Tx.Out\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wpartial-fields\n    -Wunused-packages\n    -rtsopts\n    -threaded\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-data,\n    cardano-ledger-allegra:{cardano-ledger-allegra, testlib},\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-api,\n    cardano-ledger-babbage:{cardano-ledger-babbage, testlib},\n    cardano-ledger-binary,\n    cardano-ledger-byron,\n    cardano-ledger-conway:{cardano-ledger-conway, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra:{cardano-ledger-dijkstra, testlib},\n    cardano-ledger-mary:{cardano-ledger-mary, testlib},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-slotting:testlib,\n    cardano-strict-containers,\n    containers,\n    data-default,\n    microlens,\n    microlens-mtl,\n    testlib,\n";
  }