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
      identifier = {
        name = "ouroboros-consensus-cardano";
        version = "0.26.0.2";
      };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "The instantation of the Ouroboros consensus layer used by Cardano";
      description = "The instantation of the Ouroboros consensus layer used by Cardano.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
          (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
          (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
        ];
        buildable = true;
      };
      sublibs = {
        "unstable-byronspec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
        "unstable-byron-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-byronspec or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-byronspec"))
          ];
          buildable = true;
        };
        "unstable-shelley-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-protocol-tpraos".components.sublibs.testlib or (errorHandler.buildDepError "cardano-protocol-tpraos:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
            (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-protocol".components.sublibs.unstable-protocol-testlib or (errorHandler.buildDepError "ouroboros-consensus-protocol:unstable-protocol-testlib"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          ];
          buildable = true;
        };
        "unstable-cardano-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion" or (errorHandler.buildDepError "ouroboros-consensus-diffusion"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-protocol".components.sublibs.unstable-protocol-testlib or (errorHandler.buildDepError "ouroboros-consensus-protocol:unstable-protocol-testlib"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-byron-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-byron-testlib"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-shelley-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-shelley-testlib"))
          ];
          buildable = true;
        };
        "unstable-cardano-tools" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
            (hsPkgs."cardano-ledger-allegra" or (errorHandler.buildDepError "cardano-ledger-allegra"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."compact" or (errorHandler.buildDepError "compact"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dot" or (errorHandler.buildDepError "dot"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."githash" or (errorHandler.buildDepError "githash"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion" or (errorHandler.buildDepError "ouroboros-consensus-diffusion"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-consensus-protocol".components.sublibs.unstable-protocol-testlib or (errorHandler.buildDepError "ouroboros-consensus-protocol:unstable-protocol-testlib"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          ];
          buildable = true;
        };
      };
      exes = {
        "db-analyser" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "db-immutaliser" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "db-synthesizer" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "db-truncater" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "immdb-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "snapshot-converter" = {
          depends = [
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."terminal-progress-bar" or (errorHandler.buildDepError "terminal-progress-bar"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "gen-header" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
      };
      tests = {
        "byron-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary-search" or (errorHandler.buildDepError "binary-search"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-byron-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-byron-testlib"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-byronspec or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-byronspec"))
          ];
          buildable = true;
        };
        "shelley-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-testlib"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-shelley-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-shelley-testlib"))
          ];
          buildable = true;
        };
        "cardano-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-allegra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-allegra:testlib"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-alonzo:testlib"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-babbage".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-babbage:testlib"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
            (hsPkgs."cardano-ledger-conway".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-conway:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-ledger-dijkstra".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-dijkstra:testlib"))
            (hsPkgs."cardano-ledger-mary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-mary:testlib"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-shelley:testlib"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-mempool-test-utils or (errorHandler.buildDepError "ouroboros-consensus:unstable-mempool-test-utils"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-testlib"))
            (hsPkgs."ouroboros-consensus-diffusion".components.sublibs.unstable-diffusion-testlib or (errorHandler.buildDepError "ouroboros-consensus-diffusion:unstable-diffusion-testlib"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-byron-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-byron-testlib"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-testlib"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-shelley-testlib or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-shelley-testlib"))
          ];
          buildable = true;
        };
        "tools-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
            (hsPkgs."ouroboros-consensus-protocol".components.sublibs.unstable-protocol-testlib or (errorHandler.buildDepError "ouroboros-consensus-protocol:unstable-protocol-testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ouroboros-consensus-cardano".components.sublibs.unstable-cardano-tools or (errorHandler.buildDepError "ouroboros-consensus-cardano:unstable-cardano-tools"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-consensus-cardano-0.26.0.2.tar.gz";
      sha256 = "9af13ea1c2fb31438d0602b4d4ca68b32588a6752081433a10213a6e37ed6d65";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-consensus-cardano\nversion: 0.26.0.2\nsynopsis:\n  The instantation of the Ouroboros consensus layer used by Cardano\n\ndescription:\n  The instantation of the Ouroboros consensus layer used by Cardano.\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:\n  2019-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\n\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ndata-files:\n  cddl/**/*.cddl\n  cddl/base.cddl\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/ouroboros-consensus\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\ncommon common-lib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\ncommon common-test\n  import: common-lib\n  ghc-options:\n    -threaded\n    -rtsopts\n\ncommon common-exe\n  import: common-lib\n  ghc-options:\n    -threaded\n    -rtsopts\n    \"-with-rtsopts=-N -I0 -A16m\"\n\nlibrary\n  import: common-lib\n  hs-source-dirs:\n    src/ouroboros-consensus-cardano\n    src/byron\n    src/shelley\n\n  exposed-modules:\n    Ouroboros.Consensus.Byron.ByronHFC\n    Ouroboros.Consensus.Byron.Crypto.DSIGN\n    Ouroboros.Consensus.Byron.EBBs\n    Ouroboros.Consensus.Byron.Ledger\n    Ouroboros.Consensus.Byron.Ledger.Block\n    Ouroboros.Consensus.Byron.Ledger.Config\n    Ouroboros.Consensus.Byron.Ledger.Conversions\n    Ouroboros.Consensus.Byron.Ledger.Forge\n    Ouroboros.Consensus.Byron.Ledger.HeaderValidation\n    Ouroboros.Consensus.Byron.Ledger.Inspect\n    Ouroboros.Consensus.Byron.Ledger.Integrity\n    Ouroboros.Consensus.Byron.Ledger.Ledger\n    Ouroboros.Consensus.Byron.Ledger.Mempool\n    Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion\n    Ouroboros.Consensus.Byron.Ledger.Orphans\n    Ouroboros.Consensus.Byron.Ledger.PBFT\n    Ouroboros.Consensus.Byron.Ledger.Serialisation\n    Ouroboros.Consensus.Byron.Node\n    Ouroboros.Consensus.Byron.Node.Serialisation\n    Ouroboros.Consensus.Byron.Protocol\n    Ouroboros.Consensus.Cardano\n    Ouroboros.Consensus.Cardano.Block\n    Ouroboros.Consensus.Cardano.CanHardFork\n    Ouroboros.Consensus.Cardano.Condense\n    Ouroboros.Consensus.Cardano.Ledger\n    Ouroboros.Consensus.Cardano.Node\n    Ouroboros.Consensus.Cardano.QueryHF\n    Ouroboros.Consensus.Cardano.StreamingLedgerTables\n    Ouroboros.Consensus.Shelley.Crypto\n    Ouroboros.Consensus.Shelley.Eras\n    Ouroboros.Consensus.Shelley.HFEras\n    Ouroboros.Consensus.Shelley.Ledger\n    Ouroboros.Consensus.Shelley.Ledger.Block\n    Ouroboros.Consensus.Shelley.Ledger.Config\n    Ouroboros.Consensus.Shelley.Ledger.Forge\n    Ouroboros.Consensus.Shelley.Ledger.Inspect\n    Ouroboros.Consensus.Shelley.Ledger.Integrity\n    Ouroboros.Consensus.Shelley.Ledger.Ledger\n    Ouroboros.Consensus.Shelley.Ledger.Mempool\n    Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion\n    Ouroboros.Consensus.Shelley.Ledger.PeerSelection\n    Ouroboros.Consensus.Shelley.Ledger.Protocol\n    Ouroboros.Consensus.Shelley.Ledger.Query\n    Ouroboros.Consensus.Shelley.Ledger.Query.LegacyPParams\n    Ouroboros.Consensus.Shelley.Ledger.Query.LegacyShelleyGenesis\n    Ouroboros.Consensus.Shelley.Ledger.Query.Types\n    Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol\n    Ouroboros.Consensus.Shelley.Node\n    Ouroboros.Consensus.Shelley.Node.Common\n    Ouroboros.Consensus.Shelley.Node.DiffusionPipelining\n    Ouroboros.Consensus.Shelley.Node.Praos\n    Ouroboros.Consensus.Shelley.Node.Serialisation\n    Ouroboros.Consensus.Shelley.Node.TPraos\n    Ouroboros.Consensus.Shelley.Protocol.Abstract\n    Ouroboros.Consensus.Shelley.Protocol.Praos\n    Ouroboros.Consensus.Shelley.Protocol.TPraos\n    Ouroboros.Consensus.Shelley.ShelleyHFC\n\n  build-depends:\n    base >=4.14 && <4.22,\n    base-deriving-via,\n    bytestring >=0.10 && <0.13,\n    cardano-binary,\n    cardano-crypto,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-wrapper,\n    cardano-ledger-allegra ^>=1.8,\n    cardano-ledger-alonzo ^>=1.14,\n    cardano-ledger-api ^>=1.12,\n    cardano-ledger-babbage ^>=1.12,\n    cardano-ledger-binary ^>=1.7,\n    cardano-ledger-byron ^>=1.2,\n    cardano-ledger-conway ^>=1.20,\n    cardano-ledger-core ^>=1.18,\n    cardano-ledger-dijkstra ^>=0.1,\n    cardano-ledger-mary ^>=1.9,\n    cardano-ledger-shelley ^>=1.17,\n    cardano-prelude,\n    cardano-protocol-tpraos ^>=1.4.1,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg ^>=0.2.2,\n    containers >=0.5 && <0.8,\n    contra-tracer,\n    crypton,\n    deepseq,\n    directory,\n    filepath,\n    formatting >=6.3 && <7.3,\n    fs-api,\n    measures,\n    mempack,\n    microlens,\n    mtl,\n    nothunks,\n    ouroboros-consensus ^>=0.28,\n    ouroboros-consensus-protocol ^>=0.13,\n    ouroboros-network-api ^>=0.16,\n    resource-registry,\n    serialise ^>=0.2,\n    singletons ^>=3.0,\n    small-steps,\n    sop-core ^>=0.5,\n    sop-extras ^>=0.4,\n    strict-sop-core ^>=0.1,\n    temporary,\n    text,\n    these ^>=1.2,\n    validation,\n    vector-map,\n\nlibrary unstable-byronspec\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-byronspec\n  exposed-modules:\n    Ouroboros.Consensus.ByronSpec.Ledger\n    Ouroboros.Consensus.ByronSpec.Ledger.Accessors\n    Ouroboros.Consensus.ByronSpec.Ledger.Block\n    Ouroboros.Consensus.ByronSpec.Ledger.Conversions\n    Ouroboros.Consensus.ByronSpec.Ledger.Forge\n    Ouroboros.Consensus.ByronSpec.Ledger.GenTx\n    Ouroboros.Consensus.ByronSpec.Ledger.Genesis\n    Ouroboros.Consensus.ByronSpec.Ledger.Ledger\n    Ouroboros.Consensus.ByronSpec.Ledger.Mempool\n    Ouroboros.Consensus.ByronSpec.Ledger.Orphans\n    Ouroboros.Consensus.ByronSpec.Ledger.Rules\n\n  build-depends:\n    base >=4.14 && <4.22,\n    bimap >=0.4 && <0.6,\n    byron-spec-chain,\n    byron-spec-ledger,\n    cardano-binary,\n    cardano-ledger-binary,\n    cardano-ledger-byron:testlib,\n    cborg >=0.2.2 && <0.3,\n    containers >=0.5 && <0.8,\n    mtl,\n    nothunks,\n    ouroboros-consensus ^>=0.28,\n    serialise ^>=0.2,\n    small-steps,\n    transformers,\n\nlibrary unstable-byron-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-byron-testlib\n  exposed-modules:\n    Ouroboros.Consensus.ByronDual.Ledger\n    Ouroboros.Consensus.ByronDual.Node\n    Ouroboros.Consensus.ByronDual.Node.Serialisation\n    Test.Consensus.Byron.Examples\n    Test.Consensus.Byron.Generators\n    Test.ThreadNet.Infra.Byron\n    Test.ThreadNet.Infra.Byron.Genesis\n    Test.ThreadNet.Infra.Byron.ProtocolInfo\n    Test.ThreadNet.Infra.Byron.TrackUpdates\n    Test.ThreadNet.TxGen.Byron\n\n  build-depends:\n    QuickCheck,\n    base,\n    base64-bytestring,\n    byron-spec-ledger,\n    bytestring,\n    cardano-binary,\n    cardano-crypto,\n    cardano-crypto-class,\n    cardano-crypto-test,\n    cardano-crypto-wrapper,\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-byron:{cardano-ledger-byron, testlib},\n    cardano-ledger-core,\n    containers,\n    hedgehog-quickcheck,\n    mtl,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion:unstable-diffusion-testlib,\n    ouroboros-network-api,\n    serialise,\n    text,\n    unstable-byronspec,\n\ntest-suite byron-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/byron-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.Byron.Golden\n    Test.Consensus.Byron.LedgerTables\n    Test.Consensus.Byron.Serialisation\n    Test.ThreadNet.Byron\n    Test.ThreadNet.DualByron\n\n  build-depends:\n    QuickCheck,\n    base,\n    binary-search,\n    byron-spec-chain,\n    byron-spec-ledger,\n    bytestring,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-ledger-binary,\n    cardano-ledger-byron:{cardano-ledger-byron, testlib},\n    cardano-ledger-core,\n    cardano-slotting:testlib,\n    cborg,\n    constraints,\n    containers,\n    filepath,\n    hedgehog-quickcheck,\n    mtl,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion:unstable-diffusion-testlib,\n    ouroboros-network-mock,\n    small-steps:{small-steps, testlib},\n    tasty,\n    tasty-quickcheck,\n    unstable-byron-testlib,\n    unstable-byronspec,\n\nlibrary unstable-shelley-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-shelley-testlib\n  exposed-modules:\n    Test.Consensus.Shelley.Examples\n    Test.Consensus.Shelley.Generators\n    Test.Consensus.Shelley.MockCrypto\n    Test.ThreadNet.Infra.Shelley\n    Test.ThreadNet.TxGen.Shelley\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-crypto-class,\n    cardano-data,\n    cardano-ledger-allegra:cardano-ledger-allegra,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-babbage:testlib,\n    cardano-ledger-conway:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra:testlib,\n    cardano-ledger-mary:{cardano-ledger-mary},\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos:{cardano-protocol-tpraos, testlib},\n    cardano-slotting,\n    cardano-strict-containers,\n    containers,\n    contra-tracer,\n    kes-agent <1,\n    kes-agent-crypto <1,\n    microlens,\n    mtl,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion:unstable-diffusion-testlib,\n    ouroboros-consensus-protocol:{ouroboros-consensus-protocol, unstable-protocol-testlib},\n    ouroboros-network-api,\n    quiet ^>=0.2,\n    small-steps,\n\ntest-suite shelley-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/shelley-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.Shelley.Coherence\n    Test.Consensus.Shelley.Golden\n    Test.Consensus.Shelley.LedgerTables\n    Test.Consensus.Shelley.Serialisation\n    Test.Consensus.Shelley.SupportedNetworkProtocolVersion\n    Test.ThreadNet.Shelley\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-api,\n    cardano-ledger-babbage:testlib,\n    cardano-ledger-conway:testlib,\n    cardano-ledger-core,\n    cardano-ledger-dijkstra:testlib,\n    cardano-ledger-shelley,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    constraints,\n    containers,\n    contra-tracer,\n    filepath,\n    measures,\n    microlens,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion:unstable-diffusion-testlib,\n    ouroboros-consensus-protocol,\n    sop-core,\n    strict-sop-core,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    unstable-cardano-testlib,\n    unstable-shelley-testlib,\n\nlibrary unstable-cardano-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-cardano-testlib\n  exposed-modules:\n    Test.Consensus.Cardano.Examples\n    Test.Consensus.Cardano.Generators\n    Test.Consensus.Cardano.MockCrypto\n    Test.Consensus.Cardano.ProtocolInfo\n    Test.ThreadNet.Infra.ShelleyBasedHardFork\n    Test.ThreadNet.Infra.TwoEras\n    Test.ThreadNet.TxGen.Allegra\n    Test.ThreadNet.TxGen.Alonzo\n    Test.ThreadNet.TxGen.Babbage\n    Test.ThreadNet.TxGen.Cardano\n    Test.ThreadNet.TxGen.Mary\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-ledger-alonzo:testlib,\n    cardano-ledger-api:{cardano-ledger-api},\n    cardano-ledger-binary,\n    cardano-ledger-byron,\n    cardano-ledger-conway:{cardano-ledger-conway, testlib},\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra:testlib,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley:testlib,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    contra-tracer,\n    mempack,\n    microlens,\n    mtl,\n    nothunks,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion:{ouroboros-consensus-diffusion, unstable-diffusion-testlib},\n    ouroboros-consensus-protocol:{ouroboros-consensus-protocol, unstable-protocol-testlib},\n    ouroboros-network-api,\n    sop-core,\n    sop-extras,\n    strict-sop-core,\n    text,\n    unstable-byron-testlib,\n    unstable-shelley-testlib,\n\ntest-suite cardano-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/cardano-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.Cardano.DiffusionPipelining\n    Test.Consensus.Cardano.GenCDDLs\n    Test.Consensus.Cardano.Golden\n    Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.ByteStringTxParser\n    Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server\n    Test.Consensus.Cardano.Serialisation\n    Test.Consensus.Cardano.Show\n    Test.Consensus.Cardano.SupportedNetworkProtocolVersion\n    Test.Consensus.Cardano.SupportsSanityCheck\n    Test.Consensus.Cardano.Translation\n    Test.ThreadNet.AllegraMary\n    Test.ThreadNet.Cardano\n    Test.ThreadNet.MaryAlonzo\n    Test.ThreadNet.ShelleyAllegra\n\n  other-modules: Paths_ouroboros_consensus_cardano\n  build-depends:\n    QuickCheck,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-ledger-allegra:testlib,\n    cardano-ledger-alonzo:{cardano-ledger-alonzo, testlib},\n    cardano-ledger-api,\n    cardano-ledger-babbage:testlib,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-byron:{cardano-ledger-byron, testlib},\n    cardano-ledger-conway:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-ledger-dijkstra:testlib,\n    cardano-ledger-mary:testlib,\n    cardano-ledger-shelley:{cardano-ledger-shelley, testlib},\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    constraints,\n    containers,\n    contra-tracer,\n    directory,\n    filepath,\n    microlens,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib, unstable-mempool-test-utils},\n    ouroboros-consensus-cardano:{ouroboros-consensus-cardano, unstable-cardano-testlib},\n    ouroboros-consensus-diffusion:unstable-diffusion-testlib,\n    ouroboros-consensus-protocol,\n    ouroboros-network-api,\n    ouroboros-network-protocols:{ouroboros-network-protocols, testlib},\n    pretty-simple,\n    process-extras,\n    sop-core,\n    sop-extras,\n    strict-sop-core,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    temporary,\n    typed-protocols ^>=1.0,\n    unstable-byron-testlib,\n    unstable-cardano-testlib,\n    unstable-shelley-testlib,\n\nlibrary unstable-cardano-tools\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-cardano-tools\n  exposed-modules:\n    Cardano.Api.Any\n    Cardano.Node.Types\n    Cardano.Tools.DBAnalyser.Analysis\n    Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.FileWriting\n    Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.Metadata\n    Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint\n    Cardano.Tools.DBAnalyser.Block.Byron\n    Cardano.Tools.DBAnalyser.Block.Cardano\n    Cardano.Tools.DBAnalyser.Block.Shelley\n    Cardano.Tools.DBAnalyser.CSV\n    Cardano.Tools.DBAnalyser.HasAnalysis\n    Cardano.Tools.DBAnalyser.Run\n    Cardano.Tools.DBAnalyser.Types\n    Cardano.Tools.DBImmutaliser.Run\n    Cardano.Tools.DBSynthesizer.Forging\n    Cardano.Tools.DBSynthesizer.Orphans\n    Cardano.Tools.DBSynthesizer.Run\n    Cardano.Tools.DBSynthesizer.Types\n    Cardano.Tools.DBTruncater.Run\n    Cardano.Tools.DBTruncater.Types\n    Cardano.Tools.GitRev\n    Cardano.Tools.Headers\n    Cardano.Tools.ImmDBServer.Diffusion\n    Cardano.Tools.ImmDBServer.MiniProtocols\n\n  other-modules:\n    Cardano.Api.Key\n    Cardano.Api.KeysByron\n    Cardano.Api.KeysPraos\n    Cardano.Api.KeysShelley\n    Cardano.Api.OperationalCertificate\n    Cardano.Api.SerialiseTextEnvelope\n    Cardano.Api.SerialiseUsing\n    Cardano.Node.Protocol.Alonzo\n    Cardano.Node.Protocol.Byron\n    Cardano.Node.Protocol.Cardano\n    Cardano.Node.Protocol.Conway\n    Cardano.Node.Protocol.Shelley\n\n  build-depends:\n    aeson,\n    base >=4.14 && <4.22,\n    base16-bytestring >=1.0,\n    bytestring >=0.10 && <0.13,\n    cardano-crypto,\n    cardano-crypto-class,\n    cardano-crypto-wrapper,\n    cardano-git-rev ^>=0.2.1,\n    cardano-ledger-allegra,\n    cardano-ledger-alonzo,\n    cardano-ledger-api,\n    cardano-ledger-babbage,\n    cardano-ledger-binary,\n    cardano-ledger-byron,\n    cardano-ledger-conway,\n    cardano-ledger-core,\n    cardano-ledger-dijkstra,\n    cardano-ledger-mary,\n    cardano-ledger-shelley,\n    cardano-prelude,\n    cardano-protocol-tpraos ^>=1.4.1,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg ^>=0.2.2,\n    compact,\n    containers >=0.5 && <0.8,\n    contra-tracer,\n    directory,\n    dot,\n    filepath,\n    fs-api ^>=0.4,\n    githash,\n    microlens,\n    mtl,\n    network,\n    network-mux,\n    nothunks,\n    ouroboros-consensus ^>=0.28,\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-diffusion ^>=0.24,\n    ouroboros-consensus-protocol:{ouroboros-consensus-protocol, unstable-protocol-testlib} ^>=0.13,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-framework ^>=0.19,\n    ouroboros-network-protocols,\n    resource-registry,\n    singletons,\n    sop-core,\n    sop-extras,\n    strict-sop-core,\n    text,\n    text-builder >=1,\n    transformers,\n    transformers-except,\n\nexecutable db-analyser\n  import: common-lib\n  hs-source-dirs: app\n  main-is: db-analyser.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    optparse-applicative,\n    ouroboros-consensus,\n    ouroboros-consensus-cardano:{ouroboros-consensus-cardano, unstable-cardano-tools},\n    text,\n    with-utf8,\n\n  -- NOTE: these options should match the ones in the cardano-node.\n  --\n  -- 'db-analyser' is often used as a benchmarking tool. Thus, by using\n  -- the same GHC flags as the node, we are more likely to get\n  -- performance observations that correspond to those we get from a\n  -- running node.\n  ghc-options:\n    -threaded\n    -rtsopts\n\n  if arch(arm)\n    ghc-options:\n      \"-with-rtsopts=-T -I0 -A16m -N1 --disable-delayed-os-memory-return\"\n  else\n    ghc-options:\n      \"-with-rtsopts=-T -I0 -A16m -N2 --disable-delayed-os-memory-return\"\n\n  other-modules: DBAnalyser.Parsers\n\nexecutable db-immutaliser\n  import: common-exe\n  hs-source-dirs: app\n  main-is: db-immutaliser.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    optparse-applicative,\n    ouroboros-consensus-cardano:unstable-cardano-tools,\n    with-utf8,\n\nexecutable db-synthesizer\n  import: common-exe\n  hs-source-dirs: app\n  main-is: db-synthesizer.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    optparse-applicative,\n    ouroboros-consensus,\n    unstable-cardano-tools,\n    with-utf8,\n\n  other-modules: DBSynthesizer.Parsers\n\nexecutable db-truncater\n  import: common-exe\n  hs-source-dirs: app\n  main-is: db-truncater.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    optparse-applicative,\n    ouroboros-consensus,\n    ouroboros-consensus-cardano:{ouroboros-consensus-cardano, unstable-cardano-tools},\n    with-utf8,\n\n  other-modules:\n    DBAnalyser.Parsers\n    DBTruncater.Parsers\n\nexecutable immdb-server\n  import: common-exe\n  hs-source-dirs: app\n  main-is: immdb-server.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    network,\n    optparse-applicative,\n    ouroboros-consensus,\n    ouroboros-consensus-cardano:unstable-cardano-tools,\n    with-utf8,\n\nexecutable snapshot-converter\n  import: common-exe\n  hs-source-dirs: app\n  main-is: snapshot-converter.hs\n  build-depends:\n    ansi-terminal,\n    base,\n    cardano-crypto-class,\n    directory,\n    filepath,\n    fs-api,\n    mtl,\n    optparse-applicative,\n    ouroboros-consensus,\n    ouroboros-consensus-cardano,\n    ouroboros-consensus-cardano:unstable-cardano-tools,\n    resource-registry,\n    serialise,\n    terminal-progress-bar,\n    text,\n    with-utf8,\n\n  other-modules:\n    DBAnalyser.Parsers\n\ntest-suite tools-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/tools-test\n  main-is: Main.hs\n  build-depends:\n    QuickCheck,\n    aeson,\n    base,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    ouroboros-consensus-cardano:{ouroboros-consensus-cardano, unstable-cardano-tools},\n    ouroboros-consensus-protocol:unstable-protocol-testlib,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    text,\n    unstable-cardano-tools,\n\n  other-modules:\n    Test.Cardano.Tools.Headers\n\nexecutable gen-header\n  import: common-exe\n  hs-source-dirs: app\n  main-is: gen-header.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    optparse-applicative,\n    ouroboros-consensus-cardano:unstable-cardano-tools,\n    with-utf8,\n\n  other-modules:\n    GenHeader.Parsers\n    Paths_ouroboros_consensus_cardano\n\n  autogen-modules:\n    Paths_ouroboros_consensus_cardano\n";
  }