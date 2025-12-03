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
    flags = { asserts = false; expensive-invariants = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "ouroboros-consensus"; version = "0.28.0.2"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Consensus layer for the Ouroboros blockchain protocol";
      description = "Consensus layer for the Ouroboros blockchain protocol.";
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
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
          (hsPkgs."cardano-lmdb-simple" or (errorHandler.buildDepError "cardano-lmdb-simple"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."diff-containers" or (errorHandler.buildDepError "diff-containers"))
          (hsPkgs."filelock" or (errorHandler.buildDepError "filelock"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fingertree-rm" or (errorHandler.buildDepError "fingertree-rm"))
          (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
          (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."monoid-subclasses" or (errorHandler.buildDepError "monoid-subclasses"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
          (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."rawlock" or (errorHandler.buildDepError "rawlock"))
          (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."strict-checked-vars" or (errorHandler.buildDepError "strict-checked-vars"))
          (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "unstable-consensus-testlib" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-binary:testlib"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-state-machine".components.sublibs.no-vendored-treediff or (errorHandler.buildDepError "quickcheck-state-machine:no-vendored-treediff"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "unstable-mock-block" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
          ];
          buildable = true;
        };
        "unstable-mempool-test-utils" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
          ];
          buildable = true;
        };
        "unstable-tutorials" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          ];
          buildable = true;
        };
      };
      tests = {
        "consensus-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."diff-containers" or (errorHandler.buildDepError "diff-containers"))
            (hsPkgs."fingertree-rm" or (errorHandler.buildDepError "fingertree-rm"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nonempty-containers" or (errorHandler.buildDepError "nonempty-containers"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."ouroboros-network-protocols".components.sublibs.testlib or (errorHandler.buildDepError "ouroboros-network-protocols:testlib"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
            (hsPkgs."quickcheck-monoid-subclasses" or (errorHandler.buildDepError "quickcheck-monoid-subclasses"))
            (hsPkgs."quickcheck-state-machine".components.sublibs.no-vendored-treediff or (errorHandler.buildDepError "quickcheck-state-machine:no-vendored-treediff"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
            (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-mock-block or (errorHandler.buildDepError "ouroboros-consensus:unstable-mock-block"))
          ];
          buildable = true;
        };
        "infra-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "storage-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-core".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-core:testlib"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."diff-containers" or (errorHandler.buildDepError "diff-containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fs-api" or (errorHandler.buildDepError "fs-api"))
            (hsPkgs."fs-sim" or (errorHandler.buildDepError "fs-sim"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
            (hsPkgs."quickcheck-lockstep" or (errorHandler.buildDepError "quickcheck-lockstep"))
            (hsPkgs."quickcheck-state-machine".components.sublibs.no-vendored-treediff or (errorHandler.buildDepError "quickcheck-state-machine:no-vendored-treediff"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."latex-svg-image" or (errorHandler.buildDepError "latex-svg-image"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "mempool-bench" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-mempool-test-utils or (errorHandler.buildDepError "ouroboros-consensus:unstable-mempool-test-utils"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "ChainSync-client-bench" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."ouroboros-network-mock" or (errorHandler.buildDepError "ouroboros-network-mock"))
            (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
        "PerasCertDB-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."ouroboros-consensus".components.sublibs.unstable-consensus-testlib or (errorHandler.buildDepError "ouroboros-consensus:unstable-consensus-testlib"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-consensus-0.28.0.2.tar.gz";
      sha256 = "2861b865a30db0a8271182d4cf23a58e22ba2defad316822f8cea988d2d1876d";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-consensus\nversion: 0.28.0.2\nsynopsis: Consensus layer for the Ouroboros blockchain protocol\ndescription: Consensus layer for the Ouroboros blockchain protocol.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright:\n  2019-2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\n\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  docs/haddocks/*.svg\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/ouroboros-consensus\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\nflag expensive-invariants\n  description: Enable checks for expensive invariants\n  manual: True\n  default: False\n\ncommon common-lib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n    cpp-options: -DENABLE_ASSERTIONS\n\n  if flag(expensive-invariants)\n    cpp-options: -DENABLE_EXPENSIVE_INVARIANTS\n\ncommon common-test\n  import: common-lib\n  ghc-options:\n    -threaded\n    -rtsopts\n\ncommon common-bench\n  import: common-test\n  ghc-options:\n    -threaded\n    -rtsopts\n\n  -- We use this option to avoid skewed results due to changes in cache-line\n  -- alignment. See\n  -- https://github.com/Bodigrim/tasty-bench#comparison-against-baseline\n  ghc-options: -fproc-alignment=64\n\nlibrary\n  import: common-lib\n  hs-source-dirs: src/ouroboros-consensus\n  exposed-modules:\n    Ouroboros.Consensus.Block\n    Ouroboros.Consensus.Block.Abstract\n    Ouroboros.Consensus.Block.EBB\n    Ouroboros.Consensus.Block.Forging\n    Ouroboros.Consensus.Block.NestedContent\n    Ouroboros.Consensus.Block.RealPoint\n    Ouroboros.Consensus.Block.SupportsDiffusionPipelining\n    Ouroboros.Consensus.Block.SupportsMetrics\n    Ouroboros.Consensus.Block.SupportsPeras\n    Ouroboros.Consensus.Block.SupportsProtocol\n    Ouroboros.Consensus.Block.SupportsSanityCheck\n    Ouroboros.Consensus.BlockchainTime\n    Ouroboros.Consensus.BlockchainTime.API\n    Ouroboros.Consensus.BlockchainTime.WallClock.Default\n    Ouroboros.Consensus.BlockchainTime.WallClock.HardFork\n    Ouroboros.Consensus.BlockchainTime.WallClock.Simple\n    Ouroboros.Consensus.BlockchainTime.WallClock.Types\n    Ouroboros.Consensus.BlockchainTime.WallClock.Util\n    Ouroboros.Consensus.Config\n    Ouroboros.Consensus.Config.SecurityParam\n    Ouroboros.Consensus.Config.SupportsNode\n    Ouroboros.Consensus.Forecast\n    Ouroboros.Consensus.Fragment.Diff\n    Ouroboros.Consensus.Fragment.Validated\n    Ouroboros.Consensus.Fragment.ValidatedDiff\n    Ouroboros.Consensus.Genesis.Governor\n    Ouroboros.Consensus.HardFork.Abstract\n    Ouroboros.Consensus.HardFork.Combinator\n    Ouroboros.Consensus.HardFork.Combinator.Abstract\n    Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork\n    Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks\n    Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock\n    Ouroboros.Consensus.HardFork.Combinator.AcrossEras\n    Ouroboros.Consensus.HardFork.Combinator.Basics\n    Ouroboros.Consensus.HardFork.Combinator.Block\n    Ouroboros.Consensus.HardFork.Combinator.Condense\n    Ouroboros.Consensus.HardFork.Combinator.Degenerate\n    Ouroboros.Consensus.HardFork.Combinator.Embed.Binary\n    Ouroboros.Consensus.HardFork.Combinator.Embed.Nary\n    Ouroboros.Consensus.HardFork.Combinator.Embed.Unary\n    Ouroboros.Consensus.HardFork.Combinator.Forging\n    Ouroboros.Consensus.HardFork.Combinator.Info\n    Ouroboros.Consensus.HardFork.Combinator.InjectTxs\n    Ouroboros.Consensus.HardFork.Combinator.Ledger\n    Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams\n    Ouroboros.Consensus.HardFork.Combinator.Ledger.PeerSelection\n    Ouroboros.Consensus.HardFork.Combinator.Ledger.Query\n    Ouroboros.Consensus.HardFork.Combinator.Lifting\n    Ouroboros.Consensus.HardFork.Combinator.Mempool\n    Ouroboros.Consensus.HardFork.Combinator.NetworkVersion\n    Ouroboros.Consensus.HardFork.Combinator.Node\n    Ouroboros.Consensus.HardFork.Combinator.Node.DiffusionPipelining\n    Ouroboros.Consensus.HardFork.Combinator.Node.InitStorage\n    Ouroboros.Consensus.HardFork.Combinator.Node.Metrics\n    Ouroboros.Consensus.HardFork.Combinator.Node.SanityCheck\n    Ouroboros.Consensus.HardFork.Combinator.PartialConfig\n    Ouroboros.Consensus.HardFork.Combinator.Protocol\n    Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel\n    Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView\n    Ouroboros.Consensus.HardFork.Combinator.Serialisation\n    Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common\n    Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk\n    Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient\n    Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode\n    Ouroboros.Consensus.HardFork.Combinator.State\n    Ouroboros.Consensus.HardFork.Combinator.State.Infra\n    Ouroboros.Consensus.HardFork.Combinator.State.Instances\n    Ouroboros.Consensus.HardFork.Combinator.State.Lift\n    Ouroboros.Consensus.HardFork.Combinator.State.Types\n    Ouroboros.Consensus.HardFork.Combinator.Translation\n    Ouroboros.Consensus.HardFork.History\n    Ouroboros.Consensus.HardFork.History.Caching\n    Ouroboros.Consensus.HardFork.History.EpochInfo\n    Ouroboros.Consensus.HardFork.History.EraParams\n    Ouroboros.Consensus.HardFork.History.Qry\n    Ouroboros.Consensus.HardFork.History.Summary\n    Ouroboros.Consensus.HardFork.History.Util\n    Ouroboros.Consensus.HardFork.Simple\n    Ouroboros.Consensus.HeaderStateHistory\n    Ouroboros.Consensus.HeaderValidation\n    Ouroboros.Consensus.Ledger.Abstract\n    Ouroboros.Consensus.Ledger.Basics\n    Ouroboros.Consensus.Ledger.CommonProtocolParams\n    Ouroboros.Consensus.Ledger.Dual\n    Ouroboros.Consensus.Ledger.Extended\n    Ouroboros.Consensus.Ledger.Inspect\n    Ouroboros.Consensus.Ledger.Query\n    Ouroboros.Consensus.Ledger.Query.Version\n    Ouroboros.Consensus.Ledger.SupportsMempool\n    Ouroboros.Consensus.Ledger.SupportsPeerSelection\n    Ouroboros.Consensus.Ledger.SupportsProtocol\n    Ouroboros.Consensus.Ledger.Tables\n    Ouroboros.Consensus.Ledger.Tables.Basics\n    Ouroboros.Consensus.Ledger.Tables.Combinators\n    Ouroboros.Consensus.Ledger.Tables.Diff\n    Ouroboros.Consensus.Ledger.Tables.MapKind\n    Ouroboros.Consensus.Ledger.Tables.Utils\n    Ouroboros.Consensus.Mempool\n    Ouroboros.Consensus.Mempool.API\n    Ouroboros.Consensus.Mempool.Capacity\n    Ouroboros.Consensus.Mempool.Impl.Common\n    Ouroboros.Consensus.Mempool.Init\n    Ouroboros.Consensus.Mempool.Query\n    Ouroboros.Consensus.Mempool.TxSeq\n    Ouroboros.Consensus.Mempool.Update\n    Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface\n    Ouroboros.Consensus.MiniProtocol.BlockFetch.Server\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Client\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State\n    Ouroboros.Consensus.MiniProtocol.ChainSync.Server\n    Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server\n    Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server\n    Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server\n    Ouroboros.Consensus.Node.GsmState\n    Ouroboros.Consensus.Node.InitStorage\n    Ouroboros.Consensus.Node.NetworkProtocolVersion\n    Ouroboros.Consensus.Node.ProtocolInfo\n    Ouroboros.Consensus.Node.Run\n    Ouroboros.Consensus.Node.Serialisation\n    Ouroboros.Consensus.NodeId\n    Ouroboros.Consensus.Peras.Weight\n    Ouroboros.Consensus.Protocol.Abstract\n    Ouroboros.Consensus.Protocol.BFT\n    Ouroboros.Consensus.Protocol.LeaderSchedule\n    Ouroboros.Consensus.Protocol.MockChainSel\n    Ouroboros.Consensus.Protocol.ModChainSel\n    Ouroboros.Consensus.Protocol.PBFT\n    Ouroboros.Consensus.Protocol.PBFT.Crypto\n    Ouroboros.Consensus.Protocol.PBFT.State\n    Ouroboros.Consensus.Protocol.Signed\n    Ouroboros.Consensus.Storage.ChainDB\n    Ouroboros.Consensus.Storage.ChainDB.API\n    Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment\n    Ouroboros.Consensus.Storage.ChainDB.Impl\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Args\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Background\n    Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache\n    Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Follower\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Paths\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Query\n    Ouroboros.Consensus.Storage.ChainDB.Impl.Types\n    Ouroboros.Consensus.Storage.ChainDB.Init\n    Ouroboros.Consensus.Storage.Common\n    Ouroboros.Consensus.Storage.ImmutableDB\n    Ouroboros.Consensus.Storage.ImmutableDB.API\n    Ouroboros.Consensus.Storage.ImmutableDB.Chunks\n    Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal\n    Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Parser\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.State\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util\n    Ouroboros.Consensus.Storage.ImmutableDB.Impl.Validation\n    Ouroboros.Consensus.Storage.ImmutableDB.Stream\n    Ouroboros.Consensus.Storage.LedgerDB\n    Ouroboros.Consensus.Storage.LedgerDB.API\n    Ouroboros.Consensus.Storage.LedgerDB.Args\n    Ouroboros.Consensus.Storage.LedgerDB.Forker\n    Ouroboros.Consensus.Storage.LedgerDB.Snapshots\n    Ouroboros.Consensus.Storage.LedgerDB.TraceEvent\n    Ouroboros.Consensus.Storage.LedgerDB.V1\n    Ouroboros.Consensus.Storage.LedgerDB.V1.Args\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Bridge\n    Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Status\n    Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog\n    Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq\n    Ouroboros.Consensus.Storage.LedgerDB.V1.Forker\n    Ouroboros.Consensus.Storage.LedgerDB.V1.Lock\n    Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots\n    Ouroboros.Consensus.Storage.LedgerDB.V2\n    Ouroboros.Consensus.Storage.LedgerDB.V2.Args\n    Ouroboros.Consensus.Storage.LedgerDB.V2.Forker\n    Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory\n    Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq\n    Ouroboros.Consensus.Storage.Serialisation\n    Ouroboros.Consensus.Storage.VolatileDB\n    Ouroboros.Consensus.Storage.VolatileDB.API\n    Ouroboros.Consensus.Storage.VolatileDB.Impl\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.Index\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.State\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.Types\n    Ouroboros.Consensus.Storage.VolatileDB.Impl.Util\n    Ouroboros.Consensus.Ticked\n    Ouroboros.Consensus.TypeFamilyWrappers\n    Ouroboros.Consensus.Util\n    Ouroboros.Consensus.Util.AnchoredFragment\n    Ouroboros.Consensus.Util.Args\n    Ouroboros.Consensus.Util.Assert\n    Ouroboros.Consensus.Util.CBOR\n    Ouroboros.Consensus.Util.CRC\n    Ouroboros.Consensus.Util.CallStack\n    Ouroboros.Consensus.Util.Condense\n    Ouroboros.Consensus.Util.DepPair\n    Ouroboros.Consensus.Util.EarlyExit\n    Ouroboros.Consensus.Util.Enclose\n    Ouroboros.Consensus.Util.FileLock\n    Ouroboros.Consensus.Util.HList\n    Ouroboros.Consensus.Util.IOLike\n    Ouroboros.Consensus.Util.IndexedMemPack\n    Ouroboros.Consensus.Util.LeakyBucket\n    Ouroboros.Consensus.Util.MonadSTM.NormalForm\n    Ouroboros.Consensus.Util.MonadSTM.StrictSVar\n    Ouroboros.Consensus.Util.NormalForm.Invariant\n    Ouroboros.Consensus.Util.NormalForm.StrictMVar\n    Ouroboros.Consensus.Util.NormalForm.StrictTVar\n    Ouroboros.Consensus.Util.Orphans\n    Ouroboros.Consensus.Util.RedundantConstraints\n    Ouroboros.Consensus.Util.STM\n    Ouroboros.Consensus.Util.StreamingLedgerTables\n    Ouroboros.Consensus.Util.Time\n    Ouroboros.Consensus.Util.Versioned\n\n  build-depends:\n    FailT ^>=0.1.2,\n    aeson,\n    base >=4.14 && <4.22,\n    base-deriving-via,\n    base16-bytestring,\n    bimap >=0.4 && <0.6,\n    binary >=0.8 && <0.11,\n    bytestring >=0.10 && <0.13,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-ledger-core ^>=1.18,\n    cardano-lmdb >=0.4,\n    cardano-lmdb-simple ^>=0.8,\n    cardano-prelude,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg ^>=0.2.2,\n    containers >=0.5 && <0.8,\n    contra-tracer,\n    deepseq,\n    diff-containers >=1.2,\n    filelock,\n    filepath,\n    fingertree-rm >=1.0,\n    fs-api ^>=0.4,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-mvar, strict-stm} ^>=1.8.0.1,\n    measures,\n    mempack,\n    monoid-subclasses,\n    mtl,\n    multiset ^>=0.3,\n    nothunks ^>=0.2,\n    ouroboros-network-api ^>=0.16,\n    ouroboros-network-mock ^>=0.1,\n    ouroboros-network-protocols ^>=0.15,\n    primitive,\n    psqueues ^>=0.2.3,\n    quiet ^>=0.2,\n    rawlock ^>=0.1.1,\n    resource-registry ^>=0.1,\n    semialign >=1.1,\n    serialise ^>=0.2,\n    singletons,\n    small-steps ^>=1.1,\n    sop-core ^>=0.5,\n    sop-extras ^>=0.4.1,\n    streaming,\n    strict >=0.1 && <0.6,\n    strict-checked-vars ^>=0.2,\n    strict-sop-core ^>=0.1,\n    text,\n    these ^>=1.2,\n    time,\n    transformers,\n    transformers-base,\n    typed-protocols ^>=1.0,\n    vector ^>=0.13,\n\n  x-docspec-extra-packages:\n    directory\n    latex-svg-image\n\n  build-depends: text >=1.2.5.0 && <2.2\n  x-docspec-extra-packages:\n    directory\n    latex-svg-image\n\nlibrary unstable-consensus-testlib\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-consensus-testlib\n  exposed-modules:\n    Test.LedgerTables\n    Test.Ouroboros.Consensus.ChainGenerator.Adversarial\n    Test.Ouroboros.Consensus.ChainGenerator.BitVector\n    Test.Ouroboros.Consensus.ChainGenerator.Counting\n    Test.Ouroboros.Consensus.ChainGenerator.Honest\n    Test.Ouroboros.Consensus.ChainGenerator.Params\n    Test.Ouroboros.Consensus.ChainGenerator.RaceIterator\n    Test.Ouroboros.Consensus.ChainGenerator.Slot\n    Test.Ouroboros.Consensus.ChainGenerator.Some\n    Test.Ouroboros.Consensus.DiffusionPipelining\n    Test.Ouroboros.Consensus.Protocol\n    Test.Ouroboros.Consensus.QuickCheck.Extras\n    Test.Ouroboros.Storage.TestBlock\n    Test.Util.BoolProps\n    Test.Util.ChainDB\n    Test.Util.ChainUpdates\n    Test.Util.ChunkInfo\n    Test.Util.Corruption\n    Test.Util.FileLock\n    Test.Util.HardFork.Future\n    Test.Util.HardFork.OracularClock\n    Test.Util.Header\n    Test.Util.InvertedMap\n    Test.Util.LedgerStateOnlyTables\n    Test.Util.LogicalClock\n    Test.Util.MockChain\n    Test.Util.Orphans.Arbitrary\n    Test.Util.Orphans.IOLike\n    Test.Util.Orphans.NoThunks\n    Test.Util.Orphans.Serialise\n    Test.Util.Orphans.SignableRepresentation\n    Test.Util.Orphans.ToExpr\n    Test.Util.Paths\n    Test.Util.QSM\n    Test.Util.QuickCheck\n    Test.Util.Range\n    Test.Util.RefEnv\n    Test.Util.SOP\n    Test.Util.SanityCheck\n    Test.Util.Schedule\n    Test.Util.Serialisation.CDDL\n    Test.Util.Serialisation.Examples\n    Test.Util.Serialisation.Golden\n    Test.Util.Serialisation.Roundtrip\n    Test.Util.Serialisation.SomeResult\n    Test.Util.Shrink\n    Test.Util.Slots\n    Test.Util.Split\n    Test.Util.Stream\n    Test.Util.SupportedNetworkProtocolVersion\n    Test.Util.TestBlock\n    Test.Util.TestEnv\n    Test.Util.Time\n    Test.Util.ToExpr\n    Test.Util.Tracer\n    Test.Util.WithEq\n\n  build-depends:\n    QuickCheck >=2.15,\n    base,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-binary:testlib,\n    cardano-crypto-class,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core,\n    cardano-prelude,\n    cardano-slotting,\n    cardano-slotting:testlib,\n    cardano-strict-containers,\n    cborg,\n    constraints,\n    containers,\n    contra-tracer,\n    deepseq,\n    directory,\n    file-embed,\n    filepath,\n    fs-api ^>=0.4,\n    fs-sim ^>=0.4,\n    generics-sop,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-mvar, strict-stm},\n    io-sim,\n    mempack,\n    mtl,\n    nothunks,\n    optparse-applicative,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    pretty-simple,\n    process,\n    quickcheck-instances,\n    quickcheck-state-machine:no-vendored-treediff ^>=0.10,\n    quiet,\n    random,\n    resource-registry,\n    serialise,\n    sop-core,\n    sop-extras,\n    strict-sop-core,\n    tasty,\n    tasty-golden,\n    tasty-hunit,\n    tasty-quickcheck >=0.11,\n    tasty-rerun,\n    template-haskell,\n    temporary,\n    text,\n    time,\n    transformers-base,\n    tree-diff,\n    utf8-string,\n    vector,\n    with-utf8,\n\nlibrary unstable-mock-block\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-mock-block\n  exposed-modules:\n    Ouroboros.Consensus.Mock.Ledger\n    Ouroboros.Consensus.Mock.Ledger.Address\n    Ouroboros.Consensus.Mock.Ledger.Block\n    Ouroboros.Consensus.Mock.Ledger.Block.BFT\n    Ouroboros.Consensus.Mock.Ledger.Block.PBFT\n    Ouroboros.Consensus.Mock.Ledger.Block.Praos\n    Ouroboros.Consensus.Mock.Ledger.Block.PraosRule\n    Ouroboros.Consensus.Mock.Ledger.Forge\n    Ouroboros.Consensus.Mock.Ledger.Stake\n    Ouroboros.Consensus.Mock.Ledger.State\n    Ouroboros.Consensus.Mock.Ledger.UTxO\n    Ouroboros.Consensus.Mock.Node\n    Ouroboros.Consensus.Mock.Node.Abstract\n    Ouroboros.Consensus.Mock.Node.BFT\n    Ouroboros.Consensus.Mock.Node.PBFT\n    Ouroboros.Consensus.Mock.Node.Praos\n    Ouroboros.Consensus.Mock.Node.PraosRule\n    Ouroboros.Consensus.Mock.Node.Serialisation\n    Ouroboros.Consensus.Mock.Protocol.LeaderSchedule\n    Ouroboros.Consensus.Mock.Protocol.Praos\n\n  build-depends:\n    base,\n    bimap,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-ledger-core,\n    cardano-slotting,\n    cborg,\n    containers,\n    deepseq,\n    hashable,\n    mempack,\n    mtl,\n    nothunks,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    serialise,\n    text,\n    time,\n    unstable-consensus-testlib,\n\nlibrary unstable-mempool-test-utils\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-mempool-test-utils\n  exposed-modules: Test.Consensus.Mempool.Mocked\n  build-depends:\n    base,\n    contra-tracer,\n    deepseq,\n    io-classes:strict-stm,\n    ouroboros-consensus,\n    resource-registry,\n\nlibrary unstable-tutorials\n  import: common-lib\n  visibility: public\n  hs-source-dirs: src/unstable-tutorials\n  other-modules:\n    Ouroboros.Consensus.Tutorial.Simple\n    Ouroboros.Consensus.Tutorial.WithEpoch\n\n  build-depends:\n    base,\n    cardano-ledger-core,\n    containers,\n    hashable,\n    mtl,\n    nothunks,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    serialise,\n\ntest-suite consensus-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/consensus-test\n  main-is: Main.hs\n  other-modules:\n    Test.Consensus.BlockchainTime.Simple\n    Test.Consensus.HardFork.Forecast\n    Test.Consensus.HardFork.History\n    Test.Consensus.HardFork.Infra\n    Test.Consensus.HardFork.Summary\n    Test.Consensus.HeaderValidation\n    Test.Consensus.Ledger.Tables.Diff\n    Test.Consensus.Ledger.Tables.DiffSeq\n    Test.Consensus.Mempool\n    Test.Consensus.Mempool.Fairness\n    Test.Consensus.Mempool.Fairness.TestBlock\n    Test.Consensus.Mempool.StateMachine\n    Test.Consensus.Mempool.Util\n    Test.Consensus.MiniProtocol.BlockFetch.Client\n    Test.Consensus.MiniProtocol.ChainSync.CSJ\n    Test.Consensus.MiniProtocol.ChainSync.Client\n    Test.Consensus.MiniProtocol.LocalStateQuery.Server\n    Test.Consensus.Peras.WeightSnapshot\n    Test.Consensus.Util.MonadSTM.NormalForm\n    Test.Consensus.Util.Versioned\n\n  build-depends:\n    QuickCheck,\n    async,\n    base,\n    base-deriving-via,\n    cardano-binary,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-tests ^>=2.2,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    cborg,\n    containers,\n    contra-tracer,\n    deepseq,\n    diff-containers,\n    fingertree-rm,\n    fs-api ^>=0.4,\n    fs-sim,\n    hashable,\n    io-classes:{io-classes, si-timers, strict-mvar, strict-stm},\n    io-sim,\n    measures,\n    mtl,\n    nonempty-containers,\n    nothunks,\n    ouroboros-consensus,\n    ouroboros-network,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    ouroboros-network-protocols:{ouroboros-network-protocols, testlib},\n    quickcheck-classes,\n    quickcheck-monoid-subclasses,\n    quickcheck-state-machine:no-vendored-treediff,\n    quiet,\n    random,\n    resource-registry,\n    serialise,\n    sop-core,\n    sop-extras,\n    strict-sop-core,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    time,\n    transformers,\n    transformers-base,\n    tree-diff,\n    typed-protocols:{examples, stateful, typed-protocols} ^>=1,\n    unstable-consensus-testlib,\n    unstable-mock-block,\n\ntest-suite infra-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/infra-test\n  main-is: Main.hs\n  other-modules:\n    Ouroboros.Consensus.Util.Tests\n    Test.Ouroboros.Consensus.ChainGenerator.Tests\n    Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial\n    Test.Ouroboros.Consensus.ChainGenerator.Tests.BitVector\n    Test.Ouroboros.Consensus.ChainGenerator.Tests.Counting\n    Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest\n    Test.Ouroboros.Consensus.Util.LeakyBucket.Tests\n    Test.Util.ChainUpdates.Tests\n    Test.Util.Schedule.Tests\n    Test.Util.Split.Tests\n\n  build-depends:\n    QuickCheck,\n    base,\n    cardano-ledger-core,\n    io-classes,\n    io-sim,\n    mtl,\n    ouroboros-consensus:{ouroboros-consensus, unstable-consensus-testlib},\n    random,\n    tasty,\n    tasty-quickcheck,\n    time,\n    unstable-consensus-testlib,\n    vector,\n\ntest-suite storage-test\n  import: common-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test/storage-test\n  main-is: Main.hs\n  other-modules:\n    Test.Ouroboros.Storage\n    Test.Ouroboros.Storage.ChainDB\n    Test.Ouroboros.Storage.ChainDB.FollowerPromptness\n    Test.Ouroboros.Storage.ChainDB.GcSchedule\n    Test.Ouroboros.Storage.ChainDB.Iterator\n    Test.Ouroboros.Storage.ChainDB.Model\n    Test.Ouroboros.Storage.ChainDB.Model.Test\n    Test.Ouroboros.Storage.ChainDB.Paths\n    Test.Ouroboros.Storage.ChainDB.StateMachine\n    Test.Ouroboros.Storage.ChainDB.StateMachine.Utils.RunOnRepl\n    Test.Ouroboros.Storage.ChainDB.Unit\n    Test.Ouroboros.Storage.ImmutableDB\n    Test.Ouroboros.Storage.ImmutableDB.Mock\n    Test.Ouroboros.Storage.ImmutableDB.Model\n    Test.Ouroboros.Storage.ImmutableDB.Primary\n    Test.Ouroboros.Storage.ImmutableDB.StateMachine\n    Test.Ouroboros.Storage.LedgerDB\n    Test.Ouroboros.Storage.LedgerDB.Serialisation\n    Test.Ouroboros.Storage.LedgerDB.SnapshotPolicy\n    Test.Ouroboros.Storage.LedgerDB.Snapshots\n    Test.Ouroboros.Storage.LedgerDB.StateMachine\n    Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock\n    Test.Ouroboros.Storage.LedgerDB.V1.BackingStore\n    Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Lockstep\n    Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock\n    Test.Ouroboros.Storage.LedgerDB.V1.DbChangelog\n    Test.Ouroboros.Storage.LedgerDB.V1.LMDB\n    Test.Ouroboros.Storage.Orphans\n    Test.Ouroboros.Storage.VolatileDB\n    Test.Ouroboros.Storage.VolatileDB.Mock\n    Test.Ouroboros.Storage.VolatileDB.Model\n    Test.Ouroboros.Storage.VolatileDB.StateMachine\n\n  build-depends:\n    QuickCheck,\n    aeson,\n    base,\n    bifunctors,\n    bytestring,\n    cardano-binary,\n    cardano-ledger-binary:testlib,\n    cardano-ledger-core:{cardano-ledger-core, testlib},\n    cardano-slotting:{cardano-slotting, testlib},\n    cardano-strict-containers,\n    cborg,\n    constraints,\n    containers,\n    contra-tracer,\n    diff-containers,\n    directory,\n    filepath,\n    fs-api ^>=0.4,\n    fs-sim ^>=0.4,\n    generics-sop,\n    io-classes:{io-classes, strict-mvar, strict-stm},\n    io-sim,\n    mempack,\n    mtl,\n    nothunks,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    pretty-show,\n    quickcheck-dynamic,\n    quickcheck-lockstep ^>=0.8,\n    quickcheck-state-machine:no-vendored-treediff ^>=0.10,\n    random,\n    resource-registry,\n    serialise,\n    sop-core,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    temporary,\n    text,\n    time,\n    transformers,\n    tree-diff,\n    unstable-consensus-testlib,\n    vector,\n\nbenchmark mempool-bench\n  import: common-bench\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench/mempool-bench\n  main-is: Main.hs\n  other-modules:\n    Bench.Consensus.Mempool\n    Bench.Consensus.Mempool.TestBlock\n\n  build-depends:\n    aeson,\n    base,\n    bytestring,\n    cardano-ledger-core,\n    cardano-slotting,\n    cassava,\n    containers,\n    contra-tracer,\n    deepseq,\n    mempack,\n    nothunks,\n    ouroboros-consensus,\n    serialise,\n    tasty,\n    tasty-bench,\n    tasty-hunit,\n    text,\n    transformers,\n    tree-diff,\n    unstable-consensus-testlib,\n    unstable-mempool-test-utils,\n    with-utf8,\n\nbenchmark ChainSync-client-bench\n  import: common-bench\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench/ChainSync-client-bench\n  main-is: Main.hs\n  other-modules: Bench.Consensus.ChainSyncClient.Driver\n  build-depends:\n    array,\n    base,\n    cardano-crypto-class,\n    cardano-ledger-core,\n    containers,\n    contra-tracer,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    ouroboros-network-mock,\n    ouroboros-network-protocols,\n    resource-registry,\n    time,\n    typed-protocols:examples,\n    unstable-consensus-testlib,\n    with-utf8,\n\nbenchmark PerasCertDB-bench\n  import: common-bench\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench/PerasCertDB-bench\n  main-is: Main.hs\n  other-modules:\n  build-depends:\n    base,\n    ouroboros-consensus,\n    ouroboros-network-api,\n    tasty-bench,\n    unstable-consensus-testlib,\n\ntest-suite doctest\n  import: common-test\n  main-is: doctest.hs\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-language: Haskell2010\n  ghc-options: -Wno-unused-packages\n  build-depends:\n    base,\n    latex-svg-image,\n";
  }