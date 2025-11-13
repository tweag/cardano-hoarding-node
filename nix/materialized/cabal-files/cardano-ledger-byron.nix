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
    flags = { test-normal-form = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-ledger-byron"; version = "1.2.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano during the Byron era";
      description = "The blockchain layer of Cardano during the Byron era";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."heapwords" or (errorHandler.buildDepError "heapwords"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."streaming-binary" or (errorHandler.buildDepError "streaming-binary"))
          (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-crypto-wrapper".components.sublibs.testlib or (errorHandler.buildDepError "cardano-crypto-wrapper:testlib"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."heapwords" or (errorHandler.buildDepError "heapwords"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps".components.sublibs.testlib or (errorHandler.buildDepError "small-steps:testlib"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
          ];
          buildable = true;
        };
        "epoch-validation-normal-form-test" = {
          depends = [
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."cardano-ledger-byron".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-byron:testlib"))
          ];
          buildable = if !flags.test-normal-form then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-byron-1.2.0.0.tar.gz";
      sha256 = "313b9f660ebe6ccd9b64a5dcd96a24057fcb05a4d33a0f2ee31f8b639b3c0398";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-byron\nversion: 1.2.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nsynopsis: The blockchain layer of Cardano during the Byron era\ndescription: The blockchain layer of Cardano during the Byron era\ncategory: Currency\nbuild-type: Simple\ndata-files:\n  cddl-spec/byron.cddl\n  golden/cbor/block/BlockSignature\n  golden/cbor/block/Body\n  golden/cbor/block/BoundaryBlockHeader\n  golden/cbor/block/BoundaryBody\n  golden/cbor/block/BoundaryConsensusData\n  golden/cbor/block/BoundaryProof\n  golden/cbor/block/ExtraBodyData\n  golden/cbor/block/Header\n  golden/cbor/block/HeaderHash\n  golden/cbor/block/Proof\n  golden/cbor/block/StaticConfig_GCSpec\n  golden/cbor/block/ToSign\n  golden/cbor/common/AddrSpendingData_Redeem\n  golden/cbor/common/AddrSpendingData_VerKey\n  golden/cbor/common/AddrType_R\n  golden/cbor/common/AddrType_VK\n  golden/cbor/common/Address\n  golden/cbor/common/Address0\n  golden/cbor/common/Address1\n  golden/cbor/common/Address2\n  golden/cbor/common/Address3\n  golden/cbor/common/Address4\n  golden/cbor/common/Attributes\n  golden/cbor/common/BlockCount\n  golden/cbor/common/ChainDifficulty\n  golden/cbor/common/KeyHash\n  golden/cbor/common/Lovelace\n  golden/cbor/common/LovelacePortion\n  golden/cbor/common/MerkleRoot\n  golden/cbor/common/MerkleTree\n  golden/cbor/common/TxFeePolicy_Linear\n  golden/cbor/common/TxSizeLinear\n  golden/cbor/delegation/Certificate\n  golden/cbor/delegation/DlgPayload\n  golden/cbor/mempoolpayload/MempoolPayload\n  golden/cbor/mempoolpayload/MempoolPayload1\n  golden/cbor/mempoolpayload/MempoolPayload2\n  golden/cbor/mempoolpayload/MempoolPayload3\n  golden/cbor/slotting/EpochAndSlotCount\n  golden/cbor/slotting/EpochNumber\n  golden/cbor/slotting/EpochSlots\n  golden/cbor/slotting/SlotNumber\n  golden/cbor/ssc/Commitment\n  golden/cbor/ssc/CommitmentsMap\n  golden/cbor/ssc/InnerSharesMap\n  golden/cbor/ssc/Opening\n  golden/cbor/ssc/OpeningsMap\n  golden/cbor/ssc/SharesMap\n  golden/cbor/ssc/SignedCommitment\n  golden/cbor/ssc/SscPayload_CertificatesPayload\n  golden/cbor/ssc/SscPayload_CommitmentsPayload\n  golden/cbor/ssc/SscPayload_OpeningsPayload\n  golden/cbor/ssc/SscPayload_SharesPayload\n  golden/cbor/ssc/SscProof_CertificatesProof\n  golden/cbor/ssc/SscProof_CommitmentsProof\n  golden/cbor/ssc/SscProof_OpeningsProof\n  golden/cbor/ssc/SscProof_SharesProof\n  golden/cbor/ssc/VssCertificate\n  golden/cbor/ssc/VssCertificatesHash\n  golden/cbor/ssc/VssCertificatesMap\n  golden/cbor/update/ApplicationName\n  golden/cbor/update/AttackTarget_NetworkAddressTarget\n  golden/cbor/update/AttackTarget_PubKeyAddressTarget\n  golden/cbor/update/BlockHeader_Boundary\n  golden/cbor/update/CommitmentSignature\n  golden/cbor/update/HashRaw\n  golden/cbor/update/HashTx\n  golden/cbor/update/InstallerHash\n  golden/cbor/update/Payload\n  golden/cbor/update/Proof\n  golden/cbor/update/Proposal\n  golden/cbor/update/ProposalBody\n  golden/cbor/update/ProtocolParameters\n  golden/cbor/update/ProtocolParametersUpdate\n  golden/cbor/update/ProtocolVersion\n  golden/cbor/update/SharesDistribution\n  golden/cbor/update/SoftforkRule\n  golden/cbor/update/SoftwareVersion\n  golden/cbor/update/StaticConfig_GCSpec\n  golden/cbor/update/StaticConfig_GCSrc\n  golden/cbor/update/SystemTag\n  golden/cbor/update/UpId\n  golden/cbor/update/Vote\n  golden/cbor/utxo/HashTx\n  golden/cbor/utxo/Tx\n  golden/cbor/utxo/TxAttributes\n  golden/cbor/utxo/TxId\n  golden/cbor/utxo/TxInList\n  golden/cbor/utxo/TxInWitness_RedeemWitness\n  golden/cbor/utxo/TxInWitness_VKWitness\n  golden/cbor/utxo/TxIn_Utxo\n  golden/cbor/utxo/TxOut\n  golden/cbor/utxo/TxOut1\n  golden/cbor/utxo/TxOutList\n  golden/cbor/utxo/TxOutList1\n  golden/cbor/utxo/TxPayload1\n  golden/cbor/utxo/TxProof\n  golden/cbor/utxo/TxSig\n  golden/cbor/utxo/TxSigData\n  golden/cbor/utxo/TxWitness\n  golden/json/genesis/GenesisData0_Legacy_HasNetworkMagic\n  test/mainnet-genesis.json\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nflag test-normal-form\n  description: Test ledger state normal form during epoch validation\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Cardano.Chain.Block\n    Cardano.Chain.Byron.API\n    Cardano.Chain.Common\n    Cardano.Chain.Constants\n    Cardano.Chain.Delegation\n    Cardano.Chain.Delegation.Validation.Activation\n    Cardano.Chain.Delegation.Validation.Interface\n    Cardano.Chain.Delegation.Validation.Scheduling\n    Cardano.Chain.Epoch.File\n    Cardano.Chain.Epoch.Validation\n    Cardano.Chain.Genesis\n    Cardano.Chain.MempoolPayload\n    Cardano.Chain.ProtocolConstants\n    Cardano.Chain.Slotting\n    Cardano.Chain.Ssc\n    Cardano.Chain.UTxO\n    Cardano.Chain.UTxO.UTxO\n    Cardano.Chain.UTxO.Validation\n    Cardano.Chain.Update\n    Cardano.Chain.Update.Proposal\n    Cardano.Chain.Update.Validation.Endorsement\n    Cardano.Chain.Update.Validation.Interface\n    Cardano.Chain.Update.Validation.Registration\n    Cardano.Chain.Update.Validation.Voting\n    Cardano.Chain.Update.Vote\n    Cardano.Chain.ValidationMode\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Chain.Block.Block\n    Cardano.Chain.Block.Body\n    Cardano.Chain.Block.Boundary\n    Cardano.Chain.Block.Header\n    Cardano.Chain.Block.Proof\n    Cardano.Chain.Block.Validation\n    Cardano.Chain.Block.ValidationMode\n    Cardano.Chain.Byron.API.Common\n    Cardano.Chain.Byron.API.Mempool\n    Cardano.Chain.Byron.API.Protocol\n    Cardano.Chain.Byron.API.Validation\n    Cardano.Chain.Common.AddrAttributes\n    Cardano.Chain.Common.AddrSpendingData\n    Cardano.Chain.Common.Address\n    Cardano.Chain.Common.AddressHash\n    Cardano.Chain.Common.Attributes\n    Cardano.Chain.Common.BlockCount\n    Cardano.Chain.Common.CBOR\n    Cardano.Chain.Common.ChainDifficulty\n    Cardano.Chain.Common.Compact\n    Cardano.Chain.Common.KeyHash\n    Cardano.Chain.Common.Lovelace\n    Cardano.Chain.Common.LovelacePortion\n    Cardano.Chain.Common.Merkle\n    Cardano.Chain.Common.NetworkMagic\n    Cardano.Chain.Common.TxFeePolicy\n    Cardano.Chain.Common.TxSizeLinear\n    Cardano.Chain.Delegation.Certificate\n    Cardano.Chain.Delegation.Map\n    Cardano.Chain.Delegation.Payload\n    Cardano.Chain.Genesis.AvvmBalances\n    Cardano.Chain.Genesis.Config\n    Cardano.Chain.Genesis.Data\n    Cardano.Chain.Genesis.Delegation\n    Cardano.Chain.Genesis.Generate\n    Cardano.Chain.Genesis.Hash\n    Cardano.Chain.Genesis.Initializer\n    Cardano.Chain.Genesis.KeyHashes\n    Cardano.Chain.Genesis.NonAvvmBalances\n    Cardano.Chain.Genesis.Spec\n    Cardano.Chain.Slotting.EpochAndSlotCount\n    Cardano.Chain.Slotting.EpochNumber\n    Cardano.Chain.Slotting.EpochSlots\n    Cardano.Chain.Slotting.SlotCount\n    Cardano.Chain.Slotting.SlotNumber\n    Cardano.Chain.UTxO.Compact\n    Cardano.Chain.UTxO.GenesisUTxO\n    Cardano.Chain.UTxO.Tx\n    Cardano.Chain.UTxO.TxAux\n    Cardano.Chain.UTxO.TxPayload\n    Cardano.Chain.UTxO.TxProof\n    Cardano.Chain.UTxO.TxWitness\n    Cardano.Chain.UTxO.UTxOConfiguration\n    Cardano.Chain.UTxO.ValidationMode\n    Cardano.Chain.Update.ApplicationName\n    Cardano.Chain.Update.InstallerHash\n    Cardano.Chain.Update.Payload\n    Cardano.Chain.Update.Proof\n    Cardano.Chain.Update.ProtocolParameters\n    Cardano.Chain.Update.ProtocolParametersUpdate\n    Cardano.Chain.Update.ProtocolVersion\n    Cardano.Chain.Update.SoftforkRule\n    Cardano.Chain.Update.SoftwareVersion\n    Cardano.Chain.Update.SystemTag\n    Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n\n  build-depends:\n    aeson,\n    base >=4.18 && <5,\n    base58-bytestring,\n    bimap >=0.4 && <0.6,\n    binary,\n    bytestring,\n    canonical-json,\n    cardano-crypto,\n    cardano-crypto-wrapper >=1.6,\n    cardano-ledger-binary >=1.5,\n    cardano-prelude,\n    cborg,\n    containers,\n    contra-tracer,\n    crypton,\n    digest,\n    directory,\n    filepath,\n    formatting,\n    heapwords,\n    nothunks,\n    quiet,\n    resourcet,\n    streaming,\n    streaming-binary >=0.2 && <0.4,\n    streaming-bytestring,\n    text,\n    time,\n    vector,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Chain.Binary.Cddl\n    Test.Cardano.Chain.Block.CBOR\n    Test.Cardano.Chain.Block.Gen\n    Test.Cardano.Chain.Block.Model\n    Test.Cardano.Chain.Block.Model.Examples\n    Test.Cardano.Chain.Block.Size\n    Test.Cardano.Chain.Block.Validation\n    Test.Cardano.Chain.Block.ValidationMode\n    Test.Cardano.Chain.Buildable\n    Test.Cardano.Chain.Byron.API\n    Test.Cardano.Chain.Common.Address\n    Test.Cardano.Chain.Common.Attributes\n    Test.Cardano.Chain.Common.CBOR\n    Test.Cardano.Chain.Common.Compact\n    Test.Cardano.Chain.Common.Example\n    Test.Cardano.Chain.Common.Gen\n    Test.Cardano.Chain.Common.Lovelace\n    Test.Cardano.Chain.Config\n    Test.Cardano.Chain.Delegation.CBOR\n    Test.Cardano.Chain.Delegation.Certificate\n    Test.Cardano.Chain.Delegation.Example\n    Test.Cardano.Chain.Delegation.Gen\n    Test.Cardano.Chain.Delegation.Model\n    Test.Cardano.Chain.Elaboration.Block\n    Test.Cardano.Chain.Elaboration.Delegation\n    Test.Cardano.Chain.Elaboration.Keys\n    Test.Cardano.Chain.Elaboration.UTxO\n    Test.Cardano.Chain.Elaboration.Update\n    Test.Cardano.Chain.Epoch.File\n    Test.Cardano.Chain.Genesis.CBOR\n    Test.Cardano.Chain.Genesis.Dummy\n    Test.Cardano.Chain.Genesis.Example\n    Test.Cardano.Chain.Genesis.Gen\n    Test.Cardano.Chain.Genesis.Json\n    Test.Cardano.Chain.MempoolPayload.CBOR\n    Test.Cardano.Chain.MempoolPayload.Example\n    Test.Cardano.Chain.MempoolPayload.Gen\n    Test.Cardano.Chain.Slotting.CBOR\n    Test.Cardano.Chain.Slotting.Example\n    Test.Cardano.Chain.Slotting.Gen\n    Test.Cardano.Chain.Slotting.Properties\n    Test.Cardano.Chain.Ssc.CBOR\n    Test.Cardano.Chain.UTxO.CBOR\n    Test.Cardano.Chain.UTxO.Compact\n    Test.Cardano.Chain.UTxO.Example\n    Test.Cardano.Chain.UTxO.Gen\n    Test.Cardano.Chain.UTxO.Model\n    Test.Cardano.Chain.UTxO.ValidationMode\n    Test.Cardano.Chain.Update.CBOR\n    Test.Cardano.Chain.Update.Example\n    Test.Cardano.Chain.Update.Gen\n    Test.Cardano.Chain.Update.Properties\n    Test.Cardano.Mirror\n    Test.Options\n\n  visibility: public\n  hs-source-dirs: testlib\n  other-modules:\n    Paths_cardano_ledger_byron\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n\n  build-depends:\n    base,\n    base16-bytestring >=1,\n    bimap,\n    byron-spec-chain,\n    byron-spec-ledger,\n    bytestring,\n    cardano-crypto,\n    cardano-crypto-wrapper:{cardano-crypto-wrapper, testlib},\n    cardano-ledger-binary:{cardano-ledger-binary, testlib},\n    cardano-ledger-byron,\n    cardano-prelude,\n    cardano-prelude-test,\n    containers,\n    directory,\n    filepath,\n    formatting,\n    heapwords,\n    hedgehog >=1.0.4,\n    microlens,\n    resourcet,\n    small-steps:{small-steps, testlib},\n    streaming,\n    tasty,\n    tasty-hedgehog,\n    text,\n    time,\n    vector,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: test.hs\n  hs-source-dirs: test\n  other-modules:\n    Paths_cardano_ledger_byron\n\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n    -rtsopts\n    \"-with-rtsopts=-K450K -M500M\"\n\n  build-depends:\n    base,\n    cardano-prelude,\n    tasty,\n    testlib,\n\ntest-suite epoch-validation-normal-form-test\n  type: exitcode-stdio-1.0\n  main-is: NormalFormTest.hs\n  hs-source-dirs: test\n  default-language: Haskell2010\n  default-extensions: NoImplicitPrelude\n  ghc-options:\n    -Wall\n    -Wno-all-missed-specialisations\n    -Wno-missing-deriving-strategies\n    -Wno-missing-import-lists\n    -Wno-missing-safe-haskell-mode\n    -Wno-prepositive-qualified-module\n    -Wno-safe\n    -Wno-unsafe\n    -Wunused-packages\n    -rtsopts\n    \"-with-rtsopts=-K450K -M500M\"\n\n  build-depends:\n    cardano-prelude,\n    silently,\n    testlib,\n\n  if !flag(test-normal-form)\n    buildable: False\n";
  }