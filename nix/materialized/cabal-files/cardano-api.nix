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
      specVersion = "3.8";
      identifier = { name = "cardano-api"; version = "10.19.1.0"; };
      license = "Apache-2.0";
      copyright = "2020-2024 Input Output Global Inc (IOG).";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The cardano API";
      description = "The cardano API.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-trie" or (errorHandler.buildDepError "bytestring-trie"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
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
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ordered-containers" or (errorHandler.buildDepError "ordered-containers"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-diffusion" or (errorHandler.buildDepError "ouroboros-consensus-diffusion"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-network-protocols" or (errorHandler.buildDepError "ouroboros-network-protocols"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."sop-extras" or (errorHandler.buildDepError "sop-extras"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
        ] ++ pkgs.lib.optional (!(system.isWindows || system.isWasm32)) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ [
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      sublibs = {
        "gen" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-dijkstra" or (errorHandler.buildDepError "cardano-ledger-dijkstra"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      tests = {
        "cardano-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-conway" or (errorHandler.buildDepError "cardano-ledger-conway"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-mary" or (errorHandler.buildDepError "cardano-ledger-mary"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = if system.isWasm32 then false else true;
        };
        "cardano-api-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-api" or (errorHandler.buildDepError "cardano-ledger-api"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = if system.isWasm32 then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-api-10.19.1.0.tar.gz";
      sha256 = "f328cb82c29c8fb01c8e3166c86b751ace4e55e1d6054da626c0f102747d7d1b";
    });
  }) // {
    package-description-override = "cabal-version: 3.8\nname: cardano-api\nversion: 10.19.1.0\nsynopsis: The cardano API\ndescription: The cardano API.\ncategory:\n  Cardano,\n  API,\n\ncopyright: 2020-2024 Input Output Global Inc (IOG).\nauthor: IOHK\nmaintainer: operations@iohk.io\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ncommon project-config\n  default-language: Haskell2010\n  default-extensions:\n    ImportQualifiedPost\n    OverloadedStrings\n\n  build-depends: base >=4.14 && <4.22\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wno-unticked-promoted-constructors\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\ncommon maybe-unix\n  if !(os(windows)|| arch(wasm32))\n    build-depends: unix\n\ncommon maybe-Win32\n  if os(windows)\n    build-depends: Win32\n\ncommon text\n  if os(osx)&& arch(aarch64)\n    build-depends: text >=1.2.5.0\n  else\n    build-depends: text >=2.0\n\nlibrary\n  import: project-config, maybe-unix, maybe-Win32, text\n  hs-source-dirs:\n    src\n\n  -- Do not expose any additional modules. The correct way\n  -- to expose new functionality is via Cardano.Api\n  exposed-modules:\n    Cardano.Api\n    Cardano.Api.Address\n    Cardano.Api.Block\n    Cardano.Api.Byron\n    Cardano.Api.Certificate\n    Cardano.Api.Compatible\n    Cardano.Api.Compatible.Tx\n    Cardano.Api.Consensus\n    Cardano.Api.Crypto.Ed25519Bip32\n    Cardano.Api.Era\n    Cardano.Api.Error\n    Cardano.Api.Experimental\n    Cardano.Api.Experimental.Era\n    Cardano.Api.Experimental.Plutus\n    Cardano.Api.Experimental.Simple.Script\n    Cardano.Api.Experimental.Tx\n    Cardano.Api.Genesis\n    Cardano.Api.Governance\n    Cardano.Api.HasTypeProxy\n    Cardano.Api.Hash\n    Cardano.Api.IO\n    Cardano.Api.Key\n    Cardano.Api.Ledger\n    Cardano.Api.LedgerState\n    Cardano.Api.Monad.Error\n    Cardano.Api.Network\n    Cardano.Api.Network.IPC\n    Cardano.Api.Parser.Text\n    Cardano.Api.Plutus\n    Cardano.Api.Pretty\n    Cardano.Api.ProtocolParameters\n    Cardano.Api.Query\n    Cardano.Api.Serialise.Bech32\n    Cardano.Api.Serialise.Cbor\n    Cardano.Api.Serialise.Cbor.Canonical\n    Cardano.Api.Serialise.Cip129\n    Cardano.Api.Serialise.DeserialiseAnyOf\n    Cardano.Api.Serialise.Json\n    Cardano.Api.Serialise.Raw\n    Cardano.Api.Serialise.SerialiseUsing\n    Cardano.Api.Serialise.TextEnvelope\n    Cardano.Api.Trace.Debug\n    Cardano.Api.Tx\n    Cardano.Api.UTxO\n    Cardano.Api.Value\n\n  build-depends:\n    FailT,\n    aeson >=1.5.6.0,\n    aeson-pretty >=0.8.5,\n    attoparsec,\n    barbies,\n    base16-bytestring >=1.0,\n    base58-bytestring,\n    base64-bytestring,\n    basement,\n    bech32 >=1.1.0,\n    bytestring,\n    bytestring-trie,\n    cardano-addresses ^>=4.0.0,\n    cardano-binary,\n    cardano-crypto,\n    cardano-crypto-class ^>=2.2.1,\n    cardano-crypto-wrapper ^>=1.6,\n    cardano-data >=1.0,\n    cardano-ledger-allegra >=1.7,\n    cardano-ledger-alonzo >=1.13,\n    cardano-ledger-api ^>=1.12.1,\n    cardano-ledger-babbage >=1.11,\n    cardano-ledger-binary >=1.6,\n    cardano-ledger-byron >=1.2,\n    cardano-ledger-conway >=1.19,\n    cardano-ledger-core >=1.17 && <1.19,\n    cardano-ledger-dijkstra >=0.1,\n    cardano-ledger-mary >=1.8,\n    cardano-ledger-shelley >=1.16,\n    cardano-protocol-tpraos >=1.4,\n    cardano-slotting >=0.2.0.0,\n    cardano-strict-containers >=0.1,\n    cborg,\n    containers,\n    contra-tracer,\n    crypton,\n    data-default-class,\n    deepseq,\n    directory,\n    either,\n    errors,\n    extra,\n    filepath,\n    formatting,\n    groups,\n    iproute,\n    memory,\n    microlens,\n    mono-traversable,\n    mtl,\n    network,\n    network-mux,\n    nothunks,\n    ordered-containers,\n    ouroboros-consensus ^>=0.28,\n    ouroboros-consensus-cardano ^>=0.26,\n    ouroboros-consensus-diffusion ^>=0.24,\n    ouroboros-consensus-protocol ^>=0.13,\n    ouroboros-network,\n    ouroboros-network-api >=0.15,\n    ouroboros-network-framework,\n    ouroboros-network-protocols >=0.15,\n    parsec,\n    plutus-core ^>=1.53,\n    plutus-ledger-api ^>=1.53,\n    pretty-simple,\n    prettyprinter,\n    prettyprinter-ansi-terminal,\n    prettyprinter-configurable ^>=1.36,\n    random,\n    safe-exceptions,\n    scientific,\n    serialise,\n    singletons,\n    small-steps ^>=1.1,\n    sop-core,\n    sop-extras,\n    stm,\n    strict-sop-core,\n    time,\n    transformers,\n    transformers-except ^>=0.1.3,\n    typed-protocols ^>=1.0,\n    validation,\n    vector,\n    yaml,\n\n  other-modules:\n    Cardano.Api.Byron.Internal.Key\n    Cardano.Api.Byron.Internal.Proposal\n    Cardano.Api.Certificate.Internal\n    Cardano.Api.Certificate.Internal.DRepMetadata\n    Cardano.Api.Certificate.Internal.OperationalCertificate\n    Cardano.Api.Certificate.Internal.StakePoolMetadata\n    Cardano.Api.Consensus.Internal.InMode\n    Cardano.Api.Consensus.Internal.Mode\n    Cardano.Api.Consensus.Internal.Protocol\n    Cardano.Api.Consensus.Internal.Reexport\n    Cardano.Api.Era.Internal.Case\n    Cardano.Api.Era.Internal.Core\n    Cardano.Api.Era.Internal.Eon.AllegraEraOnwards\n    Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards\n    Cardano.Api.Era.Internal.Eon.BabbageEraOnwards\n    Cardano.Api.Era.Internal.Eon.ByronToAlonzoEra\n    Cardano.Api.Era.Internal.Eon.Convert\n    Cardano.Api.Era.Internal.Eon.ConwayEraOnwards\n    Cardano.Api.Era.Internal.Eon.MaryEraOnwards\n    Cardano.Api.Era.Internal.Eon.ShelleyBasedEra\n    Cardano.Api.Era.Internal.Eon.ShelleyEraOnly\n    Cardano.Api.Era.Internal.Eon.ShelleyToAllegraEra\n    Cardano.Api.Era.Internal.Eon.ShelleyToAlonzoEra\n    Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra\n    Cardano.Api.Era.Internal.Eon.ShelleyToMaryEra\n    Cardano.Api.Era.Internal.Feature\n    Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness\n    Cardano.Api.Experimental.Plutus.Internal.Script\n    Cardano.Api.Experimental.Plutus.Internal.ScriptWitness\n    Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts\n    Cardano.Api.Experimental.Tx.Internal.AnyWitness\n    Cardano.Api.Experimental.Tx.Internal.Body\n    Cardano.Api.Experimental.Tx.Internal.Certificate\n    Cardano.Api.Experimental.Tx.Internal.Fee\n    Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements\n    Cardano.Api.Genesis.Internal\n    Cardano.Api.Genesis.Internal.Parameters\n    Cardano.Api.Governance.Internal.Action.ProposalProcedure\n    Cardano.Api.Governance.Internal.Action.VotingProcedure\n    Cardano.Api.Governance.Internal.Metadata.Anchor\n    Cardano.Api.Governance.Internal.Metadata.DrepRegistration\n    Cardano.Api.Governance.Internal.Metadata.GovAction\n    Cardano.Api.Governance.Internal.Metadata.Validation\n    Cardano.Api.Governance.Internal.Poll\n    Cardano.Api.IO.Internal.Base\n    Cardano.Api.IO.Internal.Compat\n    Cardano.Api.IO.Internal.Compat.Posix\n    Cardano.Api.IO.Internal.Compat.Wasm\n    Cardano.Api.IO.Internal.Compat.Win32\n    Cardano.Api.Internal.Orphans\n    Cardano.Api.Internal.Orphans.Misc\n    Cardano.Api.Internal.Orphans.Serialisation\n    Cardano.Api.Internal.Utils\n    Cardano.Api.Key.Internal\n    Cardano.Api.Key.Internal.Class\n    Cardano.Api.Key.Internal.Mnemonic\n    Cardano.Api.Key.Internal.Praos\n    Cardano.Api.Key.Internal.SomeAddressVerificationKey\n    Cardano.Api.Ledger.Internal.Reexport\n    Cardano.Api.LedgerState.Internal.ConvertLedgerEvent\n    Cardano.Api.LedgerState.Internal.LedgerEvent\n    Cardano.Api.LedgerState.Internal.Rule.BBODY.DELEGS\n    Cardano.Api.LedgerState.Internal.Rule.BBODY.LEDGER\n    Cardano.Api.LedgerState.Internal.Rule.BBODY.UTXOW\n    Cardano.Api.LedgerState.Internal.Rule.TICK.NEWEPOCH\n    Cardano.Api.LedgerState.Internal.Rule.TICK.RUPD\n    Cardano.Api.Network.IPC.Internal\n    Cardano.Api.Network.IPC.Internal.ChainSync.Client\n    Cardano.Api.Network.IPC.Internal.ChainSync.ClientPipelined\n    Cardano.Api.Network.IPC.Internal.Monad\n    Cardano.Api.Network.IPC.Internal.Version\n    Cardano.Api.Network.Internal.NetworkId\n    Cardano.Api.Network.Internal.Reexport\n    Cardano.Api.Plutus.Internal\n    Cardano.Api.Plutus.Internal.Script\n    Cardano.Api.Plutus.Internal.ScriptData\n    Cardano.Api.Pretty.Internal.ShowOf\n    Cardano.Api.Query.Internal.Convenience\n    Cardano.Api.Query.Internal.Expr\n    Cardano.Api.Query.Internal.Type.DebugLedgerState\n    Cardano.Api.Query.Internal.Type.DelegationsAndRewards\n    Cardano.Api.Query.Internal.Type.QueryInMode\n    Cardano.Api.Serialise.TextEnvelope.Internal\n    Cardano.Api.Serialise.TextEnvelope.Internal.Cddl\n    Cardano.Api.Tx.Internal.Body\n    Cardano.Api.Tx.Internal.Body.Lens\n    Cardano.Api.Tx.Internal.BuildTxWith\n    Cardano.Api.Tx.Internal.Convenience\n    Cardano.Api.Tx.Internal.Fee\n    Cardano.Api.Tx.Internal.Output\n    Cardano.Api.Tx.Internal.Serialise\n    Cardano.Api.Tx.Internal.Sign\n    Cardano.Api.Tx.Internal.TxIn\n    Cardano.Api.Tx.Internal.TxMetadata\n    Cardano.Api.Value.Internal\n    Cardano.Api.Value.Internal.Parser\n\nlibrary gen\n  import: project-config\n  visibility: public\n  hs-source-dirs: gen\n  exposed-modules:\n    Test.Gen.Cardano.Api\n    Test.Gen.Cardano.Api.Byron\n    Test.Gen.Cardano.Api.Era\n    Test.Gen.Cardano.Api.Hardcoded\n    Test.Gen.Cardano.Api.Metadata\n    Test.Gen.Cardano.Api.Orphans\n    Test.Gen.Cardano.Api.ProtocolParameters\n    Test.Gen.Cardano.Api.Typed\n    Test.Gen.Cardano.Crypto.Seed\n    Test.Hedgehog.Golden.ErrorMessage\n    Test.Hedgehog.Roundtrip.Bech32\n    Test.Hedgehog.Roundtrip.CBOR\n\n  build-depends:\n    FailT,\n    QuickCheck <2.16,\n    aeson >=1.5.6.0,\n    base16-bytestring,\n    bytestring,\n    cardano-api,\n    cardano-binary >=1.6 && <1.8,\n    cardano-crypto-class ^>=2.2.1,\n    cardano-crypto-test ^>=1.6,\n    cardano-crypto-wrapper,\n    cardano-ledger-alonzo >=1.8.1,\n    cardano-ledger-babbage,\n    cardano-ledger-byron,\n    cardano-ledger-conway,\n    cardano-ledger-core >=1.14,\n    cardano-ledger-dijkstra >=0.1,\n    cardano-ledger-mary,\n    cardano-ledger-shelley >=1.13,\n    cardano-strict-containers,\n    containers,\n    filepath,\n    generic-random,\n    hedgehog >=1.1,\n    hedgehog-extras,\n    hedgehog-quickcheck,\n    iproute,\n    quickcheck-instances,\n    random,\n    tasty,\n    tasty-hedgehog,\n    text,\n\ntest-suite cardano-api-test\n  import: project-config\n  hs-source-dirs: test/cardano-api-test\n  main-is: cardano-api-test.hs\n  type: exitcode-stdio-1.0\n\n  if arch(wasm32)\n    buildable: False\n  build-depends:\n    FailT,\n    QuickCheck <2.16,\n    aeson >=1.5.6.0,\n    base16-bytestring,\n    bytestring,\n    cardano-api,\n    cardano-api:gen,\n    cardano-binary,\n    cardano-crypto,\n    cardano-crypto-class ^>=2.2.1,\n    cardano-crypto-tests ^>=2.2,\n    cardano-crypto-wrapper,\n    cardano-ledger-alonzo,\n    cardano-ledger-api ^>=1.12.1,\n    cardano-ledger-binary,\n    cardano-ledger-conway,\n    cardano-ledger-core >=1.14,\n    cardano-ledger-mary,\n    cardano-ledger-shelley,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    containers,\n    data-default,\n    directory,\n    hedgehog >=1.1,\n    hedgehog-extras,\n    hedgehog-quickcheck,\n    microlens,\n    mtl,\n    ouroboros-consensus,\n    ouroboros-consensus-protocol,\n    raw-strings-qq,\n    tasty,\n    tasty-hedgehog,\n    tasty-quickcheck,\n    text,\n    time,\n\n  other-modules:\n    Test.Cardano.Api.Address\n    Test.Cardano.Api.Bech32\n    Test.Cardano.Api.CBOR\n    Test.Cardano.Api.Cip129\n    Test.Cardano.Api.Crypto\n    Test.Cardano.Api.Envelope\n    Test.Cardano.Api.EpochLeadership\n    Test.Cardano.Api.Eras\n    Test.Cardano.Api.Experimental\n    Test.Cardano.Api.Genesis\n    Test.Cardano.Api.GovAnchorValidation\n    Test.Cardano.Api.IO\n    Test.Cardano.Api.Json\n    Test.Cardano.Api.KeysByron\n    Test.Cardano.Api.Ledger\n    Test.Cardano.Api.Metadata\n    Test.Cardano.Api.Ord\n    Test.Cardano.Api.Orphans\n    Test.Cardano.Api.RawBytes\n    Test.Cardano.Api.Transaction.Autobalance\n    Test.Cardano.Api.Transaction.Body.Plutus.Scripts\n    Test.Cardano.Api.TxBody\n    Test.Cardano.Api.Value\n\n  ghc-options:\n    -threaded\n    -rtsopts\n    \"-with-rtsopts=-N -T\"\n\ntest-suite cardano-api-golden\n  import: project-config\n  hs-source-dirs: test/cardano-api-golden\n  main-is: cardano-api-golden.hs\n  type: exitcode-stdio-1.0\n\n  if arch(wasm32)\n    buildable: False\n  build-depends:\n    aeson,\n    base64-bytestring,\n    bech32 >=1.1.0,\n    bytestring,\n    cardano-api,\n    cardano-api:gen,\n    cardano-binary,\n    cardano-crypto-class ^>=2.2.1,\n    cardano-data >=1.0,\n    cardano-ledger-alonzo,\n    cardano-ledger-api ^>=1.12.1,\n    cardano-ledger-binary,\n    cardano-ledger-core >=1.14,\n    cardano-ledger-shelley,\n    cardano-protocol-tpraos,\n    containers,\n    errors,\n    filepath,\n    hedgehog >=1.1,\n    hedgehog-extras ^>=0.10,\n    microlens,\n    plutus-core ^>=1.53,\n    plutus-ledger-api ^>=1.53,\n    tasty,\n    tasty-discover,\n    tasty-hedgehog,\n    text,\n    time,\n\n  ghc-options:\n    -threaded\n    -rtsopts\n    \"-with-rtsopts=-N -T\"\n\n  build-tool-depends: tasty-discover:tasty-discover\n  other-modules:\n    Test.Golden.Cardano.Api.Genesis\n    Test.Golden.Cardano.Api.Ledger\n    Test.Golden.Cardano.Api.Script\n    Test.Golden.Cardano.Api.Value\n    Test.Golden.ErrorsSpec\n";
  }