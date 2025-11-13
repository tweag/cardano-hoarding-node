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
    flags = { release = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "cardano-addresses"; version = "4.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect";
      maintainer = "hal@cardanofoundation.org";
      author = "IOHK";
      homepage = "https://github.com/IntersectMBO/cardano-addresses#readme";
      url = "";
      synopsis = "Utils for constructing a command-line on top of cardano-addresses.";
      description = "Please see the README on GitHub at <https://github.com/IntersectMBO/cardano-addresses>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
        ];
        buildable = true;
      };
      exes = {
        "cardano-address" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          buildable = true;
        };
      };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."string-interpolate" or (errorHandler.buildDepError "string-interpolate"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.pkgsBuildBuild.cardano-address.components.exes.cardano-address or (pkgs.pkgsBuildBuild.cardano-address or (errorHandler.buildToolDepError "cardano-address:cardano-address")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cardano-addresses-4.0.0.tar.gz";
      sha256 = "8487dca5bca38c261c13c42928557cd5c0ea48bb289e370a4b7278a8eb579b8d";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\nname:           cardano-addresses\r\nversion:        4.0.0\r\nx-revision: 1\r\nsynopsis:       Utils for constructing a command-line on top of cardano-addresses.\r\ndescription:    Please see the README on GitHub at <https://github.com/IntersectMBO/cardano-addresses>\r\ncategory:       Cardano\r\nhomepage:       https://github.com/IntersectMBO/cardano-addresses#readme\r\nbug-reports:    https://github.com/IntersectMBO/cardano-addresses/issues\r\nauthor:         IOHK\r\nmaintainer:     hal@cardanofoundation.org\r\ncopyright:      2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect\r\nlicense:        Apache-2.0\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    ChangeLog.md\r\n    CONTRIBUTING.md\r\n    LICENSE\r\n    NOTICE\r\n    README.md\r\n    SECURITY.md\r\n    ./schemas/address-inspect.json\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/IntersectMBO/cardano-addresses\r\n\r\nflag release\r\n  description: Compile executables for a release.\r\n  manual: True\r\n  default: False\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Cardano.Address\r\n      Cardano.Address.Derivation\r\n      Cardano.Address.Internal\r\n      Cardano.Address.KeyHash\r\n      Cardano.Address.Script\r\n      Cardano.Address.Script.Parser\r\n      Cardano.Address.Style.Byron\r\n      Cardano.Address.Style.Icarus\r\n      Cardano.Address.Style.Shared\r\n      Cardano.Address.Style.Shelley\r\n      Cardano.Codec.Bech32.Prefixes\r\n      Cardano.Codec.Cbor\r\n      Cardano.Mnemonic\r\n      Codec.Binary.Encoding\r\n      Command\r\n      Command.Address\r\n      Command.Address.Bootstrap\r\n      Command.Address.Delegation\r\n      Command.Address.Inspect\r\n      Command.Address.Payment\r\n      Command.Address.Pointer\r\n      Command.Address.Reward\r\n      Command.Key\r\n      Command.Key.Child\r\n      Command.Key.FromRecoveryPhrase\r\n      Command.Key.Hash\r\n      Command.Key.Inspect\r\n      Command.Key.Public\r\n      Command.Key.Private\r\n      Command.Key.WalletId\r\n      Command.RecoveryPhrase\r\n      Command.RecoveryPhrase.Generate\r\n      Command.Script\r\n      Command.Script.Hash\r\n      Command.Script.Preimage\r\n      Command.Script.Validation\r\n      Command.Version\r\n      Data.Word7\r\n      Options.Applicative.Credential\r\n      Options.Applicative.Derivation\r\n      Options.Applicative.Discrimination\r\n      Options.Applicative.Governance\r\n      Options.Applicative.MnemonicSize\r\n      Options.Applicative.Public\r\n      Options.Applicative.Script\r\n      Options.Applicative.Private\r\n      Options.Applicative.Style\r\n      System.Git.TH\r\n      System.IO.Extra\r\n  other-modules:\r\n      Paths_cardano_addresses\r\n  hs-source-dirs:\r\n      lib\r\n  default-extensions:\r\n      NoImplicitPrelude\r\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints\r\n  build-depends:\r\n      aeson >= 2.0\r\n    , aeson-pretty\r\n    , ansi-terminal\r\n    , ansi-wl-pprint\r\n    , base >= 4.7 && < 5\r\n    , base58-bytestring >= 0.1.0 && < 0.2\r\n    , basement\r\n    , bech32 >= 1.1.7 && < 1.2\r\n    , bech32-th >= 1.1.7 && < 1.2\r\n    , binary\r\n    , bytestring >= 0.10.6 && < 0.13\r\n    , cardano-crypto >= 1.1.2 && < 1.4.0\r\n    , cborg >= 0.2.1 && <0.3\r\n    , containers >= 0.5 && < 0.8\r\n    , crypton >= 0.32 && < 1.1\r\n    , deepseq >= 1.4.4.0 && < 1.6\r\n    , digest\r\n    , either\r\n    , exceptions\r\n    , extra >= 1.7.14 && < 1.9\r\n    , fmt >= 0.6.3 && < 0.7\r\n    , hashable\r\n    , memory  >= 0.18.0 && < 0.19\r\n    , mtl >= 2.2.2\r\n    , optparse-applicative >= 0.18.1.0 && < 0.19\r\n    , process >= 1.6.13.2 && < 1.7\r\n    , safe >= 0.3.19 && < 0.4\r\n    , template-haskell >= 2.16.0.0 && < 2.24\r\n    , text >= 1.2 && < 2.2\r\n    , transformers >= 0.5.6.2 && < 0.7\r\n    , unordered-containers\r\n  if flag(release)\r\n    ghc-options: -Werror\r\n  default-language: Haskell2010\r\n\r\nexecutable cardano-address\r\n  main-is: Main.hs\r\n  other-modules:\r\n      Paths_cardano_addresses\r\n  hs-source-dirs:\r\n      exe\r\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints -threaded -rtsopts -with-rtsopts=-N\r\n  build-depends:\r\n      base >= 4.7 && < 5\r\n    , cardano-addresses\r\n    , with-utf8 >= 1.1.0.0 && < 1.2\r\n  if flag(release) && !impl(ghcjs) && !os(ghcjs)\r\n    ghc-options: -Werror -static -O2\r\n    cc-options: -static\r\n    ld-options: -static -pthread\r\n  default-language: Haskell2010\r\n\r\ntest-suite unit\r\n  type: exitcode-stdio-1.0\r\n  main-is: Main.hs\r\n  other-modules:\r\n      AutoDiscover\r\n      Cardano.Address.DerivationSpec\r\n      Cardano.Address.Script.ParserSpec\r\n      Cardano.Address.ScriptSpec\r\n      Cardano.Address.Style.ByronSpec\r\n      Cardano.Address.Style.IcarusSpec\r\n      Cardano.Address.Style.SharedSpec\r\n      Cardano.Address.Style.ShelleySpec\r\n      Cardano.AddressSpec\r\n      Cardano.Codec.CborSpec\r\n      Cardano.MnemonicSpec\r\n      Codec.Binary.EncodingSpec\r\n      Command.Address.BootstrapSpec\r\n      Command.Address.DelegationSpec\r\n      Command.Address.InspectSpec\r\n      Command.Address.PaymentSpec\r\n      Command.Address.PointerSpec\r\n      Command.Address.RewardSpec\r\n      Command.Key.ChildSpec\r\n      Command.Key.FromRecoveryPhraseSpec\r\n      Command.Key.HashSpec\r\n      Command.Key.InspectSpec\r\n      Command.Key.PrivateSpec\r\n      Command.Key.PublicSpec\r\n      Command.Key.WalletIdSpec\r\n      Command.KeySpec\r\n      Command.RecoveryPhrase.GenerateSpec\r\n      Command.RecoveryPhraseSpec\r\n      Command.Script.HashSpec\r\n      Command.Script.PreimageSpec\r\n      Command.Script.ValidationSpec\r\n      CommandSpec\r\n      Data.Word7Spec\r\n      Options.Applicative.DerivationSpec\r\n      System.IO.ExtraSpec\r\n      Test.Arbitrary\r\n      Test.Utils\r\n      Paths_cardano_addresses\r\n  hs-source-dirs:\r\n      test\r\n  default-extensions:\r\n      NoImplicitPrelude\r\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints -threaded -rtsopts -with-rtsopts=-N\r\n  build-tools:\r\n      cardano-address\r\n  build-tool-depends:\r\n      hspec-discover:hspec-discover\r\n  build-depends:\r\n      QuickCheck >= 2.14 && < 2.16\r\n    , aeson >= 2.0\r\n    , aeson-pretty\r\n    , base >= 4.7 && < 5\r\n    , bech32 >= 1.1.7 && < 1.2\r\n    , bech32-th >= 1.1.7 && < 1.2\r\n    , binary\r\n    , bytestring >= 0.10.6 && < 0.13\r\n    , cardano-addresses\r\n    , cardano-crypto\r\n    , crypton\r\n    , containers >= 0.5 && < 0.8\r\n    , hspec >= 2.11.0 && < 2.12\r\n    , hspec-golden >=0.1.0.3 && <0.2\r\n    , memory\r\n    , pretty-simple\r\n    , process\r\n    , string-interpolate\r\n    , temporary\r\n    , text >= 1.2 && < 2.2\r\n    , with-utf8 >= 1.1.0.0 && < 1.2\r\n  if os(windows)\r\n    build-depends:\r\n        Win32\r\n  if flag(release)\r\n    ghc-options: -Werror\r\n  default-language:   Haskell2010\r\n";
  }