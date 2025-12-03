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
      identifier = { name = "cardano-addresses"; version = "4.0.1"; };
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
      url = "http://hackage.haskell.org/package/cardano-addresses-4.0.1.tar.gz";
      sha256 = "626725f49c230e34bbe849407e0492226285a9d9a9a8721a80c1bb5c36293569";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\n\nname:           cardano-addresses\nversion:        4.0.1\nsynopsis:       Utils for constructing a command-line on top of cardano-addresses.\ndescription:    Please see the README on GitHub at <https://github.com/IntersectMBO/cardano-addresses>\ncategory:       Cardano\nhomepage:       https://github.com/IntersectMBO/cardano-addresses#readme\nbug-reports:    https://github.com/IntersectMBO/cardano-addresses/issues\nauthor:         IOHK\nmaintainer:     hal@cardanofoundation.org\ncopyright:      2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect\nlicense:        Apache-2.0\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    ChangeLog.md\n    CONTRIBUTING.md\n    LICENSE\n    NOTICE\n    README.md\n    SECURITY.md\n    ./schemas/address-inspect.json\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/cardano-addresses\n\nflag release\n  description: Compile executables for a release.\n  manual: True\n  default: False\n\nlibrary\n  exposed-modules:\n      Cardano.Address\n      Cardano.Address.Derivation\n      Cardano.Address.Internal\n      Cardano.Address.KeyHash\n      Cardano.Address.Script\n      Cardano.Address.Script.Parser\n      Cardano.Address.Style.Byron\n      Cardano.Address.Style.Icarus\n      Cardano.Address.Style.Shared\n      Cardano.Address.Style.Shelley\n      Cardano.Codec.Bech32.Prefixes\n      Cardano.Codec.Cbor\n      Cardano.Dictionary\n      Cardano.Dictionary.English\n      Cardano.Dictionary.French\n      Cardano.Dictionary.Generic\n      Cardano.Dictionary.Italian\n      Cardano.Dictionary.Japanese\n      Cardano.Dictionary.Korean\n      Cardano.Dictionary.Spanish\n      Cardano.Mnemonic\n      Codec.Binary.Encoding\n      Command\n      Command.Address\n      Command.Address.Bootstrap\n      Command.Address.Delegation\n      Command.Address.Inspect\n      Command.Address.Payment\n      Command.Address.Pointer\n      Command.Address.Reward\n      Command.Key\n      Command.Key.Child\n      Command.Key.FromRecoveryPhrase\n      Command.Key.Hash\n      Command.Key.Inspect\n      Command.Key.Public\n      Command.Key.Private\n      Command.Key.WalletId\n      Command.RecoveryPhrase\n      Command.RecoveryPhrase.Generate\n      Command.Script\n      Command.Script.Hash\n      Command.Script.Preimage\n      Command.Script.Validation\n      Command.Version\n      Data.Word7\n      Options.Applicative.Credential\n      Options.Applicative.Derivation\n      Options.Applicative.Discrimination\n      Options.Applicative.Governance\n      Options.Applicative.MnemonicLanguage\n      Options.Applicative.MnemonicSize\n      Options.Applicative.Public\n      Options.Applicative.Script\n      Options.Applicative.Private\n      Options.Applicative.Style\n      System.Git.TH\n      System.IO.Extra\n  other-modules:\n      Paths_cardano_addresses\n  hs-source-dirs:\n      lib\n  default-extensions:\n      NoImplicitPrelude\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints\n  build-depends:\n      aeson >= 2.0\n    , aeson-pretty\n    , ansi-terminal\n    , ansi-wl-pprint\n    , base >= 4.7 && < 5\n    , base58-bytestring >= 0.1.0 && < 0.2\n    , basement\n    , bech32 >= 1.1.7 && < 1.2\n    , bech32-th >= 1.1.7 && < 1.2\n    , binary\n    , bytestring >= 0.10.6 && < 0.13\n    , cardano-crypto >= 1.2.0 && < 1.4.0\n    , cborg >= 0.2.1 && <0.3\n    , containers >= 0.5 && < 0.8\n    , crypton >= 0.32 && < 1.1\n    , deepseq >= 1.4.4.0 && < 1.6\n    , digest\n    , either\n    , exceptions\n    , extra >= 1.7.14 && < 1.9\n    , fmt >= 0.6.3 && < 0.7\n    , hashable\n    , memory  >= 0.18.0 && < 0.19\n    , mtl >= 2.2.2\n    , optparse-applicative >= 0.18.1.0 && < 0.20\n    , process >= 1.6.13.2 && < 1.7\n    , safe >= 0.3.19 && < 0.4\n    , template-haskell >= 2.16.0.0 && < 2.24\n    , text >= 1.2 && < 2.2\n    , transformers >= 0.5.6.2 && < 0.7\n    , unordered-containers\n  if flag(release)\n    ghc-options: -Werror\n  default-language: Haskell2010\n\nexecutable cardano-address\n  main-is: Main.hs\n  other-modules:\n      Paths_cardano_addresses\n  hs-source-dirs:\n      exe\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >= 4.7 && < 5\n    , cardano-addresses\n    , with-utf8 >= 1.1.0.0 && < 1.2\n  if flag(release) && !impl(ghcjs) && !os(ghcjs)\n    ghc-options: -Werror -static -O2\n    cc-options: -static\n    ld-options: -static -pthread\n  default-language: Haskell2010\n\ntest-suite unit\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n      AutoDiscover\n      Cardano.Address.DerivationSpec\n      Cardano.Address.Script.ParserSpec\n      Cardano.Address.ScriptSpec\n      Cardano.Address.Style.ByronSpec\n      Cardano.Address.Style.IcarusSpec\n      Cardano.Address.Style.SharedSpec\n      Cardano.Address.Style.ShelleySpec\n      Cardano.AddressSpec\n      Cardano.Codec.CborSpec\n      Cardano.MnemonicSpec\n      Codec.Binary.EncodingSpec\n      Command.Address.BootstrapSpec\n      Command.Address.DelegationSpec\n      Command.Address.InspectSpec\n      Command.Address.PaymentSpec\n      Command.Address.PointerSpec\n      Command.Address.RewardSpec\n      Command.Key.ChildSpec\n      Command.Key.FromRecoveryPhraseSpec\n      Command.Key.HashSpec\n      Command.Key.InspectSpec\n      Command.Key.PrivateSpec\n      Command.Key.PublicSpec\n      Command.Key.WalletIdSpec\n      Command.KeySpec\n      Command.RecoveryPhrase.GenerateSpec\n      Command.RecoveryPhraseSpec\n      Command.Script.HashSpec\n      Command.Script.PreimageSpec\n      Command.Script.ValidationSpec\n      CommandSpec\n      Data.Word7Spec\n      Options.Applicative.DerivationSpec\n      System.IO.ExtraSpec\n      Test.Arbitrary\n      Test.Utils\n      Paths_cardano_addresses\n  hs-source-dirs:\n      test\n  default-extensions:\n      NoImplicitPrelude\n  ghc-options: -Wall -Wcompat -fwarn-redundant-constraints -threaded -rtsopts -with-rtsopts=-N\n  build-tools:\n      cardano-address\n  build-tool-depends:\n      hspec-discover:hspec-discover\n  build-depends:\n      QuickCheck >= 2.14 && < 2.16\n    , aeson >= 2.0\n    , aeson-pretty\n    , base >= 4.7 && < 5\n    , bech32 >= 1.1.7 && < 1.2\n    , bech32-th >= 1.1.7 && < 1.2\n    , binary\n    , bytestring >= 0.10.6 && < 0.13\n    , cardano-addresses\n    , cardano-crypto\n    , crypton\n    , containers >= 0.5 && < 0.8\n    , hspec >= 2.11.0 && < 2.12\n    , hspec-golden >=0.1.0.3 && <0.2\n    , memory\n    , pretty-simple\n    , process\n    , string-interpolate\n    , temporary\n    , text >= 1.2 && < 2.2\n    , with-utf8 >= 1.1.0.0 && < 1.2\n  if os(windows)\n    build-depends:\n        Win32\n  if flag(release)\n    ghc-options: -Werror\n  default-language:   Haskell2010\n";
  }