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
      identifier = { name = "ouroboros-network-mock"; version = "0.1.1.2"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect";
      maintainer = "marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Ouroboros Network Chain for testing purposes";
      description = "Ouroboros Network Chain for testing purposes.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-mock-0.1.1.2.tar.gz";
      sha256 = "c57a09e5a77b6725cb705ffe464e3a906133f47956ada4c057e23c5b2c7736bf";
    });
  }) // {
    package-description-override = "cabal-version:       3.0\n\nname:                   ouroboros-network-mock\nversion:                0.1.1.2\nsynopsis:               Ouroboros Network Chain for testing purposes\ndescription:            Ouroboros Network Chain for testing purposes.\nlicense:                Apache-2.0\nlicense-files:          LICENSE\n                        NOTICE\ncopyright:              2019-2023 Input Output Global Inc (IOG), 2023-2024 Intersect\nauthor:                 Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer:             marcin.szamotulski@iohk.io\ncategory:               Network\nbuild-type:             Simple\nextra-doc-files:        CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nsource-repository head\n  type:     git\n  location: https://github.com/intersectmbo/ouroboros-network\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:     Ouroboros.Network.Mock.ConcreteBlock\n                       Ouroboros.Network.Mock.Chain\n                       Ouroboros.Network.Mock.ProducerState\n\n  default-language:    Haskell2010\n  default-extensions:  ImportQualifiedPost\n\n  build-depends:       base              >=4.14 && <4.22,\n                       bytestring,\n                       containers,\n                       cborg,\n                       serialise,\n                       hashable,\n                       time,\n\n                       nothunks,\n\n                       ouroboros-network-api\n\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n                       -Wredundant-constraints\n                       -Wunused-packages\n  if flag(asserts)\n    ghc-options:       -fno-ignore-asserts\n";
  }