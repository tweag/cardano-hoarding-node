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
      identifier = { name = "ouroboros-network-mock"; version = "0.1.2.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect";
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
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-api" or (errorHandler.buildDepError "ouroboros-network-api"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/ouroboros-network-mock-0.1.2.0.tar.gz";
      sha256 = "55d8f450feb2225fc2872c68c5716775a8c784ecc25f7a3a5c64dd1de5927ef1";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: ouroboros-network-mock\nversion: 0.1.2.0\nsynopsis: Ouroboros Network Chain for testing purposes\ndescription: Ouroboros Network Chain for testing purposes.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG), 2023-2025 Intersect\nauthor: Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer: marcin.szamotulski@iohk.io\ncategory: Network\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\nflag asserts\n  description: Enable assertions\n  manual: False\n  default: False\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/ouroboros-network\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules:\n    Ouroboros.Network.Mock.Chain\n    Ouroboros.Network.Mock.ConcreteBlock\n    Ouroboros.Network.Mock.OrphanedInstances\n    Ouroboros.Network.Mock.ProducerState\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    base >=4.14 && <4.22,\n    bytestring,\n    cborg,\n    containers,\n    deepseq,\n    hashable,\n    nothunks,\n    ouroboros-network-api,\n    serialise,\n    time,\n    typed-protocols,\n\n  ghc-options:\n    -Wall\n    -Wno-unticked-promoted-constructors\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wunused-packages\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n";
  }