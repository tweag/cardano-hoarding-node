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
      identifier = { name = "sop-extras"; version = "0.4.1.0"; };
      license = "Apache-2.0";
      copyright = "2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Type-level and data utilities that build upon SOP.";
      description = "This package provides some more constructs that are not present on the\n@sop-core@ package but built upon the same foundations.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."strict-sop-core" or (errorHandler.buildDepError "strict-sop-core"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/sop-extras-0.4.1.0.tar.gz";
      sha256 = "613d122e387ed8fb56070e52e0642849021c6f05456e966f1d0f682ef751c0b8";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: sop-extras\nsynopsis: Type-level and data utilities that build upon SOP.\ndescription:\n  This package provides some more constructs that are not present on the\n  @sop-core@ package but built upon the same foundations.\n\nversion: 0.4.1.0\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Data\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\ncommon warnings\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import: warnings\n  hs-source-dirs: src\n  exposed-modules:\n    Data.SOP.Counting\n    Data.SOP.Functors\n    Data.SOP.InPairs\n    Data.SOP.Index\n    Data.SOP.Lenses\n    Data.SOP.Match\n    Data.SOP.NonEmpty\n    Data.SOP.OptNP\n    Data.SOP.Tails\n    Data.SOP.Telescope\n\n  build-depends:\n    base >=4.14 && <4.22,\n    constraints ^>=0.14,\n    nothunks ^>=0.2,\n    sop-core ^>=0.5,\n    strict-sop-core ^>=0.1,\n    these ^>=1.2,\n\n  default-language: Haskell2010\n";
  }