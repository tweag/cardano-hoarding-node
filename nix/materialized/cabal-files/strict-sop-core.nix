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
      identifier = { name = "strict-sop-core"; version = "0.1.3.0"; };
      license = "Apache-2.0";
      copyright = "2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Strict replacement for NS and NP.";
      description = "Strict replacement for NS and NP from @sop-core@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/strict-sop-core-0.1.3.0.tar.gz";
      sha256 = "d318c81dbf8e877d2ecb09a3bb69b6632d147cc9027d1906dbe96036d5f969eb";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: strict-sop-core\nsynopsis: Strict replacement for NS and NP.\ndescription: Strict replacement for NS and NP from @sop-core@.\nversion: 0.1.3.0\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2023 Input Output Global Inc (IOG), INTERSECT 2023-2024.\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncategory: Data\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\n\ncommon warnings\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import: warnings\n  hs-source-dirs: src\n  exposed-modules:\n    Data.SOP.Strict\n    Data.SOP.Strict.NP\n    Data.SOP.Strict.NS\n\n  build-depends:\n    base >=4.14 && <4.22,\n    nothunks ^>=0.2,\n    sop-core ^>=0.5,\n\n  default-language: Haskell2010\n";
  }