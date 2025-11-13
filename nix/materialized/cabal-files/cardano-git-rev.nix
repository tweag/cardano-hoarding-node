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
      identifier = { name = "cardano-git-rev"; version = "0.2.2.1"; };
      license = "Apache-2.0";
      copyright = "2022-2023 Input Output Global Inc (IOG).";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Git revisioning";
      description = "Embeds git revision into Haskell packages.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-git-rev-0.2.2.1.tar.gz";
      sha256 = "4943156d4352cf6804b69f0c4841f83fee522a05f89305cd1972135106ee9964";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-git-rev\nversion: 0.2.2.1\nsynopsis: Git revisioning\ndescription: Embeds git revision into Haskell packages.\ncategory:\n  Cardano,\n  Versioning,\n\ncopyright: 2022-2023 Input Output Global Inc (IOG).\nauthor: IOHK\nmaintainer: operations@iohk.io\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nbuild-type: Simple\nextra-source-files: README.md\n\ncommon project-config\n  default-language: Haskell2010\n  build-depends: base >=4.18 && <5\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: project-config\n  hs-source-dirs: src\n  c-sources: cbits/rev.c\n  exposed-modules: Cardano.Git.Rev\n  build-depends:\n    process,\n    template-haskell,\n    text,\n";
  }