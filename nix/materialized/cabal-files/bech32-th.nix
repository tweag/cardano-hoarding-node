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
      specVersion = "3.0";
      identifier = { name = "bech32-th"; version = "1.1.9"; };
      license = "Apache-2.0";
      copyright = "2020-2023 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com, mail@jonathanknowles.net";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Template Haskell extensions to the Bech32 library.";
      description = "Template Haskell extensions to the Bech32 library, including\nquasi-quoters for compile-time checking of Bech32 string\nliterals.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "bech32-th-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bech32-th-1.1.9.tar.gz";
      sha256 = "9a01c3b47b229ee7a0c371a11d6d540799622a7283204d7c58f4846b4be7832d";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\nname:               bech32-th\nversion:            1.1.9\nsynopsis:           Template Haskell extensions to the Bech32 library.\ndescription:        Template Haskell extensions to the Bech32 library, including\n                    quasi-quoters for compile-time checking of Bech32 string\n                    literals.\nauthor:             IOHK Engineering Team\nmaintainer:         operations@iohk.io, erikd@mega-nerd.com, mail@jonathanknowles.net\ncopyright:          2020-2023 IOHK\nlicense:            Apache-2.0\nlicense-file:       LICENSE\nhomepage:           https://github.com/input-output-hk/bech32\nbug-reports:        https://github.com/input-output-hk/bech32/issues\ncategory:           Web\nbuild-type:         Simple\n\nextra-doc-files:\n  ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/bech32.git\n\nflag release\n  description: Strict compiler warning checks.\n  default: False\n  manual: True\n\ncommon dependency-base\n    build-depends:base                            >= 4.14.3.0   && < 4.22\ncommon dependency-bech32\n    build-depends:bech32                          >= 1.1.7      && < 1.2\ncommon dependency-hspec\n    build-depends:hspec                           >= 2.11.7     && < 2.12\ncommon dependency-template-haskell\n    build-depends:template-haskell                >= 2.16.0.0   && < 2.24\ncommon dependency-text\n    build-depends:text                            >= 1.2.4.1    && < 2.2\n\nlibrary\n  import:\n    , dependency-base\n    , dependency-bech32\n    , dependency-template-haskell\n    , dependency-text\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n  if flag(release)\n    ghc-options: -Werror\n  hs-source-dirs:\n      src\n  exposed-modules:\n      Codec.Binary.Bech32.TH\n\ntest-suite bech32-th-test\n  import:\n    , dependency-base\n    , dependency-bech32\n    , dependency-hspec\n    , dependency-template-haskell\n  build-depends:\n    , bech32-th\n  build-tool-depends:\n    , hspec-discover:hspec-discover\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      test\n  ghc-options:\n      -Wall\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  main-is:\n      Main.hs\n  other-modules:\n      Codec.Binary.Bech32.THSpec\n";
  }