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
    flags = { release = false; static = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "bech32"; version = "1.1.9"; };
      license = "Apache-2.0";
      copyright = "2017 Marko Bencun, 2019-2023 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com, mail@jonathanknowles.net";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Implementation of the Bech32 cryptocurrency address format (BIP 0173).";
      description = "Implementation of the Bech32 cryptocurrency address format documented in the\nBIP (Bitcoin Improvement Proposal) 0173.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      exes = {
        "bech32" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          ];
          buildable = true;
        };
      };
      tests = {
        "bech32-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.bech32.components.exes.bech32 or (pkgs.pkgsBuildBuild.bech32 or (errorHandler.buildToolDepError "bech32:bech32")))
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bech32-1.1.9.tar.gz";
      sha256 = "c40dd6d3703629ffc07fe39d26e689c178ca5b02043fc25aa7194bac02237050";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname:          bech32\nversion:       1.1.9\nsynopsis:      Implementation of the Bech32 cryptocurrency address format (BIP 0173).\ndescription:   Implementation of the Bech32 cryptocurrency address format documented in the\n               BIP (Bitcoin Improvement Proposal) 0173.\nauthor:        IOHK Engineering Team\nmaintainer:    operations@iohk.io, erikd@mega-nerd.com, mail@jonathanknowles.net\ncopyright:     2017 Marko Bencun, 2019-2023 IOHK\nlicense:       Apache-2.0\nlicense-file:  LICENSE\nhomepage:      https://github.com/input-output-hk/bech32\nbug-reports:   https://github.com/input-output-hk/bech32/issues\ncategory:      Web\nbuild-type:    Simple\n\nextra-doc-files:\n  ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/bech32.git\n\nflag release\n  description: Strict compiler warning checks.\n  default: False\n  manual: True\n\nflag static\n  description: Try to build a static executable.\n  default: False\n  manual: True\n\ncommon dependency-array\n    build-depends:array                           >= 0.5.4.0    && < 0.6\ncommon dependency-base\n    build-depends:base                            >= 4.14.3.0   && < 4.22\ncommon dependency-base58-bytestring\n    build-depends:base58-bytestring               >= 0.1.0      && < 0.2\ncommon dependency-bytestring\n    build-depends:bytestring                      >= 0.10.12.0  && < 0.13\ncommon dependency-containers\n    build-depends:containers                      >= 0.6.5.1    && < 0.9\ncommon dependency-deepseq\n    build-depends:deepseq                         >= 1.4.4.0    && < 1.6\ncommon dependency-extra\n    build-depends:extra                           >= 1.7.14     && < 1.9\ncommon dependency-hspec\n    build-depends:hspec                           >= 2.11.7     && < 2.12\ncommon dependency-memory\n    build-depends:memory                          >= 0.18.0     && < 0.19\ncommon dependency-optparse-applicative\n    build-depends:optparse-applicative            >= 0.18.1.0   && < 0.20\ncommon dependency-prettyprinter\n    build-depends:prettyprinter                   >= 1.7.1      && < 1.8\ncommon dependency-prettyprinter-ansi-terminal\n    build-depends:prettyprinter-ansi-terminal     >= 1.1.3      && < 1.2\ncommon dependency-process\n    build-depends:process                         >= 1.6.13.2   && < 1.7\ncommon dependency-QuickCheck\n    build-depends:QuickCheck                      >= 2.14.3     && < 2.16\ncommon dependency-text\n    build-depends:text                            >= 1.2.4.1    && < 2.2\ncommon dependency-vector\n    build-depends:vector                          >= 0.13.1.0   && < 0.14\n\nlibrary\n  import:\n    , dependency-array\n    , dependency-base\n    , dependency-bytestring\n    , dependency-containers\n    , dependency-extra\n    , dependency-text\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n  if flag(release)\n    ghc-options: -Werror\n  hs-source-dirs:\n      src\n  exposed-modules:\n      Codec.Binary.Bech32\n      Codec.Binary.Bech32.Internal\n\nexecutable bech32\n  import:\n    , dependency-base\n    , dependency-base58-bytestring\n    , dependency-bytestring\n    , dependency-extra\n    , dependency-memory\n    , dependency-optparse-applicative\n    , dependency-prettyprinter\n    , dependency-prettyprinter-ansi-terminal\n    , dependency-text\n  build-depends:\n    , bech32\n  main-is: Main.hs\n  other-modules:\n      Paths_bech32\n  autogen-modules:\n      Paths_bech32\n  hs-source-dirs:\n      app\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  if flag(static)\n    ghc-options: -static\n    cc-options: -static\n    ld-options: -static -pthread\n  default-language: Haskell2010\n\ntest-suite bech32-test\n  import:\n    , dependency-base\n    , dependency-base58-bytestring\n    , dependency-bytestring\n    , dependency-containers\n    , dependency-deepseq\n    , dependency-extra\n    , dependency-hspec\n    , dependency-memory\n    , dependency-process\n    , dependency-QuickCheck\n    , dependency-text\n    , dependency-vector\n  build-depends:\n    , bech32\n  build-tool-depends:\n    , bech32:bech32\n    , hspec-discover:hspec-discover\n  default-language:\n      Haskell2010\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      test\n  ghc-options:\n      -Wall\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  main-is:\n      Main.hs\n  other-modules:\n      AppSpec\n      Codec.Binary.Bech32Spec\n";
  }