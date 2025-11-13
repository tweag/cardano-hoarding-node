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
      specVersion = "1.18";
      identifier = { name = "with-utf8"; version = "1.1.0.0"; };
      license = "MPL-2.0";
      copyright = "2020 Serokell";
      maintainer = "Kirill Elagin <kirelagin@serokell.io>";
      author = "Kirill Elagin <kirelagin@serokell.io>";
      homepage = "https://github.com/serokell/haskell-with-utf8#readme";
      url = "";
      synopsis = "Get your IO right on the first try";
      description = "This minimalistic library helps you navigate the world of text encodings\navoiding @invalid argument (invalid byte sequence)@\nand @invalid argument (invalid character)@ in runtime.\n\nSee <https://serokell.io/blog/haskell-with-utf8 this blog post> for why this\nlibrary exists and what exactly it does.\n\nThe two most important modules are:\n\n* \"Main.Utf8\"\n* \"System.IO.Utf8\"";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      exes = {
        "utf8-troubleshoot" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."th-env" or (errorHandler.buildDepError "th-env"))
          ];
          buildable = true;
        };
      };
      tests = {
        "with-utf8-test" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/with-utf8-1.1.0.0.tar.gz";
      sha256 = "a4b8d0f7c88c554c40e3c63371176fe5610db80c12756d3c57728e0a75bfe106";
    });
  }) // {
    package-description-override = "cabal-version: 1.18\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.37.0.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:           with-utf8\r\nversion:        1.1.0.0\r\nx-revision: 2\r\nsynopsis:       Get your IO right on the first try\r\ndescription:    This minimalistic library helps you navigate the world of text encodings\r\n                avoiding @invalid argument (invalid byte sequence)@\r\n                and @invalid argument (invalid character)@ in runtime.\r\n                .\r\n                See <https://serokell.io/blog/haskell-with-utf8 this blog post> for why this\r\n                library exists and what exactly it does.\r\n                .\r\n                The two most important modules are:\r\n                .\r\n                  * \"Main.Utf8\"\r\n                  * \"System.IO.Utf8\"\r\ncategory:       IO\r\nhomepage:       https://github.com/serokell/haskell-with-utf8#readme\r\nbug-reports:    https://github.com/serokell/haskell-with-utf8/issues\r\nauthor:         Kirill Elagin <kirelagin@serokell.io>\r\nmaintainer:     Kirill Elagin <kirelagin@serokell.io>\r\ncopyright:      2020 Serokell\r\nlicense:        MPL-2.0\r\nlicense-file:   LICENSES/MPL-2.0.txt\r\nbuild-type:     Simple\r\nextra-doc-files:\r\n    CHANGELOG.md\r\n    README.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/serokell/haskell-with-utf8\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Data.Text.IO.Utf8\r\n      Data.Text.Lazy.IO.Utf8\r\n      Main.Utf8\r\n      System.IO.Utf8\r\n      System.IO.Utf8.Internal\r\n  other-modules:\r\n      Paths_with_utf8\r\n  hs-source-dirs:\r\n      lib\r\n  ghc-options: -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints\r\n  build-depends:\r\n      base >=4.10 && <4.22\r\n    , safe-exceptions ==0.1.*\r\n    , text >=0.7 && <2.2\r\n  default-language: Haskell2010\r\n\r\nexecutable utf8-troubleshoot\r\n  main-is: Main.hs\r\n  other-modules:\r\n      Paths_with_utf8\r\n  hs-source-dirs:\r\n      app/utf8-troubleshoot\r\n  ghc-options: -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints\r\n  c-sources:\r\n      app/utf8-troubleshoot/cbits/locale.c\r\n  build-depends:\r\n      base >=4.10 && <4.22\r\n    , directory >=1.2.5.0 && <1.4\r\n    , filepath >=1.0 && <1.6\r\n    , process >=1.0.1.1 && <1.7\r\n    , safe-exceptions\r\n    , text >=0.7 && <2.2\r\n    , th-env >=0.1.0.0 && <0.2\r\n  default-language: Haskell2010\r\n\r\ntest-suite with-utf8-test\r\n  type: exitcode-stdio-1.0\r\n  main-is: Test.hs\r\n  other-modules:\r\n      Test.Utf8.Choice\r\n      Test.Utf8.ReadWrite\r\n      Test.Utf8.Set\r\n      Test.Util\r\n      Tree\r\n      Paths_with_utf8\r\n  hs-source-dirs:\r\n      test\r\n  ghc-options: -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints\r\n  build-tool-depends:\r\n      tasty-discover:tasty-discover\r\n  build-depends:\r\n      HUnit\r\n    , base >=4.10 && <4.22\r\n    , deepseq\r\n    , hedgehog\r\n    , safe-exceptions\r\n    , tasty\r\n    , tasty-hedgehog\r\n    , tasty-hunit\r\n    , temporary\r\n    , text >=0.7 && <2.2\r\n    , unix\r\n    , with-utf8\r\n  default-language: Haskell2010\r\n\r\n";
  }