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
    flags = { bytestring = true; text = true; vector = true; };
    package = {
      specVersion = "3.0";
      identifier = { name = "nothunks"; version = "0.2.1.1"; };
      license = "Apache-2.0";
      copyright = "2018-2024 Input Output Global Inc (IOG)";
      maintainer = "Marcin Szamotulski <coot@coot.me>";
      author = "IOG";
      homepage = "";
      url = "";
      synopsis = "Examine values for unexpected thunks";
      description = "Long lived application data typically should not contain\nany thunks. This library can be used to examine values for\nunexpected thunks, which can then be used in assertions.\nThis can be invaluable in avoiding memory leaks, or tracking\ndown existing ones.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.2") (hsPkgs."wherefrom-compat" or (errorHandler.buildDepError "wherefrom-compat"))) ++ pkgs.lib.optional (flags.bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ pkgs.lib.optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ pkgs.lib.optional (flags.vector) (hsPkgs."vector" or (errorHandler.buildDepError "vector"));
        buildable = true;
      };
      tests = {
        "nothunks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nothunks-0.2.1.1.tar.gz";
      sha256 = "cca7983daf5dea23fdf413c207c43b8e08e8b6cf067b386a94ee6561f8511cda";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\r\nname:               nothunks\r\nversion:            0.2.1.1\r\nx-revision: 2\r\nsynopsis:           Examine values for unexpected thunks\r\ndescription:        Long lived application data typically should not contain\r\n                    any thunks. This library can be used to examine values for\r\n                    unexpected thunks, which can then be used in assertions.\r\n                    This can be invaluable in avoiding memory leaks, or tracking\r\n                    down existing ones.\r\nlicense:            Apache-2.0\r\nlicense-files:      LICENSE\r\n                    NOTICE\r\nbug-reports:        https://github.com/input-output-hk/nothunks\r\nauthor:             IOG\r\nmaintainer:         Marcin Szamotulski <coot@coot.me>\r\ncopyright:          2018-2024 Input Output Global Inc (IOG)\r\ncategory:           Development\r\nextra-doc-files:    README.md CHANGELOG.md\r\ntested-with:        GHC == {8.10, 9.0, 9.2, 9.4, 9.6, 9.8, 9.10}\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/input-output-hk/nothunks\r\n\r\nflag bytestring\r\n  description: Provide instances for bytestring\r\n  default: True\r\n  manual: True\r\n\r\nflag text\r\n  description: Provide instances for text\r\n  default: True\r\n  manual: True\r\n\r\nflag vector\r\n  description: Provide instances for vector\r\n  default: True\r\n  manual: True\r\n\r\nlibrary\r\n    exposed-modules:  NoThunks.Class\r\n\r\n    build-depends:    base       >= 4.12 && < 5\r\n                    , containers >= 0.5  && < 0.8\r\n                    , stm        >= 2.5  && < 2.6\r\n                    , time       >= 1.5  && < 1.15\r\n\r\n                      -- Whatever is bundled with ghc\r\n                    , ghc-heap\r\n\r\n    if impl(ghc >= 9.2)\r\n      build-depends:  wherefrom-compat ^>= 0.1.1\r\n\r\n    if flag(bytestring)\r\n      build-depends:  bytestring >= 0.10 && < 0.13\r\n    if flag(text)\r\n      build-depends:  text       >= 1.2  && < 1.3 || >= 2 && < 2.2\r\n    if flag(vector)\r\n      build-depends:  vector     >= 0.12 && < 0.14\r\n\r\n    hs-source-dirs:   src\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n\r\ntest-suite nothunks-test\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    other-modules:    Test.NoThunks.Class\r\n\r\n    build-depends:    base\r\n\r\n                      -- Self dependency\r\n                    , nothunks\r\n\r\n                      -- Dependencies shared with the lib\r\n                    , containers\r\n                    , stm\r\n\r\n                      -- Whatever is bundled with ghc\r\n                    , ghc-prim\r\n\r\n                      -- Additional dependencies\r\n                    , hedgehog       >= 1.1 && < 1.5\r\n                    , random         >= 1.1 && < 1.3\r\n                    , tasty          >= 1.3 && < 1.6\r\n                    , tasty-hedgehog >= 1.1 && < 1.5\r\n\r\n    hs-source-dirs:   test\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n";
  }