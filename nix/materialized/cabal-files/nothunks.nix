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
    package-description-override = "cabal-version:      3.0\nname:               nothunks\nversion:            0.2.1.1\nsynopsis:           Examine values for unexpected thunks\ndescription:        Long lived application data typically should not contain\n                    any thunks. This library can be used to examine values for\n                    unexpected thunks, which can then be used in assertions.\n                    This can be invaluable in avoiding memory leaks, or tracking\n                    down existing ones.\nlicense:            Apache-2.0\nlicense-files:      LICENSE\n                    NOTICE\nbug-reports:        https://github.com/input-output-hk/nothunks\nauthor:             IOG\nmaintainer:         Marcin Szamotulski <coot@coot.me>\ncopyright:          2018-2024 Input Output Global Inc (IOG)\ncategory:           Development\nextra-doc-files:    README.md CHANGELOG.md\ntested-with:        GHC == {8.10, 9.0, 9.2, 9.4, 9.6, 9.8, 9.10}\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/nothunks\n\nflag bytestring\n  description: Provide instances for bytestring\n  default: True\n  manual: True\n\nflag text\n  description: Provide instances for text\n  default: True\n  manual: True\n\nflag vector\n  description: Provide instances for vector\n  default: True\n  manual: True\n\nlibrary\n    exposed-modules:  NoThunks.Class\n\n    build-depends:    base       >= 4.12 && < 5\n                    , containers >= 0.5  && < 0.8\n                    , stm        >= 2.5  && < 2.6\n                    , time       >= 1.5  && < 1.13\n\n                      -- Whatever is bundled with ghc\n                    , ghc-heap\n\n    if impl(ghc >= 9.2)\n      build-depends:  wherefrom-compat ^>= 0.1.1\n\n    if flag(bytestring)\n      build-depends:  bytestring >= 0.10 && < 0.13\n    if flag(text)\n      build-depends:  text       >= 1.2  && < 1.3 || >= 2 && < 2.2\n    if flag(vector)\n      build-depends:  vector     >= 0.12 && < 0.14\n\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    ghc-options:      -Wall\n\ntest-suite nothunks-test\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    other-modules:    Test.NoThunks.Class\n\n    build-depends:    base\n\n                      -- Self dependency\n                    , nothunks\n\n                      -- Dependencies shared with the lib\n                    , containers\n                    , stm\n\n                      -- Whatever is bundled with ghc\n                    , ghc-prim\n\n                      -- Additional dependencies\n                    , hedgehog       >= 1.1 && < 1.5\n                    , random         >= 1.1 && < 1.3\n                    , tasty          >= 1.3 && < 1.6\n                    , tasty-hedgehog >= 1.1 && < 1.5\n\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall\n";
  }