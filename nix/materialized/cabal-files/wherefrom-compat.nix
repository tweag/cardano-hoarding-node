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
      identifier = { name = "wherefrom-compat"; version = "0.1.1.1"; };
      license = "BSD-2-Clause";
      copyright = "The wherefrom-compat contributors";
      maintainer = "teofilcamarasu@gmail.com";
      author = "Teo Camarasu";
      homepage = "";
      url = "";
      synopsis = "A compatibility layer for GHC's 'wherefrom' function";
      description = "A compatibility layer for GHC's 'wherefrom' function,\nwhich exposes info provenance information.\nEach major version of this library exports \na different version of this interface.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."wherefrom-compat" or (errorHandler.buildDepError "wherefrom-compat"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wherefrom-compat-0.1.1.1.tar.gz";
      sha256 = "c1e3e22e9730a683a7fbc450b5944d97dc34650af8a8c72000170d56ca9eb388";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\r\nname:            wherefrom-compat\r\nversion:         0.1.1.1\r\nx-revision: 1\r\nsynopsis:        A compatibility layer for GHC's 'wherefrom' function\r\ndescription:\r\n  A compatibility layer for GHC's 'wherefrom' function,\r\n  which exposes info provenance information.\r\n  Each major version of this library exports \r\n  a different version of this interface.\r\n\r\nlicense:         BSD-2-Clause\r\nlicense-file:    LICENSE\r\nauthor:          Teo Camarasu\r\nmaintainer:      teofilcamarasu@gmail.com\r\ncopyright:       The wherefrom-compat contributors\r\nbuild-type:      Simple\r\nextra-doc-files: CHANGELOG.md\r\nbug-reports:     https://codeberg.org/teo/wherefrom-compat/issues\r\ncategory:        Compatibility\r\ntested-with:     \r\n  GHC ==9.2.8 \r\n  ||  ==9.4.7 \r\n  ||  ==9.6.3 \r\n  ||  ==9.8.1\r\n  ||  ==9.10.1\r\n\r\ncommon warnings\r\n  ghc-options: -Wall\r\n\r\nlibrary\r\n  import:           warnings\r\n  exposed-modules:  GHC.InfoProv.Compat\r\n  build-depends:    base >=4.16.0.0 && <4.22.0.0\r\n  hs-source-dirs:   src\r\n  default-language: Haskell2010\r\n\r\ntest-suite test\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   test\r\n  ghc-options:      -finfo-table-map\r\n  main-is:          Spec.hs\r\n  build-depends:\r\n    , base\r\n    , tasty             ^>=1.5\r\n    , tasty-hunit       ^>=0.10\r\n    , wherefrom-compat\r\n\r\n  default-language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://codeberg.org/teo/wherefrom-compat.git\r\n";
  }