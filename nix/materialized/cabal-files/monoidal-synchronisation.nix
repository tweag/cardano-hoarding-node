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
      identifier = { name = "monoidal-synchronisation"; version = "0.1.0.6"; };
      license = "Apache-2.0";
      copyright = "2021-2023 Input Output Global Inc (IOG), 2023-2024 Intersect";
      maintainer = "coot@coot.me";
      author = "Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Monoidal synchronisation";
      description = "Monoidal synchronisation.";
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
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/monoidal-synchronisation-0.1.0.6.tar.gz";
      sha256 = "a7479ec4c934936c6a03d3fb15be105cde5bdd1ff9d8ae9032e258ff79c8f219";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                   monoidal-synchronisation\nversion:                0.1.0.6\nsynopsis:               Monoidal synchronisation\ndescription:            Monoidal synchronisation.\nlicense:                Apache-2.0\nlicense-files:          LICENSE\n                        NOTICE\nauthor:                 Marcin Szamotulski\nmaintainer:             coot@coot.me\ncategory:               Network\ncopyright:              2021-2023 Input Output Global Inc (IOG), 2023-2024 Intersect\nextra-doc-files:        CHANGELOG.md\n\nlibrary\n  exposed-modules:  Data.Monoid.Synchronisation\n  build-depends:    base >=4.14 && <4.22\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  ghc-options:     -Wall\n                   -Wcompat\n                   -Wincomplete-uni-patterns\n                   -Wincomplete-record-updates\n                   -Wpartial-fields\n                   -Widentities\n                   -Wredundant-constraints\n                   -Wno-unticked-promoted-constructors\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  main-is:             Main.hs\n  hs-source-dirs:      test\n  other-modules:       Test.Data.Monoid.Synchronisation\n  build-depends:       base >=4.14 && <4.22\n\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                     , io-classes\n                     , io-sim\n                     , monoidal-synchronisation\n  default-language: Haskell2010\n  ghc-options:       -rtsopts\n                     -threaded\n                     -Wall\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n                     -Wno-unticked-promoted-constructors\n";
  }