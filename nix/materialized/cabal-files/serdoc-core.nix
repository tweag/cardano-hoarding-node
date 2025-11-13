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
      identifier = { name = "serdoc-core"; version = "0.3.2.0"; };
      license = "Apache-2.0";
      copyright = "2023 IO Global";
      maintainer = "tobias@well-typed.com";
      author = "Tobias Dammers";
      homepage = "";
      url = "";
      synopsis = "Generated documentation of serialization formats";
      description = "A set of typeclasses, primitives, combinators, and TH\nutilities for documenting serialization formats in a mostly\nautomatic fashion.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
        ];
        buildable = true;
      };
      tests = {
        "serdoc-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."serdoc-core" or (errorHandler.buildDepError "serdoc-core"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/serdoc-core-0.3.2.0.tar.gz";
      sha256 = "a54fd2170a556b835ac1f056e2f683c617f9076e7dd0bf2a06930fb850e65810";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\nname:               serdoc-core\nversion:            0.3.2.0\nsynopsis:           Generated documentation of serialization formats\ndescription:        A set of typeclasses, primitives, combinators, and TH\n                    utilities for documenting serialization formats in a mostly\n                    automatic fashion.\nlicense:            Apache-2.0\nlicense-file:       LICENSE\nauthor:             Tobias Dammers\nmaintainer:         tobias@well-typed.com\ncopyright:          2023 IO Global\ncategory:           Data\nbuild-type:         Simple\nextra-doc-files:    README.md\n               ,    CHANGELOG.md\n-- extra-source-files:\n\ncommon warnings\n    ghc-options: -Wall\n\ncommon project-config\n    ghc-options:\n        -haddock\n        -- -ddump-splices\n\nlibrary\n    import: project-config\n    import: warnings\n    exposed-modules: Data.SerDoc.Class\n                   , Data.SerDoc.Info\n                   , Data.SerDoc.TestUtil\n                   , Data.SerDoc.TH\n    build-depends: base >=4.14.0.0 && <5\n                 , bytestring >=0.11 && <0.13\n                 , containers >=0.6 && <0.9\n                 , text >=1.1 && <2.2\n                 , template-haskell >=2.16 && <2.24\n                 , th-abstraction >=0.6 && <0.8\n    hs-source-dirs:   src\n    default-language: Haskell2010\n\ntest-suite serdoc-test\n    import: project-config\n    import: warnings\n    default-language: Haskell2010\n    other-modules: Data.SerDoc.Test.Class\n                 , Data.SerDoc.Test.Info\n    -- other-extensions:\n    type:             exitcode-stdio-1.0\n    hs-source-dirs:   test\n    main-is:          Main.hs\n    build-depends: base >=4.14.0.0 && <5\n                 , serdoc-core\n                 , bytestring >=0.11 && <0.13\n                 , mtl >=2.3.1 && <2.4\n                 , tasty >=1.5 && <1.6\n                 , tasty-quickcheck >=0.10.3 && <0.12\n";
  }