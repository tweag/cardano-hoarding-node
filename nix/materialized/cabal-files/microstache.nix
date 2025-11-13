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
      specVersion = "2.2";
      identifier = { name = "microstache"; version = "1.0.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus<oleg.grenrus@iki.fi>";
      author = "Mark Karpov <markkarpov@openmailbox.org>, Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/haskellari/microstache";
      url = "";
      synopsis = "Mustache templates for Haskell";
      description = "Mustache templates for Haskell.\n\nBased on @stache@ library, which uses @megaparsec@.\nThis library uses @parsec@, thus the name: @microstache@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
        };
        "mustache-spec" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/microstache-1.0.3.tar.gz";
      sha256 = "35f290e57bd40fbaf7695d85efe34a2836441efbbb9deb696c7982b898aa898f";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               microstache\nversion:            1.0.3\nx-revision:         1\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nauthor:\n  Mark Karpov <markkarpov@openmailbox.org>, Oleg Grenrus <oleg.grenrus@iki.fi>\n\nmaintainer:         Oleg Grenrus<oleg.grenrus@iki.fi>\nhomepage:           https://github.com/haskellari/microstache\nbug-reports:        https://github.com/haskellari/microstache/issues\ncategory:           Text\nsynopsis:           Mustache templates for Haskell\nbuild-type:         Simple\ndescription:\n  Mustache templates for Haskell.\n  .\n  Based on @stache@ library, which uses @megaparsec@.\n  This library uses @parsec@, thus the name: @microstache@.\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n  specification/comments.json\n  specification/delimiters.json\n  specification/interpolation.json\n  specification/inverted.json\n  specification/partials.json\n  specification/sections.json\n\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/microstache.git\n\nlibrary\n  build-depends:\n      aeson                 >=2.2.2.0  && <2.3\n    , base                  >=4.12.0.0 && <4.22\n    , containers            >=0.6.0.1  && <0.8\n    , deepseq               >=1.4.4.0  && <1.6\n    , directory             >=1.3.3.0  && <1.4\n    , filepath              >=1.4.2.1  && <1.6\n    , parsec                >=3.1.13.0 && <3.2\n    , text                  >=1.2.3.0  && <1.3  || >=2.0 && <2.2\n    , transformers          >=0.5.6.2  && <0.7\n    , unordered-containers  >=0.2.20   && <0.3\n    , vector                >=0.13.1.0 && <0.14\n\n  exposed-modules:\n    Text.Microstache\n    Text.Microstache.Compile\n    Text.Microstache.Parser\n    Text.Microstache.Render\n    Text.Microstache.Type\n\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  default-language: Haskell2010\n\ntest-suite spec\n  main-is:          Spec.hs\n  hs-source-dirs:   tests tasty-as-hspec\n  type:             exitcode-stdio-1.0\n  build-depends:\n      aeson\n    , base\n    , containers\n    , microstache\n    , parsec\n    , text\n\n  -- tasty-as-hspec\n  build-depends:\n      base-orphans  >=0.8.7    && <0.10\n    , tasty         >=1.4.0.1  && <1.6\n    , tasty-hunit   >=0.10.0.3 && <0.11\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups\n\n  other-modules:\n    Test.Hspec\n    Text.Microstache.ParserSpec\n    Text.Microstache.RenderSpec\n    Text.Microstache.TypeSpec\n\n  default-language: Haskell2010\n\ntest-suite mustache-spec\n  main-is:          Spec.hs\n  hs-source-dirs:   mustache-spec tasty-as-hspec\n  type:             exitcode-stdio-1.0\n  build-depends:\n      aeson\n    , base\n    , bytestring\n    , containers\n    , microstache\n    , parsec\n    , text\n\n  -- tasty-as-hspec\n  build-depends:\n      base-orphans  >=0.8.7    && <0.10\n    , tasty         >=1.4.0.1  && <1.6\n    , tasty-hunit   >=0.10.0.3 && <0.11\n\n  other-modules:    Test.Hspec\n  default-language: Haskell2010\n";
  }