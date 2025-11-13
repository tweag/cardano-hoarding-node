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
      specVersion = "1.10";
      identifier = {
        name = "prettyprinter-compat-ansi-wl-pprint";
        version = "1.0.2";
      };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "David Luposchainsky <dluposchainsky at google>";
      author = "David Luposchainsky";
      homepage = "http://github.com/quchen/prettyprinter";
      url = "";
      synopsis = "Drop-in compatibility package to migrate from »ansi-wl-pprint« to »prettyprinter«.";
      description = "See README.md";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
        ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/prettyprinter-compat-ansi-wl-pprint-1.0.2.tar.gz";
      sha256 = "05b27251f18b91efbf14c687c2851323b92032220337cd04ab1f741a84019e55";
    });
  }) // {
    package-description-override = "name:                prettyprinter-compat-ansi-wl-pprint\nversion:             1.0.2\ncabal-version:       >= 1.10\ncategory:            User Interfaces, Text\nsynopsis:            Drop-in compatibility package to migrate from »ansi-wl-pprint« to »prettyprinter«.\ndescription:         See README.md\nlicense:             BSD2\nlicense-file:        LICENSE.md\nextra-source-files:  README.md\nauthor:              David Luposchainsky\nmaintainer:          David Luposchainsky <dluposchainsky at google>\nbug-reports:         http://github.com/quchen/prettyprinter/issues\nhomepage:            http://github.com/quchen/prettyprinter\nbuild-type:          Simple\ntested-with:         GHC==9.0.1, GHC==8.10.4, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\n\nsource-repository head\n  type: git\n  location: git://github.com/quchen/prettyprinter.git\n\nlibrary\n    exposed-modules:  Text.PrettyPrint.ANSI.Leijen\n    ghc-options:      -Wall\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    other-extensions:\n          CPP\n        , OverloadedStrings\n\n    build-depends:\n          base                        >= 4.5 && < 5 && < 5\n        , text                        >= 1.2\n        , prettyprinter               >= 1.7.0\n        , prettyprinter-ansi-terminal >= 1.1\n\n    if !impl(ghc >= 8.0)\n        build-depends: semigroups >= 0.1\n";
  }